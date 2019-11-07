module BranchSplit where

import GCLParser.GCLDatatype
import Datatypes
import PreProcessing
import Common
import Control.Monad
import System.Clock
import Z3Converter
import Control.StopWatch
import Z3.Monad

----------------------------------------------------------------------------------------------------------------
--                            Main function
----------------------------------------------------------------------------------------------------------------
-- this function splits a program tree in different program paths
analyseTree :: [VarDeclaration] -> [(Int, ProgramPath)] -> Stmt -> Int -> Int -> Bool -> IO (Int, Int, TimeSpec, [(Int, ProgramPath)])
analyseTree varDecls xs s@(Seq s1 s2) n ifDepth heuristics = do
    (ifDepth1, infeasible1, time1, leftResult)  <- analyseTree varDecls xs s1 n ifDepth heuristics
    (ifDepth2, infeasible2, time2, finalResult) <- analyseTree varDecls leftResult s2 n ifDepth1 heuristics
    return (ifDepth2, infeasible1 + infeasible2, time1 + time2, finalResult)

analyseTree varDecls xs s@(IfThenElse g s1 s2) n ifDepth heuristics = do
    ((infeasible1, validIfBranches),   time1) <- stopWatch (filterValidPaths g         heuristics varDecls ifDepth xs)
    ((infeasible2, validElseBranches), time2) <- stopWatch (filterValidPaths (OpNeg g) heuristics varDecls ifDepth xs)

    (depth1,infeasible1, time1,ifStmt)        <- analyseTree varDecls (addStmtToPaths (Assume g)         validIfBranches)   s1 n (ifDepth - 1) heuristics
    (depth2,infeasible2, time2, elseStmt)     <- analyseTree varDecls (addStmtToPaths (Assume (OpNeg g)) validElseBranches) s2 n (ifDepth - 1) heuristics
    let finalDepth = (depth1 + depth2) `quot` 2 -1

    return $ (finalDepth, infeasible1 + infeasible2, time1 + time2, ifStmt ++ elseStmt)

analyseTree varDecls xs s@(While exp stmt) n ifDepth heuristics = do
    ((infeasible1, emptyLoopPath), time1) <- stopWatch (filterValidPaths (OpNeg exp) heuristics varDecls ifDepth xs)

    let emptyLoop   = (addStmtToPaths (Assume (OpNeg exp)) emptyLoopPath)

    (bodyDepth, infeasible2, time2, bodyResult)      <- recursiveWhile varDecls (heuristics, n) stmt exp n (ifDepth, 0, TimeSpec 0 0, []) xs

    return (bodyDepth -1 ,infeasible1 + infeasible2, time1 + time2,  emptyLoop ++ (addStmtToPaths (Assume (OpNeg exp)) (concat bodyResult)))

analyseTree varDecls xs s n ifDepth heuristics = return (ifDepth ,0, TimeSpec 0 0, addStmtToPaths s xs)


-- handle the recursive while statements
recursiveWhile :: [VarDeclaration] -> (Bool, Int) -> Stmt -> Expr-> Int -> (Int, Int, TimeSpec, [[(Int, ProgramPath)]]) -> [(Int, ProgramPath)] -> IO (Int, Int, TimeSpec, [[(Int, ProgramPath)]])
recursiveWhile varDecls (True,0)        body guard n newTuple                               bodyTillNow = do
    return (newTuple)
recursiveWhile varDecls (heuristics, i) body guard n t [] = do
    return t
recursiveWhile varDecls (heuristics, i) body guard n t@(depth, infeasible1, time1, acc)     bodyTillNow = do
    -- check feasiblity paths
    ((infeasible2, paths2),time2 ) <- stopWatch ( filterValidPaths guard heuristics varDecls depth bodyTillNow)
    -- create body paths
    (newDepth,infeasible3, time3 , pathAfterWhile) <- analyseTree varDecls (addStmtToPaths (Assume guard) paths2) body n (depth-1) heuristics

    ((infeasible4, paths4),time4 ) <- stopWatch ( filterValidPaths (OpNeg guard) heuristics varDecls newDepth pathAfterWhile)

    let paths5 = addStmtToPaths (Assume (OpNeg guard)) paths4

    let newTuple = (newDepth, infeasible1 + infeasible2 + infeasible3 + infeasible4, time1 + time2 + time3 + time4, (paths5 : acc))
    recursiveWhile varDecls (heuristics, i-1) body guard n newTuple pathAfterWhile

filterValidPaths :: Expr -> Bool -> [VarDeclaration] -> Int -> [(Int, ProgramPath)] -> IO (Int, [(Int, ProgramPath)])
filterValidPaths g heuristics varDecls ifDepth paths = do
    if ifDepth > 0 && heuristics then do
         validated <- filterM (isBranchValid varDecls g) paths
         return (length paths - length validated , validated)
    else
        return (0, paths)

isBranchValid :: [VarDeclaration] -> Expr -> (Int, ProgramPath) -> IO Bool
isBranchValid varDecls g (i, path) = do
    let normalizedWlp = normalizeQuantifiers finalWlp
    test1 <-isGuardSat normalizedWlp varDecls
    test2 <-isSat test1
    return test2
      where
          finalWlp = foldl (flip generateWlpGuard) g path
          isSat (Sat) = return True
          isSat _ = return False

addStmtToPath :: Stmt -> (Int, ProgramPath) -> (Int, ProgramPath)
addStmtToPath stmt (i,xs) = (i -1, stmt : xs)

addStmtToPaths :: Stmt -> [(Int, ProgramPath)] -> [(Int, ProgramPath)]
addStmtToPaths stmt xs = map (addStmtToPath stmt) (filter (\(i,ls) -> i /= 0) xs)


---------------------------------------------------------------------------------------------------------------
--                         Generate wlp functions
---------------------------------------------------------------------------------------------------------------

generateWlp :: Stmt -> Expr -> Expr
generateWlp (Skip) post                = post
generateWlp (Assume expr1) post        = BinopExpr Implication expr1 post
generateWlp (Assign name expr) post    = replaceVar name expr post
generateWlp (AAssign name expr newval) post = replaceVar name (RepBy (Var name) expr newval) post
generateWlp (Assert expr1) post        = BinopExpr And expr1 post
generateWlp (IfThenElse g s1 s2) post  = BinopExpr And ifSide elseSide
    where
        ifSide   = BinopExpr Implication g (generateWlp s1 post)
        elseSide = BinopExpr Implication (OpNeg g)  (generateWlp s2 post)

-- This function has a different statement for Assume
generateWlpGuard :: Stmt -> Expr -> Expr
generateWlpGuard (Skip) post                = post
generateWlpGuard (Assume expr1) post        = BinopExpr And expr1 post
generateWlpGuard (Assign name expr) post    = replaceVar name expr post
generateWlpGuard (AAssign name expr newval) post = replaceVar name (RepBy (Var name) expr newval) post
generateWlpGuard (Assert expr1) post        = BinopExpr And expr1 post
generateWlpGuard (IfThenElse g s1 s2) post  = BinopExpr And ifSide elseSide
    where
        ifSide   = BinopExpr Implication g (generateWlpGuard s1 post)
        elseSide = BinopExpr Implication (OpNeg g)  (generateWlpGuard s2 post)
