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

splitBranch :: Stmt -> Int -> [ProgramPath]
splitBranch s@(Seq s1 s2) n          = [ x ++ y  | x <-  splitBranch s1 n, y <- splitBranch s2 n ]
splitBranch s@(IfThenElse g s1 s2) n =
        (map (putInFront (Assume g))         (splitBranch s1 n)) ++
        (map (putInFront (Assume (OpNeg g))) (splitBranch s2 n))
      where
        putInFront x y = x : y
splitBranch s@(While exp stmt) n        = postFixLoops (concat (map (\xs-> repeatLoop ((Assume exp) : xs)) (splitBranch stmt n)))
--splitBranch s@(While exp stmt) n        = (map (\xs-> (Assume exp) : xs ++ [Assume (OpNeg exp)]) (splitBranch stmt n))
    where
        repeatLoop xs = map (\loopLength -> concat (take loopLength (repeat xs))) [0..n]
        postFixLoops xss = map (\xs -> xs ++ [Assume (OpNeg exp)]) xss
--removed block cases since we already remove all instance of block before this function.
splitBranch s n = [[s]]

-- TODO trycatch

-- assuming this is: program path -> post condition
generateWlp :: Stmt -> Expr -> Expr
generateWlp (Skip) post                = post
generateWlp (Assume expr1) post        = BinopExpr Implication expr1 post
generateWlp (Assign name expr) post    = replaceVar name expr post
generateWlp (AAssign name expr newval) post = replaceVar name (RepBy (Var name) expr newval) post
generateWlp (Assert expr1) post        = BinopExpr And expr1 post
-- Why is there an IfThenElse here? Do we not remove this in splitBranch?
generateWlp (IfThenElse g s1 s2) post  = BinopExpr And ifSide elseSide
    where
        ifSide   = BinopExpr Implication g (generateWlp s1 post)
        elseSide = BinopExpr Implication (OpNeg g)  (generateWlp s2 post)


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

    putStrLn "amount of paths for and after while"
    putStrLn $ show (length xs)
    (bodyDepth, infeasible2, time2, bodyResult)      <- scanWhile
    putStrLn $ show (length (concat bodyResult))
    --printList (concat bodyResult)
    ((infeasible3, bodyPaths), time3 )    <- stopWatch( filterValidPaths (OpNeg exp) heuristics varDecls ifDepth (concat bodyResult))
    putStrLn $ show (length bodyPaths)
    --printList bodyPaths
    return (bodyDepth -1 ,infeasible1 + infeasible2 + infeasible3, time1 + time2 + time3,  emptyLoop ++ (addStmtToPaths (Assume (OpNeg exp)) bodyPaths))
      where
        scanWhile           = foldM (scanfn varDecls heuristics stmt exp n)  (ifDepth, 0, TimeSpec 0 0, [xs]) [1..n]
        --scanfn (depth, acc) _        = do
        --    -- filter the paths on is feasible and continue loop on feasible paths
        --    paths <- filterValidPaths exp varDecls depth (head acc)
        --    (newDepth, res) <- analyseTree varDecls (preFixLoops (Assume exp) paths) stmt n depth
        --    return (newDepth -1, (res : acc))

--(preFixLoops (Assume (OpNeg exp)) xs) ++ (preFixLoops (Assume (OpNeg exp)) (concat scanWhile))
--analyseTree varDecls [] s n = return [[s]]
analyseTree varDecls xs s n ifDepth heuristics = return (ifDepth ,0, TimeSpec 0 0, addStmtToPaths s xs)

scanfn :: [VarDeclaration] -> Bool-> Stmt -> Expr-> Int ->(Int, Int, TimeSpec, [[(Int, ProgramPath)]]) -> Int -> IO (Int, Int, TimeSpec, [[(Int, ProgramPath)]])
scanfn varDecls heuristics stmt guard n (depth, infeasible1, time1, acc) _        = do
    -- filter the paths on is feasible and continue loop on feasible paths
    ((infeasible2, paths),time2 ) <- stopWatch ( filterValidPaths guard heuristics varDecls depth (head acc))
    (newDepth,infeasible3, time3 , res) <- analyseTree varDecls (addStmtToPaths (Assume guard) paths) stmt n depth heuristics
    return (newDepth -1, infeasible1 + infeasible2 + infeasible3, time1 + time2 + time3, (res : acc))


filterValidPaths :: Expr -> Bool -> [VarDeclaration] -> Int -> [(Int, ProgramPath)] -> IO (Int, [(Int, ProgramPath)])
filterValidPaths g heuristics varDecls ifDepth paths = do
    if ifDepth > 0 && heuristics then do
         validated <- filterM (isBranchValid varDecls g) paths
         return (length paths - length validated , validated)
    else
        return (0, paths)

isBranchValid :: [VarDeclaration] -> Expr -> (Int, ProgramPath) -> IO Bool
isBranchValid varDecls g (i, path) = do
    test1 <-isGuardSat finalWlp varDecls
    test2 <-isSat test1
    --putStrLn $ show test2
    --putStrLn $ show finalWlp
    return test2
      where
          finalWlp = foldl (flip generateWlpGuard) g path
          isSat (Sat) = return True
          isSat _ = return False

addStmtToPath :: Stmt -> (Int, ProgramPath) -> (Int, ProgramPath)
addStmtToPath stmt (i,xs) = (i -1, stmt : xs)

addStmtToPaths :: Stmt -> [(Int, ProgramPath)] -> [(Int, ProgramPath)]
addStmtToPaths stmt xs = map (addStmtToPath stmt) (filter (\(i,ls) -> i /= 0) xs)

generateWlpGuard :: Stmt -> Expr -> Expr
generateWlpGuard (Skip) post                = post
generateWlpGuard (Assume expr1) post        = BinopExpr And expr1 post
generateWlpGuard (Assign name expr) post    = replaceVar name expr post
generateWlpGuard (AAssign name expr newval) post = replaceVar name (RepBy (Var name) expr newval) post
-- TODO: This does not hold for the first assert, should make an exception for that. Maybe type match on []?
generateWlpGuard (Assert expr1) post        = BinopExpr And expr1 post
-- Is sequence needed? Seems like we already deal with this in splitBranch. Maybe we can use sequence instead of a list?
-- generateWlpGuard (Seq s1 s2) a@(Just expr2) = generateWlpGuard s1 (generateWlpGuard s2 a)
-- generateWlpGuard (Block [decl:] stmt) expr = replaceVar generatename
generateWlpGuard (IfThenElse g s1 s2) post  = BinopExpr And ifSide elseSide
    where
        ifSide   = BinopExpr Implication g (generateWlpGuard s1 post)
        elseSide = BinopExpr Implication (OpNeg g)  (generateWlpGuard s2 post)

printList :: (Show a) => [a] -> IO ()
printList [] = return ()
printList (x:xs) = do
    putStrLn $ show x
    printList xs