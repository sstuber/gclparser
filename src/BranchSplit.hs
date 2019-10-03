module BranchSplit where

import GCLParser.GCLDatatype
import Datatypes
import PreProcessing
import Common
import Control.Monad
import Z3Converter
import Z3.Monad

-- TODO add means to process arrays
-- TODO get programs in the right structure to check validity


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


analyseTree :: [VarDeclaration] -> [ProgramPath] -> Stmt -> Int -> Int-> IO (Int, [ProgramPath])
analyseTree varDecls xs s@(Seq s1 s2) n ifDepth = do
    (ifDepth1, leftResult) <- analyseTree varDecls xs s1 n ifDepth
    analyseTree varDecls leftResult s2 n ifDepth1

analyseTree varDecls xs s@(IfThenElse g s1 s2) n ifDepth = do

    validIfBranches <- filterValidPaths g varDecls ifDepth xs  -- filterM (isBranchValid varDecls g) xs
    --putStrLn $ show validIfBranches
    (depth1,ifStmt)    <- analyseTree varDecls (map ((:) (Assume g))  validIfBranches) s1 n (ifDepth-1)
    --putStrLn $ show ifStmt

    validElseBranches <- filterM (isBranchValid varDecls  (OpNeg g)) xs
    (depth2, elseStmt)  <- analyseTree varDecls (map ((:) (Assume (OpNeg g))) validElseBranches) s2 n (ifDepth -1)
    return $ ( (depth1 + depth2) `quot` 2 -1 , ifStmt ++ elseStmt)
analyseTree varDecls xs s@(While exp stmt) n ifDepth = do
    emptyLoopPath <- filterValidPaths (OpNeg exp) varDecls ifDepth xs
    let emptyLoop   = (preFixLoops (Assume (OpNeg exp)) emptyLoopPath)

    (bodyDepth, bodyResult)      <- scanWhile
    bodyPaths <- filterValidPaths (OpNeg exp) varDecls ifDepth (concat bodyResult)

    return $  (bodyDepth -1 ,emptyLoop ++ (preFixLoops (Assume (OpNeg exp)) bodyPaths))
      where
        scanWhile           = foldM scanfn (ifDepth, [xs]) [1..n]
        scanfn (depth, acc) _        = do
            -- filter the paths on is feasible and continue loop on feasible paths
            paths <- filterValidPaths exp varDecls depth (head acc)
            (newDepth, res) <- analyseTree varDecls (preFixLoops (Assume exp) paths) stmt n ifDepth
            return (newDepth -1, (res : acc))
        preFixLoops v []    = [[v]]
        preFixLoops v xss   = map ((:) v) xss

--(preFixLoops (Assume (OpNeg exp)) xs) ++ (preFixLoops (Assume (OpNeg exp)) (concat scanWhile))
--analyseTree varDecls [] s n = return [[s]]
analyseTree varDecls xs s n ifDepth = return $ (ifDepth ,map ((:) s) xs)

filterValidPaths :: Expr -> [VarDeclaration] -> Int -> [ProgramPath] -> IO [ProgramPath]
filterValidPaths g varDecls ifDepth paths = do
    if ifDepth > 0 then
         filterM (isBranchValid varDecls g) paths
    else
        return paths

isBranchValid :: [VarDeclaration] -> Expr -> ProgramPath -> IO Bool
isBranchValid varDecls g path = return =<< isSat =<< isGuardSat finalWlp varDecls
    where
        finalWlp = foldl (flip generateWlpGuard) g path
        isSat (Sat) = return True
        isSat _ = return False




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
