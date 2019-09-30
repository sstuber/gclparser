module BranchSplit where

import GCLParser.GCLDatatype
import Datatypes
import PreProcessing
import Common
import Control.Monad
import Z3Converter

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


analyseTree :: [ProgramPath] -> Stmt -> Int -> IO [ProgramPath]
analyseTree xs s@(Seq s1 s2) n  = do
    leftResult <- analyseTree xs s1 n
    analyseTree leftResult s2 n

analyseTree xs s@(IfThenElse g s1 s2) n = do
    ifStmt <- analyseTree (map ((:) (Assume g))  xs) s1 n
    elseStmt <- analyseTree (map ((:) (Assume (OpNeg g))) xs) s2 n
    return $ ifStmt ++ elseStmt
analyseTree xs s@(While exp stmt) n = do
    let emptyLoop = (preFixLoops (Assume (OpNeg exp)) xs)
    bodyResult <- scanWhile
    return $  emptyLoop ++ (preFixLoops (Assume (OpNeg exp)) (concat bodyResult))
      where
        scanWhile = foldM scanfn [xs] [1..n]
        scanfn acc _ = analyseTree (preFixLoops (Assume  exp) (head acc)) stmt n >>= \res -> return (res : acc)
        preFixLoops v []  = [[v]]
        preFixLoops v xss  = map ((:) v) xss

--(preFixLoops (Assume (OpNeg exp)) xs) ++ (preFixLoops (Assume (OpNeg exp)) (concat scanWhile))
analyseTree [] s n = return [[s]]
analyseTree xs s n = return $ map ((:) s) xs


isBranchValid :: ProgramPath -> [VarDeclaration] -> Stmt -> Expr -> IO Bool
isBranchValid path varDecls pre g = return =<< isValid =<< isExprValid finalWlp varDecls
    where
        expr = foldl (flip generateWlpGuard) g path
        finalWlp = generateWlpGuard pre expr
        isValid (Valid) = return True
        isValid _ = return False




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
