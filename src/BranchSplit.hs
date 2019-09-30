module BranchSplit where

import GCLParser.GCLDatatype
import Datatypes
import PreProcessing
import Common

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

{-
= Skip
    | Assert     Expr
    | Assume     Expr
    | Assign     String           Expr
    | AAssign    String           Expr   Expr
    | DrefAssign String           Expr
    | Seq        Stmt             Stmt
    | IfThenElse Expr             Stmt   Stmt
    | While      Expr             Stmt
    | Block      [VarDeclaration] Stmt
    | TryCatch   String           Stmt   Stmt
-}
