module BranchSplit where

import GCLParser.GCLDatatype

splitBranch :: Stmt -> [[Stmt]]
splitBranch s@(Skip)        = [[s]]
splitBranch s@(Assume _ )   = [[s]]
splitBranch s@(AAssign _ _ _)    = [[s]]
splitBranch s@(Assign _ _)    = [[s]]
splitBranch s@(Seq s1 s2)       = [ x ++ y  | x <-  splitBranch s1, y <- splitBranch s2 ]
splitBranch s@(IfThenElse g s1 s2) = (map (\xs-> (Assume g) : xs) (splitBranch s1)) ++
      (map (\xs-> (Assume (OpNeg g) : xs)) (splitBranch s2))
-- Might be worthwhile to look into replacing ++ for something else
splitBranch s@(While exp stmt) = (map (\xs-> (Assume exp) : xs ++ [Assume (OpNeg exp)]) (splitBranch stmt))
splitBranch s = [[s]]

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
