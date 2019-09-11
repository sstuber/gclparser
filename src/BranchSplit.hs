module BranchSplit where

import GCLParser.GCLDatatype

-- TODO first assume and last assert are pre and post conditions
-- convert 


splitBranch :: Stmt -> [[Stmt]]
splitBranch s@(Skip)        = [[s]]
splitBranch s@(Assume _ )   = [[s]]
splitBranch s@(AAssign _ _ _)    = [[s]]
splitBranch s@(Assign _ _)    = [[s]]
splitBranch s@(Seq s1 s2)       = [ x ++ y  | x <-  splitBranch s1, y <- splitBranch s2 ]
splitBranch s@(IfThenElse g s1 s2) = (map (\xs-> (Assume g) : xs) (splitBranch s1)) ++
      (map (\xs-> (Assume (OpNeg g) : xs)) (splitBranch s2))
-- TODO expand loop to go n times deep
-- Might be worthwhile to look into replacing ++ for something else
splitBranch s@(While exp stmt) = (map (\xs-> (Assume exp) : xs ++ [Assume (OpNeg exp)]) (splitBranch stmt))
splitBranch s = [[s]]


generateWlp :: Stmt

data DataWlp
    = VarWlp String
    | IntWlp Int
    | BoolWlp Bool
    | ExprWlp DataWlp OprWlp DataWlp
    | LogicWlp DataWlp LogicOpr DataWlp

test = LogicWlp (VarWlp "p") OrWlp (VarWlp "q")

data LogicOpr
    = AndWlp
    | OrWlp
    | ImpWlp


data OprWlp
    = Plus
    | Minus
    | Times
    | Div


exprToDataWlp :: Expr -> DataWlp

--1<x ∧ 0<x ∧ 1<x ∧ x≤2 ⇒ x=3




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
