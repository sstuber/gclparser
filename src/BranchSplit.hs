module BranchSplit where

import GCLParser.GCLDatatype
import Datatypes
import PreProcessing
import Common

-- TODO change the name of all variables in a block
-- TODO add means to process arrays
-- TODO get programs in the right structure to check validity


splitBranch :: Stmt -> [ProgramPath]
splitBranch s@(Seq s1 s2)           = [ x ++ y  | x <-  splitBranch s1, y <- splitBranch s2 ]
splitBranch s@(IfThenElse g s1 s2)  =
        (map (putInFront (Assume g))         (splitBranch s1)) ++
        (map (putInFront (Assume (OpNeg g))) (splitBranch s2))
      where
        putInFront x y = x : y
-- TODO expand loop to go n times deep
splitBranch s@(While exp stmt)      = (map (\xs-> (Assume exp) : xs ++ [Assume (OpNeg exp)]) (splitBranch stmt))
-- Not sure if this is the correct implementation of block, but needed it to test something.
splitBranch (Block declarations stmt) = splitBranch (changeVarNames declarations stmt)
splitBranch (Block [] stmt) = splitBranch stmt
splitBranch s = [[s]]

-- TODO trycatch

generatePost :: Stmt -> Expr
generatePost (Assert expr) = expr

-- assuming this is: program path -> post condition
generateWlp :: Stmt -> Expr -> Expr
generateWlp (Skip) post                 = post
generateWlp (Assume expr1) post        = BinopExpr Implication expr1 post
generateWlp (Assign name expr) post    = replaceVar name expr post
generateWlp (AAssign name expr newval) post = replaceVar name (RepBy (Var name) expr newval) post
-- TODO: This does not hold for the first assert, should make an exception for that. Maybe type match on []?
generateWlp (Assert expr1) post        = BinopExpr And expr1 post
-- Is sequence needed? Seems like we already deal with this in splitBranch. Maybe we can use sequence instead of a list?
-- generateWlp (Seq s1 s2) a@(Just expr2) = generateWlp s1 (generateWlp s2 a)
-- generateWlp (Block [decl:] stmt) expr = replaceVar generatename
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
