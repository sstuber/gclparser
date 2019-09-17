module BranchSplit where

import GCLParser.GCLDatatype
import Datatypes

-- TODO first assume and last assert are pre and post conditions
-- convert
-- TODO change the name of all variables in a block

splitPre :: Stmt -> Maybe Expr
splitPre (Assume a)     = (Just a)
splitPre (Seq s1 s2)    = splitPre s1
splitPre _              = Nothing

splitPost :: Stmt -> Maybe Expr
splitPost (Assert a)     = (Just a)
splitPost (Seq s1 s2)    = splitPost s2
splitPost _              = Nothing

-- remove the first assume you find on the left side of the seqs
removePre :: Stmt -> Stmt
removePre (Seq (Assume _) s2) = s2
removePre s@(Seq s1 s2)       = Seq (removePre s1) s2
removePre s                   = s

splitBranch :: Stmt -> [ProgramPath]
splitBranch s@(Skip)                = [[s]]
splitBranch s@(Assume _ )           = [[s]]
splitBranch s@(AAssign _ _ _)       = [[s]]
splitBranch s@(Assign _ _)          = [[s]]
splitBranch s@(Seq s1 s2)           = [ x ++ y  | x <-  splitBranch s1, y <- splitBranch s2 ]
splitBranch s@(IfThenElse g s1 s2)  =
        (map (putInFront (Assume g))         (splitBranch s1)) ++
        (map (putInFront (Assume (OpNeg g))) (splitBranch s2))
      where
        putInFront x y = x : y
-- TODO expand loop to go n times deep
-- Might be worthwhile to look into replacing ++ for something else
splitBranch s@(While exp stmt)      = (map (\xs-> (Assume exp) : xs ++ [Assume (OpNeg exp)]) (splitBranch stmt))
splitBranch s = [[s]]

generatePost :: Stmt -> Expr
generatePost (Assert expr) = expr

-- assuming this is: program path -> post condition
generateWlp :: Stmt -> Expr -> Expr
generateWlp (Skip) expr                 = expr
generateWlp (Assume expr1) expr2        = BinopExpr Implication expr1 expr2
generateWlp (Assign name expr) expr2    = replaceVar name expr expr2
-- TODO: This does not hold for the first assert, should make an exception for that. Maybe type match on []?
generateWlp (Assert expr1) expr2        = BinopExpr And expr1 expr2
-- Is sequence needed? Seems like we already deal with this in splitBranch. Maybe we can use sequence instead of a list?
-- generateWlp (Seq s1 s2) a@(Just expr2) = generateWlp s1 (generateWlp s2 a)
-- generateWlp (Block [decl:] stmt) expr = replaceVar generatename
generateWlp (IfThenElse g s1 s2) expr2  = BinopExpr And ifSide elseSide
    where
        ifSide   = BinopExpr Implication g (generateWlp s1 expr2)
        elseSide = BinopExpr Implication (OpNeg g)  (generateWlp s2 expr2)

-- name of var to replace -> expression to replace with -> post condition
replaceVar :: String -> Expr -> Expr -> Expr
replaceVar name toReplaceExpr e@(Var name2)                 = if name2 == name then toReplaceExpr else e
replaceVar name toReplaceExpr (LitI int)                    = (LitI int)
replaceVar name toReplaceExpr (LitB bool)                   = (LitB bool)
replaceVar name toReplaceExpr (Parens expr)                 = Parens (replaceVar name toReplaceExpr expr)
replaceVar name toReplaceExpr (OpNeg expr)                  = OpNeg (replaceVar name toReplaceExpr expr)
replaceVar name toReplaceExpr (BinopExpr op expr1 expr2)    = BinopExpr op replacedExpr1 replacedExpr2
    where
        replacedExpr1 = replaceVar name toReplaceExpr expr1
        replacedExpr2 = replaceVar name toReplaceExpr expr2


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
