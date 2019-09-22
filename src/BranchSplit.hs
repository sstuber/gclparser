module BranchSplit where

import GCLParser.GCLDatatype
import Datatypes

-- TODO change the name of all variables in a block
-- TODO add means to process arrays
-- TODO get programs in the right structure to check validity

fetchPre :: Stmt -> Maybe PreCon
fetchPre (Assume a)     = Just a
fetchPre (Seq s1 s2)    = fetchPre s1
fetchPre _              = Nothing

fetchPost :: Stmt -> Maybe PostCon
fetchPost (Assert a)     = (Just a)
fetchPost (Seq s1 s2)    = fetchPost s2
fetchPost _              = Nothing

-- remove the first assume you find on the left side of the seqs
removePre :: Stmt -> Stmt
removePre (Seq (Assume _) s2) = s2
removePre s@(Seq s1 s2)       = Seq (removePre s1) s2
removePre s                   = s

removePost :: Stmt -> Stmt
removePost (Seq s1 (Assert _)) = s1
removePost s@(Seq s1 s2)       = Seq s1 (removePost s2)
removePost s                    = s

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

changeVarNames :: [VarDeclaration] -> Stmt -> Stmt
changeVarNames [(VarDeclaration name _)] stmt = (changeVarName name stmt)
changeVarNames ((VarDeclaration name _) : t) stmt = changeVarNames t (changeVarName name stmt)

-- TODO if the post condition is about local variables, then this is problematic for our program.
changeVarName :: String -> Stmt -> Stmt
-- TODO give the variable an unique name
changeVarName name (Assert expr) = Assert (replaceVar name (Var name) expr)
changeVarName name (Assume expr) = Assume (replaceVar name (Var name) expr)
changeVarName name (Assign name2 expr) = Assign (if name == name2 then ('1' : name)  else name2)
                                                (replaceVar name (Var name) expr)
changeVarName name (AAssign name2 index expr) = AAssign (if name == name2 then ('1' : name) else name2)
                                                        (replaceVar name (Var name) index)
                                                        (replaceVar name (Var name) index)
changeVarName name (Seq stmt1 stmt2) = Seq (changeVarName name stmt1) (changeVarName name stmt2)
changeVarName name (IfThenElse expr stmt1 stmt2) = IfThenElse (replaceVar name (Var name) expr)
                                                              (changeVarName name stmt1)
                                                              (changeVarName name stmt2)
changeVarName name (While expr stmt) = While (replaceVar name (Var name) expr) (changeVarName name stmt)
changeVarName name (Block decl stmt) = changeVarNames decl stmt
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

-- name of var to replace -> expression to replace with -> post condition
replaceVar :: String -> Expr -> Expr -> Expr
replaceVar name toReplaceExpr e@(Var name2)                 = if name2 == name then toReplaceExpr else e
replaceVar name toReplaceExpr (LitI int)                    = (LitI int)
replaceVar name toReplaceExpr (LitB bool)                   = (LitB bool)
replaceVar name toReplaceExpr (Parens expr)                 = Parens (replaceVar name toReplaceExpr expr)
replaceVar name toReplaceExpr (ArrayElem array index)       = ArrayElem (replaceVar name toReplaceExpr array)
                                                                        (replaceVar name toReplaceExpr index)
-- Maybe should go into newval with replaceVar?
replaceVar name toReplaceExpr (RepBy arrayname expr newval) = RepBy (replaceVar name toReplaceExpr arrayname)
                                                                    (replaceVar name toReplaceExpr expr)
                                                                    (replaceVar name toReplaceExpr newval)
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
