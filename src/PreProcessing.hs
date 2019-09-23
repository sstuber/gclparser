module PreProcessing where

import GCLParser.GCLDatatype
import Datatypes
import Common

-- TODO function to get all var names for Z3
   -- structure of pre-processing
   -- change all var names
   -- save different var names and types for Z3
   -- remove all blocks
   -- remove pre and post condition
   -- give to splitBranch

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

-- TODO change the var names before we make the program path.
changeVarNames :: [VarDeclaration] -> Stmt -> Stmt
changeVarNames [(VarDeclaration name _)] stmt = (changeVarName name stmt)
changeVarNames ((VarDeclaration name _) : t) stmt = changeVarNames t (changeVarName name stmt)

-- TODO change the array name in sizeoff when dealing with local vars, but not when assig
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