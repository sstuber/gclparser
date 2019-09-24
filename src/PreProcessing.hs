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

-- TODO does [] work as last statement?
findAllNamesAndTypes :: Stmt -> [VarDeclaration]
findAllNamesAndTypes (Seq stmt1 stmt2)          = (findAllNamesAndTypes stmt1) ++ (findAllNamesAndTypes stmt2)
findAllNamesAndTypes (IfThenElse _ stmt1 stmt2) = (findAllNamesAndTypes stmt1) ++ (findAllNamesAndTypes stmt2)
findAllNamesAndTypes (While _ stmt)             = (findAllNamesAndTypes stmt)
findAllNamesAndTypes (Block vars stmt)          = vars ++ (findAllNamesAndTypes stmt)
findAllNamesAndTypes _                          = []

removeAllBlocks :: Stmt -> Stmt
removeAllBlocks s@(Assert _)                  = s
removeAllBlocks s@(Assume _)                  = s
removeAllBlocks s@(Assign _ _)                = s
removeAllBlocks s@(AAssign name2 index expr)  = s
removeAllBlocks (Seq stmt1 stmt2)             = Seq (removeAllBlocks stmt1) (removeAllBlocks stmt2)
removeAllBlocks (IfThenElse expr stmt1 stmt2) = IfThenElse expr (removeAllBlocks stmt1) (removeAllBlocks stmt2)
removeAllBlocks (While expr stmt)             = While expr (removeAllBlocks stmt)
removeAllBlocks (Block decl stmt)             = removeAllBlocks stmt
removeAllBlocks (Skip)                        = Skip