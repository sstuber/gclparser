module PreProcessing where

import GCLParser.GCLDatatype
import Datatypes
import Common
import Data.Char

import qualified Data.Map.Strict as M

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

renameVars :: Stmt -> M.Map String String -> Stmt
renameVars (Block decls stmt) varMap = Block newDecls (renameVars stmt newMap)
    where
    -- TODO make it such that it increases in in count 
      newMap = foldr (\(VarDeclaration name ttype) acc -> M.insert name (updateName name) acc  ) varMap decls
      newDecls = map (\(VarDeclaration name ttype) -> VarDeclaration (replaceName name newMap) ttype) decls
      replaceName n m = case M.lookup n m of
        Nothing -> n
        Just newName -> newName
      updateName n = if (length (takeWhile isDigit n)) > 0
        then 
          (show (1 + (read (takeWhile isDigit n)))) ++ (dropWhile isDigit n)
        else
          '1' : n


renameVars (Seq stmt1 stmt2) map = Seq (renameVars stmt1 map) (renameVars stmt2 map)
renameVars (Assert e) map = Assert (replaceVarWithMap map e)
renameVars (Assume e) map = Assume (replaceVarWithMap map e)
renameVars (While e stmt) map = While (replaceVarWithMap map e) (renameVars stmt map)
renameVars (IfThenElse e stmt1 stmt2) map = IfThenElse (replaceVarWithMap map e)
                                        (renameVars stmt1 map)
                                        (renameVars stmt2 map)
renameVars (Assign name expr) map = Assign newName (replaceVarWithMap map expr)
    where
      newName = case M.lookup name map of
          Nothing -> name
          Just e   -> e
renameVars (AAssign name index expr) map = AAssign newName (replaceVarWithMap map index) (replaceVarWithMap map expr)
    where
      newName = case M.lookup name map of
          Nothing -> name
          Just e   -> e
renameVars Skip map = Skip
