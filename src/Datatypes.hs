module Datatypes where

import GCLParser.GCLDatatype
import qualified Data.Map.Strict as M

type ProgramPath = [Stmt]

type PreprocessResult = (Stmt, Maybe PreCon, Maybe PostCon, [VarDeclaration])--, PostCon
type PreCon           = Expr
type PostCon          = Expr