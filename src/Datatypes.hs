module Datatypes where

import GCLParser.GCLDatatype
import qualified Data.Map.Strict as M

type ProgramPath = [Stmt]
data Z3Validation = Valid | UnValid | Z3undef deriving (Show, Eq)

type PreprocessResult = (Stmt, Maybe PreCon, Maybe PostCon, [VarDeclaration])--, PostCon
type PreCon           = Expr
type PostCon          = Expr