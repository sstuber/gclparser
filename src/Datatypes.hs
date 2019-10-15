module Datatypes where

import GCLParser.GCLDatatype
import qualified Data.Map.Strict as M
import System.Clock

type ProgramPath = [Stmt]
data Z3Validation = Valid | UnValid | Z3undef deriving (Show, Eq)

type ProgramInput = (Int, Int, Int, Bool, Int)
type ProgramOutput = (TimeSpec, Int, Int, TimeSpec, Int, Bool)

type PreprocessResult = (Stmt, Maybe PreCon, Maybe PostCon, [VarDeclaration])--, PostCon
type PreCon           = Expr
type PostCon          = Expr