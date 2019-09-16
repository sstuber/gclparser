module Datatypes where

import GCLParser.GCLDatatype

type ProgramPath = [Stmt]

type PreprocessResult = (Stmt, Maybe PreCon)--, PostCon
type PreCon           = Expr
type PostCon          = Expr