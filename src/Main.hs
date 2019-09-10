module Main where

import BranchSplit
import GCLParser.Parser
import GCLParser.PrettyPrint
import GCLParser.GCLDatatype

main :: IO ()
main = do
    (test1) <- parseGCLfile "examples/mintest.gcl"
    putStrLn (show test1)
    let (Right program) = test1
    let test = splitBranch (stmt program)

    putStrLn $ show test


    putStrLn "hello"








