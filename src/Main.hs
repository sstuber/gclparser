module Main where

import GCLParser.Parser

main :: IO ()
main = do
    test <- parseGCLfile "examples/min.gcl"

    putStrLn "hello"








