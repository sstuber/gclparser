module Main where

import BranchSplit
import GCLParser.Parser
import GCLParser.PrettyPrint
import GCLParser.GCLDatatype
import Datatypes

main :: IO ()
main = do
    (test1) <- parseGCLfile "examples/mintest.gcl"
    putStrLn (show test1)
    let (Right program) = test1
    let test = splitBranch (stmt program)

    putStrLn $ show test


    putStrLn "hello"



preProcessProgram :: Program -> IO PreprocessResult
preProcessProgram program = do
    putStrLn "Start Preprocess"
    let programBody = stmt program

    -- fetch precondition
    let maybePreCon = splitPre programBody
    -- remove precondition
    noPreBody <- removePreCondition maybePreCon programBody

    return (noPreBody, maybePreCon)

-- TODO write a function like this for postCondition except post condition has to be present
removePreCondition :: Maybe PreCon -> Stmt -> IO Stmt
removePreCondition maybePreCon body = do
    newBody <- case maybePreCon of
            Nothing -> do
                putStrLn "No precondition found"
                return body
            Just x  -> do
                putStrLn $ "Precondition found -> " ++ (show x)
                let fixedBody = removePre body
                return fixedBody
    return newBody
