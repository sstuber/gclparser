module Main where

import BranchSplit
import GCLParser.Parser
import GCLParser.PrettyPrint
import GCLParser.GCLDatatype
import Datatypes
import PreProcessing

uNFOLDLOOP :: Int
uNFOLDLOOP = 1


main :: IO ()
main = do
    (test1) <- parseGCLfile "examples/swap.gcl"
    putStrLn (show test1)
    let (Right program) = test1
    let test = splitBranch (stmt program) uNFOLDLOOP

    -- putStrLn $ show test
    (proc, pre, (Just post)) <- preProcessProgram program

    let branches = splitBranch proc uNFOLDLOOP
    let wlp = foldr generateWlp post (head branches)
    putStrLn $ show (head branches)
    putStrLn $ show wlp


    --putStrLn $ show proc


    putStrLn "hello"



preProcessProgram :: Program -> IO PreprocessResult
preProcessProgram program = do
    putStrLn "Start Preprocess"
    let programBody = stmt program

    let maybePreCon = fetchPre programBody
    let maybePostCon = fetchPost programBody
    noPreBody <- removePreCondition maybePreCon programBody
    noPostBody <- removePostCondition maybePostCon noPreBody

    return (noPostBody, maybePreCon, maybePostCon)

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

removePostCondition :: Maybe PostCon -> Stmt -> IO Stmt
removePostCondition maybePostCon body = do
    newBody <- case maybePostCon of
            Nothing -> do
                putStrLn "No postcondition found"
                error "No postcondition"
            Just x -> do
                putStrLn $ "Postcondition found -> " ++ (show x)
                let fixedBody = removePost body
                return fixedBody
    return newBody
