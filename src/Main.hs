module Main where

import BranchSplit
import GCLParser.Parser
import GCLParser.GCLDatatype
import Datatypes
import PreProcessing
import qualified Data.Map.Strict as M
import Z3Converter

uNFOLDLOOP :: Int
uNFOLDLOOP = 1



main :: IO ()
main = do
    (test1) <- parseGCLfile "../examples/min.gcl"
    putStrLn (show test1)
    let (Right program) = test1
    let test = splitBranch (stmt program) uNFOLDLOOP

    -- putStrLn $ show test
    (proc, pre, (Just post), varDecls) <- preProcessProgram program

    putStrLn "TEST"
    putStrLn $ show proc
    let branches = splitBranch proc uNFOLDLOOP

    let wlp = foldr generateWlp post (head branches)
    putStrLn $ show (head branches)
    --putStrLn $ show (length branches)
    --let wlp = foldr (\new acc -> (foldr generateWlp post new) : acc ) [] (take 5 branches)
    --putStrLn $ show branches
    putStrLn "wlp below -------------------------------------- "
    putStrLn $ show wlp

    isValid <- isExprValid wlp varDecls

    putStrLn $ show isValid
    --putStrLn $ show proc


    putStrLn "hello"



preProcessProgram :: Program -> IO PreprocessResult
preProcessProgram program = do
    putStrLn "Start Preprocess"
    let programBody = stmt program

    let uniqueVars = renameVars programBody M.empty
    let allVarDeclarations = (input program) ++ (output program) ++ (findAllNamesAndTypes uniqueVars)
    putStrLn $ "allNamesAndTypes " ++ (show allVarDeclarations)

    let noBlocks = removeAllBlocks uniqueVars
    let maybePreCon = fetchPre noBlocks
    let maybePostCon = fetchPost noBlocks
    noPreBody <- removePreCondition maybePreCon noBlocks
    noPostBody <- removePostCondition maybePostCon noPreBody

    -- TODO clean up
    --varDecls <- fetchVarDecls program noPostBody

    return (noPostBody, maybePreCon, maybePostCon, allVarDeclarations)

fetchVarDecls :: Program -> Stmt -> IO [VarDeclaration]
fetchVarDecls program stmt = do
    let inVars = input program
    let outVars = output program
    -- TODO vars from blocks to varDecl list
    return (inVars ++ outVars)


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
