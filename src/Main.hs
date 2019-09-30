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

-- Benchmarks
-- TODO Total number of inspected paths, and of these, the number of paths you manage to identify as unfeasible.
-- TODO Consumed computation time: time spent on verification, time spent on identifying unfeasible paths, time spent on
-- array assignment optimization (see below), and total used time.
-- TODO Total size of the formulas that you have to verify. We’ll define size of a formula f to be the number of atoms in
-- f . An atom is a maximal expression that does not contain a boolean operator. For example the formula 1<x ∧ 0<x
-- has two atoms, and its size is 2.

main :: IO ()
main = do
    (parseResult) <- parseGCLfile "examples/benchmark/pullUp.gcl"
    putStrLn "ParseResult"
    putStrLn (show parseResult)
    putStrLn ""
    let (Right program) = parseResult
    (stmts, pre, (Just post), varDecls) <- preProcessProgram program

    let branches = splitBranch stmts uNFOLDLOOP

    let wlp = map (foldr generateWlp post) branches
    putStrLn $ show (head branches)
    --putStrLn $ show (length branches)
    --let wlp = foldr (\new acc -> (foldr generateWlp post new) : acc ) [] (take 5 branches)
    putStrLn "wlp below -------------------------------------- "
    putStrLn $ show (head wlp)

    let isValid = areExprValid wlp varDecls
    isValidHead <- (head isValid)
    putStrLn $ show isValidHead

    putStrLn "hello"

processSinglePath :: [VarDeclaration] -> Maybe PreCon -> PostCon  -> ProgramPath -> IO Z3Validation
processSinglePath varDecls pre post path = do
    let wlp = foldr generateWlp post path
    let finalWlp = case pre of
          Nothing -> wlp
          Just x  -> generateWlp (Assume x) wlp
    
    isExprValid finalWlp varDecls



preProcessProgram :: Program -> IO PreprocessResult
preProcessProgram program = do
    putStrLn "Start Preprocess"
    putStrLn "------------------------------------------------------------------- "
    let programBody = stmt program

    putStrLn "Process local variables"
    let uniqueVars = renameVars programBody M.empty
    let allVarDeclarations = (input program) ++ (output program) ++ (findAllNamesAndTypes uniqueVars)
    putStrLn $ (++) "Total amount of (local) variables: "  $ show . length $ allVarDeclarations
    -- TODO maybe pretty print all vars
    putStrLn ""

    putStrLn "Procces pre- and postconditions"
    let noBlocks = removeAllBlocks uniqueVars
    let maybePreCon = fetchPre noBlocks
    let maybePostCon = fetchPost noBlocks
    noPreBody <- removePreCondition maybePreCon noBlocks
    finalBody <- removePostCondition maybePostCon noPreBody
    putStrLn ""
    putStrLn "Preprocessed body"
    putStrLn $ show finalBody
    putStrLn ""


    putStrLn "End Preprocess"
    putStrLn "------------------------------------------------------------------- "

    return (finalBody, maybePreCon, maybePostCon, allVarDeclarations)

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
                putStrLn $ "Precondition found: " ++ (show x)
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
                putStrLn $ "Postcondition found: " ++ (show x)
                let fixedBody = removePost body
                return fixedBody
    return newBody
