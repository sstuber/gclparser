module Main where

import BranchSplit
import GCLParser.Parser
import GCLParser.GCLDatatype
import Datatypes
import PreProcessing
import qualified Data.Map.Strict as M
import Z3Converter
import System.Clock
import Control.StopWatch
import Common

uNFOLDLOOP :: Int
uNFOLDLOOP = 1

-- Benchmarks
-- TODO Total number of inspected paths, and of these, the number of paths you manage to identify as unfeasible.
{- TODO Consumed computation time: time spent on verification, time spent on identifying unfeasible paths, time spent on
    array assignment optimization (see below), and total used time. -}

main :: IO ()
main = do
    let clock = Monotonic
    starttime <- getTime clock

    (parseResult) <- parseGCLfile "examples/benchmark/bsort.gcl"
    putStrLn "ParseResult"
    putStrLn (show parseResult)
    putStrLn ""
    let (Right program) = parseResult
    (stmts, pre, (Just post), varDecls) <- preProcessProgram program

    let branches = splitBranch stmts uNFOLDLOOP


    let wlp = map (foldr generateWlp post) branches
    putStrLn $ "Total number of branches: " ++ (show (length branches))
    putStrLn $ "Size of formulas: " ++ (show (countAtoms (head wlp)))
    --let wlp = foldr (\new acc -> (foldr generateWlp post new) : acc ) [] (take 5 branches)
    putStrLn "wlp below -------------------------------------- "
    putStrLn $ show (head wlp)
    let clock2 = Monotonic


    (isValid, validationTime) <- stopWatch (checkValidityOfProgram post branches varDecls 1)

    putStrLn "Paths checked on validity:"
    putStrLn $ show $ maximum $ map trd3 isValid


    putStrLn "----------------- Time Metrics ------------------"
    putStrLn $ "Runtime on checking validity: " ++ show (sec validationTime) ++ " seconds; " ++ show (nsec validationTime) ++ "  nanoseconds;"
    time <- getTime clock
    putStrLn $ show  "Total runtime of the program is: " ++ (show ((sec time) - (sec starttime)))
                          ++ " seconds and " ++ (show ((nsec time) - (nsec starttime))) ++ " nanoseconds."

    putStrLn "hello"


checkValidityOfProgram :: PostCon -> [ProgramPath] -> [VarDeclaration] -> Int -> IO[(Bool, ProgramPath, Int)]
checkValidityOfProgram post [h] vardec count = do
    validity <- (isExprValid (foldr generateWlp post h) vardec)
    let val = case validity of
              Valid ->
                  True
              UnValid ->
                  False
              Z3undef ->
                  False
    return [(val, h, count)]
checkValidityOfProgram post (h : t) vardec count = do
    validity <- (isExprValid (foldr generateWlp post h) vardec)
    let val = case validity of
          Valid ->
              True
          UnValid ->
              False
          Z3undef ->
              False
    res <- if val then do
          result <- (checkValidityOfProgram post t vardec (count + 1))
          return result
        else do
          putStrLn $ "!!PROGRAM INVALLID!!\n-------------------- \nFailed on path: " ++ (show h)
          return []
    return $ (val, h, count) : res


countAtoms :: Expr -> Int
countAtoms (BinopExpr And expr1 expr2) = (countAtoms expr1) + (countAtoms expr2)
countAtoms (BinopExpr Or expr1 expr2) = (countAtoms expr1) + (countAtoms expr2)
countAtoms (BinopExpr Implication expr1 expr2) = (countAtoms expr1) + (countAtoms expr2)
countAtoms (OpNeg expr) = countAtoms expr
countAtoms (Parens expr) = countAtoms expr
countAtoms (Forall _ expr) = countAtoms expr
countAtoms _ = 1


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
