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

    (parseResult) <- parseGCLfile "../examples/E.gcl"

    putStrLn "ParseResult"
    putStrLn (show parseResult)
    putStrLn ""

    let (Right program) = parseResult

    processProgram program starttime clock

{-


    (stmts, (Just pre), (Just post), varDecls) <- preProcessProgram program

    let branches = splitBranch stmts uNFOLDLOOP


    let wlp = map (foldr generateWlp post) branches
    putStrLn $ "Total number of branches: " ++ (show (length branches))
    putStrLn $ "Size of formulas: " ++ (show (countAtoms (head wlp)))
    test <- analyseTree varDecls [[(Assume pre)]] stmts uNFOLDLOOP
    --let wlp = foldr (\new acc -> (foldr generateWlp post new) : acc ) [] (take 5 branches)
    putStrLn "wlp below -------------------------------------- "
    putStrLn $ show wlp
    let clock2 = Monotonic


    (pathData, validationTime) <- stopWatch (checkValidityOfProgram post branches varDecls 1)

    putStrLn "Paths checked on validity:"
    putStrLn $ show $ length pathData


    putStrLn "----------------- Time Metrics ------------------"
    putStrLn $ "Runtime on checking validity: " ++ show (sec validationTime) ++ " seconds; " ++ show (nsec validationTime) ++ "  nanoseconds;"
    time <- getTime clock
    putStrLn $ show  "Total runtime of the program is: " ++ (show ((sec time) - (sec starttime)))
                          ++ " seconds and " ++ (show ((nsec time) - (nsec starttime))) ++ " nanoseconds."
-}
    putStrLn "hello"

processProgram :: Program -> TimeSpec -> Clock -> IO ()
processProgram program startTime clock= do
    -- preprocess program
    (stmts, (Just pre), (Just post), varDecls) <- preProcessProgram program

    -- every path ends with the precondition
    let branchRoot = [[(Assume pre)]]
    -- get all the feasible branches
    programPaths <- analyseTree varDecls [[(Assume pre)]] stmts uNFOLDLOOP

    -- validate all feasible paths
    (pathDataList, validationTime) <- stopWatch (checkValidityOfProgram post programPaths varDecls 1)

    putStrLn "Paths checked on validity:"
    putStrLn $ show $ length pathDataList

    displayTimeMetrics validationTime startTime clock
    return ()

displayTimeMetrics :: TimeSpec -> TimeSpec -> Clock-> IO ()
displayTimeMetrics validationTime startTime clock = do
    putStrLn "----------------- Time Metrics ------------------"
    putStrLn $ "Runtime on checking validity: " ++ show (sec validationTime) ++ " seconds; " ++ show (nsec validationTime) ++ "  nanoseconds;"
    time <- getTime clock
    putStrLn $ show  "Total runtime of the program is: " ++ (show ((sec time) - (sec startTime)))
                          ++ " seconds and " ++ (show ((nsec time) - (nsec startTime))) ++ " nanoseconds."

checkValidityOfProgram :: PostCon -> [ProgramPath] -> [VarDeclaration] -> Int -> IO[(Bool, ProgramPath, Int)]
checkValidityOfProgram post [] vardec count = return []
checkValidityOfProgram post (h : t) vardec count = do
    let wlp   = foldl (flip generateWlp) post h

    z3Result  <- (isExprValid wlp vardec)
    let validity = z3Result == Valid

    res       <- if validity then do
          result <- (checkValidityOfProgram post t vardec (count + 1))
          return result
        else do
          putStrLn $ "!!PROGRAM INVALLID!!\n-------------------- \nFailed on path: " ++ (show h)
          return []
    return $ (validity, h, count) : res


countAtoms :: Expr -> Int
countAtoms (BinopExpr And expr1 expr2) = (countAtoms expr1) + (countAtoms expr2)
countAtoms (BinopExpr Or expr1 expr2) = (countAtoms expr1) + (countAtoms expr2)
countAtoms (BinopExpr Implication expr1 expr2) = (countAtoms expr1) + (countAtoms expr2)
countAtoms (OpNeg expr) = countAtoms expr
countAtoms (Parens expr) = countAtoms expr
countAtoms (Forall _ expr) = countAtoms expr
countAtoms _ = 1

-- possibly not usefull
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
