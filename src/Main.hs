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
import Data.Csv
import Control.Monad.Supply
import qualified Data.Text.IO as TextIO
import qualified Data.ByteString.Lazy as BS

uNFOLDLOOP :: Int
uNFOLDLOOP = 2

maxDepth :: Int
maxDepth = 15

ifDepth = 0

-- Benchmarks
-- TODO Total number of inspected paths, and of these, the number of paths you manage to identify as unfeasible.
{- TODO Consumed computation time: time spent on verification, time spent on identifying unfeasible paths, time spent on
    array assignment optimization (see below), and total used time. -}
-- TODO Write a function that tests programs and incorporates the variable N.
-- TODO Heuristics on and off.
-- TODO Implement extra heuristics, possibly from papers.

main :: IO ()
main = do
    BS.writeFile "metrics/metrics.csv" $ encode [("Total time" :: String, "Atoms" :: String, "uNFOLDLOOP" :: String)]
    (parseResult) <- parseGCLfile "examples/benchmark/memberOf.gcl"

    putStrLn "ParseResult"
    putStrLn (show parseResult)
    putStrLn ""

    putStrLn "Do you want to turn on the heuristics?"
    -- heuristics <- getLine
    let heuristics = False

    let (Right program) = parseResult
    let programInput =  (1, uNFOLDLOOP, ifDepth, True)
    loopProgram program programInput
    {-
    ((validationTime, atoms, pathsChecked, infeasibleTime, infeasibleAmount, programValidity), totaltime) <- stopWatch (processProgram program programInput)

    putStrLn "------------- VALIDATION TIME -------------"
    putStrLn $ show validationTime

    putStrLn "------------- TOTAL ATOMS -----------------"
    putStrLn $ show atoms

    putStrLn "------------- PATHS INFEASIBLE ------------"
    putStrLn $ show infeasibleAmount
    putStrLn "------------- PATHS VALIDATED -------------"
    putStrLn $ show pathsChecked

    putStrLn "------------- EXTRA TIME FEASIBILITY ------"
    putStrLn $ show infeasibleTime

    putStrLn "------------- PROGRAM VALID ----------------"
    putStrLn $ show programValidity

    putStrLn "-------------- TOTALTIME ------------------"
    putStrLn $ show  "Total runtime of the program is: " ++ show (sec totaltime)
                     ++ " seconds and " ++ show (nsec totaltime) ++ " nanoseconds."

                     -}

{- Metrics written to file: (! indicates that it is not yet added)
    ! # experiment round
    ! Heuristics on or off
    ! Loop depth
    ! N
    ! Total number of inspected paths
    ! Unfeasible paths
    - Time spent on verification
    ! Time spent on finding unfeasable paths
    ! Time spent on array assignment optimization
    - Total size of formulas
    -}
    --BS.appendFile "metrics/metrics.csv" $ encode [(show totaltime :: String, atoms :: Int, uNFOLDLOOP :: Int)]
    putStrLn "hello"

loopProgram :: Program -> ProgramInput -> IO ()
loopProgram program input@(1, x, y, False) = do
    runProgram program input

loopProgram program input@(n, x, y, False) = do
    runProgram program input
    loopProgram program (n - 1, x, y, False)

loopProgram program input@(1, x, y, True) = do
    runProgram program input
    loopProgram program (8, x, y, False)

loopProgram program input@(n, x, y, True) = do
    runProgram program input
    loopProgram program (n - 1, x, y, True)

runProgram :: Program -> ProgramInput -> IO ()
runProgram program programInput = do
    ((validationTime, atoms, pathsChecked, infeasibleTime, infeasibleAmount, programValidity), totaltime) <- stopWatch (processProgram program programInput)

    putStrLn "------------- VALIDATION TIME -------------"
    putStrLn $ show validationTime

    putStrLn "------------- TOTAL ATOMS -----------------"
    putStrLn $ show atoms

    putStrLn "------------- PATHS INFEASIBLE ------------"
    putStrLn $ show infeasibleAmount
    putStrLn "------------- PATHS VALIDATED -------------"
    putStrLn $ show pathsChecked

    putStrLn "------------- EXTRA TIME FEASIBILITY ------"
    putStrLn $ show infeasibleTime

    putStrLn "------------- PROGRAM VALID ----------------"
    putStrLn $ show programValidity

    putStrLn "-------------- TOTALTIME ------------------"
    putStrLn $ show  "Total runtime of the program is: " ++ show (sec totaltime)
                     ++ " seconds and " ++ show (nsec totaltime) ++ " nanoseconds."

    {- Metrics written to file: (! indicates that it is not yet added)
    ! # experiment round
    ! Heuristics on or off
    ! Loop depth
    ! N
    ! Total number of inspected paths
    ! Unfeasible paths
    - Time spent on verification
    ! Time spent on finding unfeasable paths
    ! Time spent on array assignment optimization
    - Total size of formulas
    -}
    BS.appendFile "metrics/metrics.csv" $ encode [(show totaltime :: String, atoms :: Int, uNFOLDLOOP :: Int)]
    putStrLn "END"


{- Metrics written to file: (! indicates that it is not yet added)
    ! # experiment round
    ! Heuristics on or off
    ! Loop depth
    ! N
    ! Total number of inspected paths
    ! Unfeasible paths
    - Time spent on verification
    ! Time spent on finding unfeasable paths
    ! Time spent on array assignment optimization
    - Total size of formulas
    -}
    BS.appendFile "metrics/metrics.csv" $ encode [(show totaltime :: String, atoms :: Int, uNFOLDLOOP :: Int)]
    putStrLn "hello"


processProgram :: Program -> ProgramInput -> IO ProgramOutput
processProgram program (n, loopDepth, ifDepthLocal, heuristic)  = do
    -- preprocess program
    (stmts, (Just pre), (Just post), varDecls) <- preProcessProgram program n

    putStrLn "test"

    -- every path ends with the precondition
    let branchRoot = [(maxDepth ,[(Assume pre)])]
    -- get all the feasible branches
    (testDepth, infeasibleAmount, infeasibleTime, programPaths) <- analyseTree varDecls branchRoot stmts loopDepth ifDepthLocal heuristic
    putStrLn "infeasible and time"
    putStrLn $ show infeasibleAmount
    putStrLn $ show infeasibleTime

    putStrLn "path depth ?"
    mapM (\x -> putStrLn $ show (maxDepth - (fst x ))) programPaths

    let test = map snd programPaths
    putStrLn "testDepth"
    putStrLn $ show testDepth
    -- validate all feasible paths
    (pathDataList, validationTime) <- stopWatch (checkValidityOfProgram post test varDecls)

    --putStrLn "Paths checked on validity:"
    --putStrLn $ show $ length pathDataList

    displayTimeMetrics validationTime
    atoms <- displayAtomSize post (map snd programPaths)
    let programValidity = isProgramValid pathDataList

    return (validationTime, atoms, length pathDataList, infeasibleTime, infeasibleAmount, programValidity)

replaceNbyIntTree :: Int -> Stmt  -> Stmt
replaceNbyIntTree i = replaceVarStmt "N" (LitI i)

displayTimeMetrics :: TimeSpec -> IO ()
displayTimeMetrics validationTime = do
    putStrLn "----------------- Time Metrics ------------------"
    putStrLn $ "Runtime on checking validity: " ++ show (sec validationTime) ++ " seconds; " ++ show (nsec validationTime) ++ "  nanoseconds;"

displayAtomSize :: PostCon -> [ProgramPath] -> IO Int
displayAtomSize post path = do
    let wlp = map (foldr generateWlp post) path
    let atoms = foldr (+) 0 (map countAtoms wlp)
    return atoms

isProgramValid :: [(Bool, ProgramPath)] -> Bool
isProgramValid [] = False
isProgramValid ((b,p): xs) = b


checkValidityOfProgram :: PostCon -> [ProgramPath] -> [VarDeclaration] -> IO [(Bool, ProgramPath)]
checkValidityOfProgram post [] vardec = return []
checkValidityOfProgram post (h : t) vardec = do
    let wlp   = foldl (flip generateWlp) post h
    putStrLn $ show post
    putStrLn $ show wlp
    z3Result  <- (isExprValid wlp vardec)
    let validity = z3Result == Valid

    res       <- if validity then do
          result <- (checkValidityOfProgram post t vardec)
          return result
        else do
          putStrLn $ "!!PROGRAM INVALLID!!\n-------------------- \nFailed on path: " ++ (show h)
          return [(False, h)]
    return $ (validity, h) : res


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



preProcessProgram :: Program -> Int-> IO PreprocessResult
preProcessProgram program n = do
    putStrLn "Start Preprocess"
    putStrLn "------------------------------------------------------------------- "
    let programBody = stmt program

    putStrLn "Process local variables"
    let (uniqueVars,_) = runSupply (renameVars programBody M.empty) [1..]
    let allVarDeclarations = (input program) ++ (output program) ++ (findAllNamesAndTypes uniqueVars)
    putStrLn $ (++) "Total amount of (local) variables: "  $ show . length $ allVarDeclarations
    -- TODO maybe pretty print all vars
    putStrLn ""
    -- Give back 9 trees, one for every n
    let nPlaced =  replaceNbyIntTree n uniqueVars

    putStrLn "Procces pre- and postconditions"
    let noBlocks = removeAllBlocks nPlaced
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
