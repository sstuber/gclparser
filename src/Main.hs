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
uNFOLDLOOP = 15

maxDepth :: Int
maxDepth = 50

ifDepth = 10

-- TODO Implement extra heuristics, possibly from papers.

main :: IO ()
main = do
    BS.writeFile "metrics/metrics.csv" $ encode [("Experiment round" :: String,
                                                  "Validity" :: String,
                                                  "Heuristics" :: String,
                                                  "Loop depth" :: String,
                                                  "If depth" :: String,
                                                  "N" :: String,
                                                  "Total number of inspected paths" :: String,
                                                  "Number of unfeasible paths" :: String,
                                                  "Time spent on verification" :: String,
                                                  "Time spent on finding unfeasible paths" :: String,
                                                  "Total time" :: String,
                                                  "Atoms" :: String)]
    (parseResult) <- parseGCLfile "examples/benchmark/pullUp.gcl"
    putStrLn "ParseResult"
    putStrLn (show parseResult)
    putStrLn ""

    -- putStrLn "Do you want to turn on the heuristics?"
    -- heuristics <- getLine
    let heuristics = False

    let (Right program) = parseResult
    let programInput =  (10, uNFOLDLOOP, ifDepth, True)
    loopProgram program programInput 1
    putStrLn "hello"

    -- Poging om loopProgram wat te verkorten.
    --let iets = [(x, uNFOLDLOOP, ifDepth, y) | x <- [2..10], y <- [True, False]]
    --putStrLn $ show iets
    --mapM_ (runProgram program) iets


loopProgram :: Program -> ProgramInput -> Int -> IO ()
loopProgram program input@(2, x, y, False) round = do
    runProgram program input round

loopProgram program input@(n, x, y, False) round = do
    runProgram program input round
    loopProgram program (n - 1, x, y, False) (round + 1)

loopProgram program input@(2, x, y, True) round = do
    runProgram program input round
    loopProgram program (10, x, y, False) (round + 1)

loopProgram program input@(n, x, y, True) round = do
    runProgram program input round
    loopProgram program (n - 1, x, y, True) (round + 1)

runProgram :: Program -> ProgramInput -> Int -> IO ()
runProgram program programInput@(n, loopdepth, ifdepth, heur) round = do
    ((validationTime, atoms, pathsChecked, infeasibleTime, infeasibleAmount, programValidity), totaltime) <- stopWatch (processProgram program programInput)

    putStrLn $ show programInput

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
    - # experiment round
    - Heuristics on or off
    - Loop depth
    - If depth
    - N
    - Total number of inspected paths
    - Unfeasible paths
    - Time spent on verification
    - Time spent on finding unfeasable paths
    ! Time spent on array assignment optimization
    - Total time spent
    - Total size of formulas
    -}
    BS.appendFile "metrics/metrics.csv" $ encode [(round :: Int,
                                                   show programValidity :: String,
                                                   show heur :: String,
                                                   loopdepth :: Int,
                                                   ifdepth :: Int,
                                                   n :: Int,
                                                   pathsChecked :: Int,
                                                   infeasibleAmount :: Int,
                                                   show validationTime :: String,
                                                   show infeasibleTime :: String,
                                                   show totaltime :: String,
                                                   atoms :: Int)]
    putStrLn "END"

processProgram :: Program -> ProgramInput -> IO ProgramOutput
processProgram program (n, loopDepth, ifDepthLocal, heuristic)  = do
    -- preprocess program
    (stmts, (Just pre), (Just post), varDecls) <- preProcessProgram program n

    -- every path ends with the precondition
    let branchRoot = [(maxDepth ,[(Assume pre)])]
    -- get all the feasible branches
    -- Kunnen we dit niet uitzetten als we de heuristieken uit hebben staan?
    (testDepth, infeasibleAmount, infeasibleTime, programPaths) <- analyseTree varDecls branchRoot stmts loopDepth ifDepthLocal heuristic

    --putStrLn "path depth ?"
    --mapM (\x -> putStrLn $ show (maxDepth - (fst x ))) programPaths

    let test = map snd programPaths
    --putStrLn "testDepth"
    --putStrLn $ show testDepth
    -- validate all feasible paths
    (pathDataList, validationTime) <- stopWatch (checkValidityOfProgram post test varDecls)


    displayTimeMetrics validationTime
    atoms <- displayAtomSize post (map snd programPaths)
    let programValidity = isProgramValid $ reverse pathDataList

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
isProgramValid [] = True
isProgramValid ((b,p): xs) = b


checkValidityOfProgram :: PostCon -> [ProgramPath] -> [VarDeclaration] -> IO [(Bool, ProgramPath)]
checkValidityOfProgram post [] vardec = return []
checkValidityOfProgram post (h : t) vardec = do
    let wlp   = foldl (flip generateWlp) post h
    --putStrLn $ show post
    --putStrLn "!!!!!!!!!!!WLP!!!!!!!!!!!!!!!!"
    --putStrLn $ show wlp
    z3Result  <- (isExprValid wlp vardec)
    let validity = z3Result == Valid
    --putStrLn "!!!--------------------VALIDITY--------------------!!!"
    --putStrLn $ show validity
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
