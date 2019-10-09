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
import qualified Data.Text.IO as TextIO
import qualified Data.ByteString.Lazy as BS

uNFOLDLOOP :: Int
uNFOLDLOOP = 5

maxDepth :: Int
maxDepth = 60

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
    let clock = Monotonic
    starttime <- getTime clock
    BS.writeFile "metrics/metrics.csv" $ encode [("Total time" :: String, "Atoms" :: String, "uNFOLDLOOP" :: String)]


    (parseResult) <- parseGCLfile "examples/benchmark/memberOf.gcl"

    putStrLn "ParseResult"
    putStrLn (show parseResult)
    putStrLn ""

    putStrLn "Do you want to turn on the heuristics?"
    -- heuristics <- getLine
    let heuristics = False

    let (Right program) = parseResult

    processProgram program starttime clock

{-


    (stmts, (Just pre), (Just post), varDecls) <- preProcessProgram program

    let branches = splitBranch stmts uNFOLDLOOP

    putStrLn $ "Size of formulas: " ++ (show (countAtoms wlp))

    let wlp = map (foldr generateWlp post) branches
    putStrLn $ "Total number of branches: " ++ (show (length branches))
    putStrLn $ "Size of formulas: " ++ (show (countAtoms (head wlp)))
    test <- analyseTree varDecls [[(Assume pre)]] stmts uNFOLDLOOP

-}
    putStrLn "hello"

processProgram :: Program -> TimeSpec -> Clock -> IO ()
processProgram program startTime clock = do
    -- preprocess program
    (stmts, (Just pre), (Just post), varDecls) <- preProcessProgram program 2

    putStrLn "test"


    -- every path ends with the precondition
    let branchRoot = [(maxDepth ,[(Assume pre)])]
    -- get all the feasible branches
    (testDepth, programPaths) <- analyseTree varDecls branchRoot stmts uNFOLDLOOP ifDepth True

    putStrLn "path depth ?"
    mapM (\x -> putStrLn $ show (maxDepth - (fst x ))) programPaths

    let test = map snd programPaths
    putStrLn "testDepth"
    putStrLn $ show testDepth
    -- validate all feasible paths
    (pathDataList, validationTime) <- stopWatch (checkValidityOfProgram post test varDecls 1)

    putStrLn "Paths checked on validity:"
    putStrLn $ show $ length pathDataList

    time <- displayTimeMetrics validationTime startTime clock
    atoms <- displayAtomSize post programPaths
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
    BS.appendFile "metrics/metrics.csv" $ encode [(show time :: String, atoms :: Int, uNFOLDLOOP :: Int)]


    return ()

replaceNbyIntTree :: Int -> Stmt  -> Stmt
replaceNbyIntTree i = replaceVarStmt "N" (LitI i)

displayTimeMetrics :: TimeSpec -> TimeSpec -> Clock-> IO (TimeSpec)
displayTimeMetrics validationTime startTime clock = do
    putStrLn "----------------- Time Metrics ------------------"
    putStrLn $ "Runtime on checking validity: " ++ show (sec validationTime) ++ " seconds; " ++ show (nsec validationTime) ++ "  nanoseconds;"
    time <- getTime clock
    putStrLn $ show  "Total runtime of the program is: " ++ show ((sec time) - (sec startTime))
                          ++ " seconds and " ++ show ((nsec time) - (nsec startTime)) ++ " nanoseconds."
    return(TimeSpec ((sec time) - (sec startTime)) ((nsec time) - (nsec startTime)))

displayAtomSize :: PostCon -> [ProgramPath] -> IO Int
displayAtomSize post path = do
    let wlp = map (foldr generateWlp post) path
    let atoms = foldr (+) 0 (map countAtoms wlp)
    putStrLn "------------Total size of atoms----------------"
    putStrLn $ show atoms
    return atoms

checkValidityOfProgram :: PostCon -> [ProgramPath] -> [VarDeclaration] -> IO[(Bool, ProgramPath)]
checkValidityOfProgram post [] vardec = return []
checkValidityOfProgram post (h : t) vardec = do
    let wlp   = foldl (flip generateWlp) post h
    let atoms = countAtoms wlp

    z3Result  <- (isExprValid wlp vardec)
    let validity = z3Result == Valid

    res       <- if validity then do
          result <- (checkValidityOfProgram post t vardec)
          return result
        else do
          putStrLn $ "!!PROGRAM INVALLID!!\n-------------------- \nFailed on path: " ++ (show h)
          return []
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
    let uniqueVars = renameVars programBody M.empty
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
