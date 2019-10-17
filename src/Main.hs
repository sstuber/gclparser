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

import HandleProgram

uNFOLDLOOP :: Int
uNFOLDLOOP = 15

maxDepth :: Int
maxDepth = 50

ifDepth = 10

metricsDirectory = "../metrics/"
metricsFileType = ".csv"
benchmarkDirectory = "../examples/benchmark/"
benchmarkFileType = ".gcl"
-- loop guard k
benchmarkFiletest = benchmarkFile "pullUp" [5,10] [5] [20]

-- TODO Implement extra heuristics, possibly from papers.

main :: IO ()
main = do
    BS.writeFile "../metrics/metrics.csv" $ encode [("Experiment round" :: String,
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

    (parseResult) <- parseGCLfile "../examples/benchmark/pullUp.gcl"

    putStrLn "ParseResult"
    putStrLn (show parseResult)
    putStrLn ""

    -- putStrLn "Do you want to turn on the heuristics?"
    -- heuristics <- getLine
    let heuristics = False

    let (Right program) = parseResult
    let programInput =  (10, uNFOLDLOOP, ifDepth, True, maxDepth)
    loopProgram program programInput 1
    putStrLn "hello"

    -- Poging om loopProgram wat te verkorten.
    --let iets = [(x, uNFOLDLOOP, ifDepth, y) | x <- [2..10], y <- [True, False]]
    --putStrLn $ show iets
    --mapM_ (runProgram program) iets

{-

for every file
    all combinations of parameters
        generate for 2 .. 10

-}

benchmarkFile :: String -> [Int] -> [Int] -> [Int] -> IO ()
benchmarkFile filename loopDepths guardDepths kDepths = do
    (Right program) <- parseGCLfile $ benchmarkDirectory ++ filename ++ benchmarkFileType
    let parameterCombinations = [(k, loop, guard) | guard <- guardDepths, loop <- loopDepths, k <- kDepths]

    putStrLn "combinations"
    putStrLn $ show parameterCombinations

    mapM (runBenchmark program filename) parameterCombinations
    return ()

runBenchmark :: Program -> String -> MetricParams -> IO ()
runBenchmark program caseFileName params = do
    let caseName = caseFileName ++ (metricParamsToString params)
    let filename = metricsDirectory ++ caseName ++ metricsFileType
    BS.writeFile filename $ encode [( "Validity" :: String,
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

    let nInputs = generateNInputs params
    putStrLn $ show (length nInputs)

    putStrLn $ "start validation of " ++ (show params)
    validationResults <- mapM (testProgram program) nInputs

    -- print metrics to csv file
    mapM (appendMetricFile caseName) validationResults
    return ()


metricParamsToString:: MetricParams -> String
metricParamsToString (k, loop, guard) = "_" ++ (show k) ++ "_" ++ (show loop) ++ "_" ++ (show guard)

testProgram :: Program -> ProgramInput -> IO (ProgramInput, (ProgramOutput, TimeSpec))
testProgram program programInput = do
    programResult <- stopWatch (processProgram program programInput)
    printResult (programInput, programResult)
    return (programInput, programResult)

generateNInputs :: MetricParams -> [ProgramInput]
generateNInputs (kDepth, loopDepth, guardDepth) = [ (n, loopDepth, guardDepth, heur, kDepth) |  heur <- [True,False], n <- [2..10]]

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

appendMetricFile :: String ->  (ProgramInput, (ProgramOutput, TimeSpec)) -> IO ()
appendMetricFile caseFilename  (input ,((validationTime, atoms, pathsChecked, infeasibleTime, infeasibleAmount, programValidity), totaltime)) = do
    let filename = metricsDirectory ++ caseFilename ++ metricsFileType

    let (n, loopdepth, ifdepth, heur, k) = input

    BS.appendFile filename $ encode [(show programValidity :: String,
                                      show heur :: String,
                                      loopdepth :: Int,
                                      ifdepth :: Int,
                                      n :: Int,
                                      pathsChecked :: Int,
                                      infeasibleAmount :: Int,
                                      (timeSpectoDouble validationTime) :: Double,
                                      (timeSpectoDouble infeasibleTime) :: Double,
                                      (timeSpectoDouble totaltime) :: Double,
                                      atoms :: Int)]

printResult :: (ProgramInput, (ProgramOutput, TimeSpec)) -> IO ()
printResult (input, ((validationTime, atoms, pathsChecked, infeasibleTime, infeasibleAmount, programValidity), totaltime)) = do
    putStrLn $ show input

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
    putStrLn $ "Total runtime of the program is: " ++ show (sec totaltime)
                     ++ " seconds and " ++ show (nsec totaltime) ++ " nanoseconds."

loopProgram :: Program -> ProgramInput -> Int -> IO ()
loopProgram program input@(2, x, y, False, k) round = do
    runProgram program input round

loopProgram program input@(n, x, y, False,k) round = do
    runProgram program input round
    loopProgram program (n - 1, x, y, False,k) (round + 1)

loopProgram program input@(2, x, y, True,k) round = do
    runProgram program input round
    loopProgram program (10, x, y, False,k) (round + 1)

loopProgram program input@(n, x, y, True,k) round = do
    runProgram program input round
    loopProgram program (n - 1, x, y, True,k) (round + 1)

runProgram :: Program -> ProgramInput -> Int -> IO ()
runProgram program programInput@(n, loopdepth, ifdepth, heur, k) round = do
    tuple@((validationTime, atoms, pathsChecked, infeasibleTime, infeasibleAmount, programValidity), totaltime) <- stopWatch (processProgram program programInput)

    printResult (programInput,tuple)

    --putStrLn "press enter"
    --getLine

    BS.appendFile "../metrics/metrics.csv" $ encode [(round :: Int,
                                                   show programValidity :: String,
                                                   show heur :: String,
                                                   loopdepth :: Int,
                                                   ifdepth :: Int,
                                                   n :: Int,
                                                   pathsChecked :: Int,
                                                   infeasibleAmount :: Int,
                                                   show (timeSpectoDouble validationTime) :: String,
                                                   show (timeSpectoDouble infeasibleTime) :: String,
                                                   show (timeSpectoDouble totaltime) :: String,
                                                   atoms :: Int)]
    putStrLn "END"





