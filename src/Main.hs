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
uNFOLDLOOP = 50

maxDepth :: Int
maxDepth = 50

ifDepth = 10

metricsDirectory = "../metrics/"
metricsFileType = ".csv"
benchmarkDirectory = "../examples/benchmark/"
benchmarkFileType = ".gcl"
-- loop guard k
benchmarkFiletest = benchmarkFile "divByN" [5] [5] [50]

-- run a benchmark test on a file from the metrics/benchmark folder
benchmarkTest x = benchmarkFile x  [10,20] [5,10,15] [30,50]


main :: IO ()
main = do
    putStrLn "validating memberof"
    benchmarkTest "memberOf"

    putStrLn "validating pullUp"
    benchmarkTest "pullUp"

    putStrLn "validating divByN"
    benchmarkTest "divByN"

{-

for every file
    all combinations of parameters
        generate for 2 .. 10

-}

-- run all possible combinations of the metric input parameters on the benchmark file
benchmarkFile :: String -> [Int] -> [Int] -> [Int] -> IO ()
benchmarkFile filename loopDepths guardDepths kDepths = do
    (Right program) <- parseGCLfile $ benchmarkDirectory ++ filename ++ benchmarkFileType
    let parameterCombinations = [(k, loop, guard) | guard <- guardDepths, loop <- loopDepths, k <- kDepths]

    putStrLn "combinations"
    putStrLn $ show parameterCombinations

    mapM (runBenchmark program filename) parameterCombinations
    return ()

-- run a set of metric parameters for all possible combinations of N and heuristics
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

-- run a set of program inputs and generate output
testProgram :: Program -> ProgramInput -> IO (ProgramInput, (ProgramOutput, TimeSpec))
testProgram program programInput = do
    programResult <- stopWatch (processProgram program programInput)
    printResult (programInput, programResult)
    return (programInput, programResult)

-- from a set of metric parameters, generate a list of program inputs over N [2..10] and heuristics [True,False]
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

-- print a program output onto the specific csv file
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
    putStrLn $ show (timeSpectoDouble validationTime)

    putStrLn "------------- TOTAL ATOMS -----------------"
    putStrLn $ show atoms

    putStrLn "------------- PATHS INFEASIBLE ------------"
    putStrLn $ show infeasibleAmount
    putStrLn "------------- PATHS VALIDATED -------------"
    putStrLn $ show pathsChecked

    putStrLn "------------- EXTRA TIME FEASIBILITY ------"
    putStrLn $ show (timeSpectoDouble infeasibleTime)

    putStrLn "------------- PROGRAM VALID ----------------"
    putStrLn $ show programValidity

    putStrLn "-------------- TOTALTIME ------------------"
    putStrLn $ "Total runtime of the program is: " ++ show (sec totaltime)
                     ++ " seconds and " ++ show (nsec totaltime) ++ " nanoseconds."





