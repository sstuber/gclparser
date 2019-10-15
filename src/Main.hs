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

ifDepth = 30

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

    putStrLn "press enter"
    getLine
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
    BS.appendFile "../metrics/metrics.csv" $ encode [(round :: Int,
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



displayTimeMetrics :: TimeSpec -> IO ()
displayTimeMetrics validationTime = do
    putStrLn "----------------- Time Metrics ------------------"
    putStrLn $ "Runtime on checking validity: " ++ show (sec validationTime) ++ " seconds; " ++ show (nsec validationTime) ++ "  nanoseconds;"




