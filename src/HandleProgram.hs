module HandleProgram where

import BranchSplit
import GCLParser.GCLDatatype
import Datatypes
import PreProcessing
import qualified Data.Map.Strict as M
import Z3Converter
import System.Clock
import Control.StopWatch
import Common
import Control.Monad.Supply


---------------------------- MAIN FUNCTIONS -----------------------------------------

processProgram :: Program -> ProgramInput -> IO ProgramOutput
processProgram program (n, loopDepth, ifDepthLocal, heuristic, depthK)  = do
    -- preprocess program
    (stmts, (Just pre), (Just post), varDecls) <- preProcessProgram program n

    -- every path ends with the precondition
    let branchRoot = [(depthK ,[(Assume pre)])]
    -- get all the feasible branches
    -- Kunnen we dit niet uitzetten als we de heuristieken uit hebben staan?
    (testDepth, infeasibleAmount, infeasibleTime, programPaths) <- analyseTree varDecls branchRoot stmts loopDepth ifDepthLocal heuristic


    --putStrLn "path depth ?"
    --putStrLn $ show (length programPaths)
    --mapM (\x -> putStrLn $ show (depthK - (fst x ))) programPaths
    let test = map snd programPaths
    --putStrLn "testDepth"
    --putStrLn $ show testDepth
    -- validate all feasible paths
    (pathDataList, validationTime) <- stopWatch (checkValidityOfProgram post test varDecls)

    putStrLn "amount of generated paths"
    putStrLn $ show (length programPaths)

    atoms <- getAtomSize post (map snd programPaths)
    let programValidity = isProgramValid $ reverse pathDataList

    return (validationTime, atoms, length pathDataList, infeasibleTime, infeasibleAmount, programValidity)

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


--------------------------------------------- SUPPORT FUNCTIONS ---------------------------------

replaceNbyIntTree :: Int -> Stmt  -> Stmt
replaceNbyIntTree i = replaceVarStmt "N" (LitI i)

getAtomSize :: PostCon -> [ProgramPath] -> IO Int
getAtomSize post path = do
    let wlp = map (foldr generateWlp post) path
    let atoms = foldr (+) 0 (map countAtoms wlp)
    return atoms

isProgramValid :: [(Bool, ProgramPath)] -> Bool
isProgramValid [] = True
isProgramValid ((b,p): xs) = b




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


