module Z3Converter where


import GCLParser.GCLDatatype
import Datatypes
import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Z3.Monad

import qualified Data.Map.Strict as M

type ConstMap         = M.Map String AST

--------------------------------------------------------------------------------------------------------
-- main function to use form this file
--------------------------------------------------------------------------------------------------------
areExprValid :: [Expr] -> [VarDeclaration] -> [IO Z3Validation]
areExprValid (expr:t ) vardec = [(isExprValid expr vardec)] ++ (areExprValid t vardec)
areExprValid []        vardec = []


-- unsat -> valid as we negate our expr
isExprValid  :: Expr -> [VarDeclaration] -> IO Z3Validation
isExprValid expr varDecls = do
  result <- evalZ3 (evalExpr expr varDecls)
  return (isValid result)
    where
      isValid res = case res of
          Unsat -> Valid
          Sat   -> UnValid
          Undef -> Z3undef

ioPrint :: String -> Z3 ()
ioPrint = liftIO . putStrLn

isGuardSat:: Expr -> [VarDeclaration] -> IO Result
isGuardSat expr varDecls = do
    result <- evalZ3 (evalGuard expr varDecls)
    return result

evalGuard :: Expr -> [VarDeclaration] -> Z3 Result
evalGuard expr varDecls = do
    varDeclMap   <- foldM createZVar M.empty varDecls
    exprAst      <- convertZ3ToExpr varDeclMap expr
    astString    <- astToString exprAst
    result       <- (assert exprAst >> check)
    return result

evalExpr ::  Expr -> [VarDeclaration] -> Z3 Result
evalExpr expr varDecls = do
    varDeclMap <- foldM createZVar M.empty varDecls

    exprAst <- convertZ3ToExpr varDeclMap expr
    result <- assertExpr exprAst

    return result

-- We negate our expression.
-- If the negation of an expression is unsatisfiable then the original expression is valid
assertExpr :: AST -> Z3 Result
assertExpr ast = mkNot ast >>= assert >> check


createZVar :: ConstMap ->  VarDeclaration-> Z3 ConstMap
createZVar constMap decl@(VarDeclaration name varType)  = do
    constSort <- case varType of
        PType PTInt     -> mkIntSort
        PType PTBool    -> mkBoolSort
        AType PTInt     -> mkIntSort >>= \intSort -> mkArraySort intSort intSort
        AType PTBool    -> do
            intSort   <- mkIntSort
            boolSort  <- mkBoolSort
            mkArraySort intSort boolSort

    midMap <- if isArray decl then
          do
            let lengthName = '#': name
            sort  <- mkIntSort
            var   <- mkFreshConst lengthName sort
            return $ M.insert lengthName var constMap
       else
          return constMap

    finalConstant     <- mkFreshConst name constSort
    let updatedMap    = M.insert name finalConstant midMap
    return updatedMap

isArray :: VarDeclaration -> Bool
isArray (VarDeclaration _ (AType _)) = True
isArray _                               = False

convertZ3ToExpr :: ConstMap -> Expr -> Z3 AST
convertZ3ToExpr constMap (Var a)            = do
    let maybeLookup = M.lookup a constMap
    let finalVar = case maybeLookup of
          Nothing       -> error  ("var name not found -> " ++ a)
          Just z3Var    -> z3Var
    return finalVar
convertZ3ToExpr constMap (SizeOf a )        = do
    let varName = '#' : a
    let maybeLookup = M.lookup varName constMap
    let finalVar = case maybeLookup of
          Nothing       -> error  ("var name not found -> " ++ varName)
          Just z3Var    -> z3Var
    return finalVar
convertZ3ToExpr constMap (LitI x)           = mkIntNum x
convertZ3ToExpr constMap (LitB x)           = mkBool x
convertZ3ToExpr constMap (Parens x)         = convertZ3ToExpr constMap x
convertZ3ToExpr constMap e@(ArrayElem _ _)  = handleArrayElem constMap e
-- Exists
convertZ3ToExpr constMap (OpNeg (Forall var (OpNeg expr))) = do
    varConst            <- mkFreshIntVar var
    let intermediateMap = M.insert var varConst constMap
    varApp              <- toApp varConst
    exprAST             <- convertZ3ToExpr intermediateMap expr
    finalExpr           <- mkExistsConst [] [varApp] exprAST
    return finalExpr

convertZ3ToExpr constMap (OpNeg x)          = convertZ3ToExpr constMap x >>= mkNot
convertZ3ToExpr constMap (BinopExpr op x y) = (convertZ3ToExpr constMap x) >>= \res1 ->  (convertZ3ToExpr constMap y) >>= \res2 -> (z3ByOp op) res1 res2
convertZ3ToExpr constMap (Forall var expr)     = do
    varConst            <- mkFreshIntVar var
    let intermediateMap = M.insert var varConst constMap
    varApp              <- toApp varConst
    exprAST             <- convertZ3ToExpr intermediateMap expr
    finalExpr           <- mkForallConst [] [varApp] exprAST
    return finalExpr

handleArrayElem :: ConstMap -> Expr -> Z3 AST
handleArrayElem constMap (ArrayElem expr1 expr2) = do
    arrayAST <- handleArrayElem constMap expr1
    indexAST <- convertZ3ToExpr constMap expr2
    mkSelect arrayAST indexAST
handleArrayElem constMap (RepBy expr1 expr2 expr3) = do
    arrayAST <- handleArrayElem constMap expr1
    indexAST <- convertZ3ToExpr constMap expr2
    valueAST <- convertZ3ToExpr constMap expr3
    mkStore arrayAST indexAST valueAST
handleArrayElem constMap (Var a) = do
    let maybeLookup = M.lookup a constMap
    let finalVar = case maybeLookup of
          Nothing       -> error  ("var name not found -> " ++ a)
          Just z3Var    -> z3Var
    return finalVar

z3ByOp :: BinOp -> (AST -> AST -> Z3 AST)
z3ByOp And              = \x y -> mkAnd [x,y]
z3ByOp Or               = \x y -> mkOr  [x,y]
z3ByOp Implication      = mkImplies
z3ByOp LessThan         = mkLt
z3ByOp LessThanEqual    = mkLe
z3ByOp GreaterThan      = mkGt
z3ByOp GreaterThanEqual = mkGe
z3ByOp Equal            = mkEq

z3ByOp Minus            = \x y -> mkSub [x,y]
z3ByOp Plus             = \x y -> mkAdd [x,y]
z3ByOp Multiply         = \x y -> mkMul [x,y]
z3ByOp Divide           = mkDiv

