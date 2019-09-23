module Z3Converter where


import GCLParser.GCLDatatype
import Datatypes
import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Z3.Monad

import qualified Data.Map.Strict as M


type ConstMap         = M.Map String AST

testHard :: Expr
testHard =  BinopExpr
          Implication
          (BinopExpr LessThan (Var "y") (Var "x"))
          (BinopExpr
            And
              (BinopExpr
                And
                (Parens (BinopExpr
                            Or
                            (Parens (BinopExpr Equal (Var "y") (Var "x")))
                            (Parens (BinopExpr Equal (Var "y") (Var "y")))))
                (Parens (BinopExpr
                            LessThanEqual
                            (Var "y")
                            (Var "x"))))
              (Parens (BinopExpr LessThanEqual (Var "y") (Var "y"))))
testEasy :: Expr
testEasy = BinopExpr Equal (Var "y") (Var "x")

testVars :: [VarDeclaration]
testVars = [
    VarDeclaration "x" (PType PTInt),
    VarDeclaration "y" (PType PTInt)
  ]

--------------------------------------------------------------------------------------------------------
-- main function to use form this file
--------------------------------------------------------------------------------------------------------

-- unsat -> valid as we negate our expr
isExprValid  :: Expr -> [VarDeclaration] -> IO (Either Bool String)
isExprValid expr varDecls = do
  result <- evalZ3 (evalExpr expr varDecls)
  return (isValid result)
    where
      isValid res = case res of
          Unsat -> Left True
          Sat   -> Left False
          Undef -> Right "expression could not be validated"


ioPrint :: String -> Z3 ()
ioPrint = liftIO . putStrLn
--evaluateWlp :: Expr -> [VarDeclaration]-> Bool
--evaluateWlp expr varDecls = evalZ3 ()



evalExpr ::  Expr -> [VarDeclaration] -> Z3 Result
evalExpr expr varDecls = do
    varDeclMap <- foldM createZVar M.empty varDecls

    ioPrint "var map"
    let declAsts = M.elems varDeclMap
    printresult <- forM declAsts (\decl -> do
          declStr <- astToString decl
          ioPrint declStr
      )

    exprAst <- convertZ3ToExpr varDeclMap expr
    astString <- astToString exprAst
    ioPrint astString
    -- result <- (assert exprAst >> check)
    -- ioPrint $ show result
    result <- assertExpr exprAst
    ioPrint $ show result

    return result

-- We negate our expression.
-- If the negation of an expression is unsatisfiable then the original expression is valid
-- TODO put this into the report
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
            sort <- mkIntSort
            var <- mkFreshConst lengthName sort
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
-- TODO this is not save yet
    let (Just lookupConst) =  M.lookup a constMap
    return lookupConst
convertZ3ToExpr constMap (LitI x)           = (mkIntSymbol x) >>= mkIntVar
convertZ3ToExpr constMap (LitB x)           = mkBool x
convertZ3ToExpr constMap (Parens x)         = convertZ3ToExpr constMap x
-- TODO fix arrays
--convertZ3ToExpr constMap (ArrayElem x y) =
convertZ3ToExpr constMap (OpNeg x)          = convertZ3ToExpr constMap x >>= mkNot
convertZ3ToExpr constMap (BinopExpr op x y) = (convertZ3ToExpr constMap x) >>= \res1 ->  (convertZ3ToExpr constMap y) >>= \res2 -> (z3ByOp op) res1 res2


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

{-
data Expr
    = Var                String
    | LitI               Int
    | LitB               Bool
    | LitNull
    | Parens             Expr
    | ArrayElem          Expr Expr
    | OpNeg              Expr
    | BinopExpr          BinOp  Expr   Expr
    | Forall             String Expr
    | SizeOf             String
    | RepBy              Expr   Expr   Expr
    | Cond               Expr   Expr   Expr
    | NewStore           Expr
    | Dereference        String
    deriving (Eq)
-}
