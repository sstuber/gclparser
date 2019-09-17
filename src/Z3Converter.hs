module Z3Converter where


import GCLParser.GCLDatatype
import Datatypes
import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Z3.Monad

import qualified Data.Map.Strict as M


type ConstMap         = M.Map String AST


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

testEasy = BinopExpr Equal (Var "y") (Var "x")


--evaluateWlp :: Expr -> [VarDeclaration]-> Bool
--evaluateWlp expr varDecls = evalZ3 ()

eval ::  Expr -> [VarDeclaration] -> Z3 ()
eval expr varDecls = do
    varDeclMap <- foldM createZVar M.empty varDecls

    liftIO . putStrLn $ show varDeclMap



createZVar :: ConstMap ->  VarDeclaration-> Z3 ConstMap
createZVar constMap (VarDeclaration name varType)  = do
    constSort <- case varType of
        PType PTInt -> do mkIntSort
        PType PTBool -> do mkBoolSort
        AType PTInt -> do
            intSort <- mkIntSort
            mkArraySort intSort intSort
        AType PTBool -> do
            intSort <- mkIntSort
            boolSort <- mkBoolSort
            mkArraySort intSort boolSort

    finalConstant <- mkFreshConst name constSort
    let updatedMap = M.insert name finalConstant constMap
    return updatedMap


-- Symbol -> Sort -> Const
-- M.Map String Const

