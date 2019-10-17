module Common where

import GCLParser.GCLDatatype

import qualified Data.Map.Strict as M
import System.Clock
import Data.Char
import Control.Monad.Supply
-- name of var to replace -> expression to replace with -> post condition
replaceVar :: String -> Expr -> Expr -> Expr
replaceVar name toReplaceExpr e@(Var name2)                 = if name2 == name then toReplaceExpr else e
replaceVar name toReplaceExpr (LitI int)                    = (LitI int)
replaceVar name toReplaceExpr (LitB bool)                   = (LitB bool)
replaceVar name toReplaceExpr (Parens expr)                 = Parens (replaceVar name toReplaceExpr expr)
replaceVar name toReplaceExpr (ArrayElem array index)       = ArrayElem (replaceVar name toReplaceExpr array)
                                                                        (replaceVar name toReplaceExpr index)
-- Maybe should go into newval with replaceVar?
replaceVar name toReplaceExpr (RepBy arrayname expr newval) = RepBy (replaceVar name toReplaceExpr arrayname)
                                                                    (replaceVar name toReplaceExpr expr)
                                                                    (replaceVar name toReplaceExpr newval)
replaceVar name toReplaceExpr (OpNeg expr)                  = OpNeg (replaceVar name toReplaceExpr expr)
replaceVar name toReplaceExpr (SizeOf name2)                = SizeOf name2
replaceVar name toReplaceExpr (BinopExpr op expr1 expr2)    = BinopExpr op replacedExpr1 replacedExpr2
    where
        replacedExpr1 = replaceVar name toReplaceExpr expr1
        replacedExpr2 = replaceVar name toReplaceExpr expr2
replaceVar name toReplaceExpr (Forall i expr)                             = Forall i (replaceVar name toReplaceExpr expr)


replaceVarWithMap :: M.Map String String -> Expr -> Supply Int Expr
replaceVarWithMap varMap e@(Var name2)                 = case M.lookup name2 varMap of
      Nothing  -> return e
      Just e2  -> return $ Var e2
replaceVarWithMap varMap (LitI int)                    = return (LitI int)
replaceVarWithMap varMap (LitB bool)                   = return (LitB bool)
replaceVarWithMap varMap (Parens expr)                 = replaceVarWithMap varMap expr >>= \x -> return $ Parens x
replaceVarWithMap varMap (ArrayElem array index)       = do
    sArray        <- (replaceVarWithMap varMap array)
    sIndex        <- (replaceVarWithMap varMap index)
    return $ ArrayElem sArray sIndex

replaceVarWithMap varMap (RepBy arrayname expr newval) = do
    sArrayName    <- replaceVarWithMap varMap arrayname
    sExpr         <- replaceVarWithMap varMap expr
    sNewVal       <- replaceVarWithMap varMap newval
    return $ RepBy sArrayName sExpr sNewVal
replaceVarWithMap varMap (OpNeg expr)                  = (replaceVarWithMap varMap expr) >>=  \x -> return $ OpNeg x
replaceVarWithMap varMap e@(SizeOf name2) = case M.lookup name2 varMap of
    Nothing       -> return e
    Just e2       -> return $ SizeOf e2
replaceVarWithMap varMap (BinopExpr op expr1 expr2)    = do
    replacedExpr1  <- replaceVarWithMap varMap expr1
    replacedExpr2  <- replaceVarWithMap varMap expr2
    return $ BinopExpr op replacedExpr1 replacedExpr2
replaceVarWithMap varMap (Forall i expr) = do
    newInt <- supply
    let newName = (show newInt) ++ i
    let newMap = M.insert i newName varMap
    sExpr <- (replaceVarWithMap newMap expr)
    return $ Forall newName sExpr

replaceVarStmt :: String -> Expr -> Stmt -> Stmt
replaceVarStmt name new (Block      d stmt)           = Block d (replaceVarStmt name new stmt)
replaceVarStmt name new (Seq        stmt1 stmt2)      = Seq        (replaceVarStmt name new stmt1) (replaceVarStmt name new stmt2)
replaceVarStmt name new (Assert     expr)             = Assert      (replaceVar name new expr)
replaceVarStmt name new (Assume     expr)             = Assume      (replaceVar name new expr)
replaceVarStmt name new (Assign     n expr )          = Assign     n (replaceVar name new expr)
replaceVarStmt name new (AAssign    n expr1 expr2)    = AAssign    n (replaceVar name new expr1) (replaceVar name new expr2)
replaceVarStmt name new (DrefAssign n expr)           = DrefAssign n (replaceVar name new expr)
replaceVarStmt name new (IfThenElse expr stmt1 stmt2) = IfThenElse (replaceVar name new expr) (replaceVarStmt name new stmt1) (replaceVarStmt name new stmt2)
replaceVarStmt name new (While      expr stmt)        = While (replaceVar name new expr) (replaceVarStmt name new stmt)
replaceVarStmt name new (Skip       )                 = Skip
replaceVarStmt name new s = s


timeSpectoDouble :: TimeSpec -> Double
timeSpectoDouble t = (seconds + nseconds) :: Double
    where
      seconds = (fromIntegral (sec t)) :: Double
      nseconds = ((fromIntegral (nsec t) :: Double ) / 1000000000 :: Double)


fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b

trd3 :: (a, b, c) -> c
trd3 (a, b, c) = c

mapSnd2 :: (a -> b) -> [(c,a)] -> [(c,b)]
mapSnd2 f = map (\(i,x)-> (i, f x))

mapFst2 :: (a-> b) -> [(a,c)] -> [(b,c)]
mapFst2 f = map (\(i,x)-> (f i, x))