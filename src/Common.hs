module Common where

import GCLParser.GCLDatatype

import qualified Data.Map.Strict as M
import System.Clock

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


replaceVarWithMap :: M.Map String String -> Expr -> Expr
replaceVarWithMap varMap e@(Var name2)                 = case M.lookup name2 varMap of
      Nothing  -> e
      Just e2  -> Var e2
replaceVarWithMap varMap (LitI int)                    = (LitI int)
replaceVarWithMap varMap (LitB bool)                   = (LitB bool)
replaceVarWithMap varMap (Parens expr)                 = Parens (replaceVarWithMap varMap expr)
replaceVarWithMap varMap (ArrayElem array index)       = ArrayElem (replaceVarWithMap varMap array)
                                                                        (replaceVarWithMap varMap index)
-- Maybe should go into newval with replaceVarWithMap?
replaceVarWithMap varMap (RepBy arrayname expr newval) = RepBy (replaceVarWithMap varMap arrayname)
                                                                (replaceVarWithMap varMap expr)
                                                                (replaceVarWithMap varMap newval)
replaceVarWithMap varMap (OpNeg expr)                  = OpNeg (replaceVarWithMap varMap expr)
replaceVarWithMap varMap (SizeOf name2)                = SizeOf name2
replaceVarWithMap varMap (BinopExpr op expr1 expr2)    = BinopExpr op replacedExpr1 replacedExpr2
    where
        replacedExpr1 = replaceVarWithMap varMap expr1
        replacedExpr2 = replaceVarWithMap varMap expr2
replaceVarWithMap varMap (Forall i expr)                             = Forall i (replaceVarWithMap varMap expr)

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b

trd3 :: (a, b, c) -> c
trd3 (a, b, c) = c

