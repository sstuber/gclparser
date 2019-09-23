module Common where

import GCLParser.GCLDatatype

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
