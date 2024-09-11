module Pinafore.Language.Interpret.AppNotation
    ( appNotationUnquote
    , appNotationQuote
    ) where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpret.Value
import Pinafore.Language.Interpreter
import Pinafore.Language.Type
import Pinafore.Language.VarID

allocateAppNotationVar :: QInterpreter VarID
allocateAppNotationVar = do
    i <- refSucc appNotationVarRef
    return $ mkAppNotationVarID i

appNotationUnquote :: QInterpreter QExpression -> QInterpreter QExpression
appNotationUnquote mexpr = do
    v <- allocateAppNotationVar
    (expr, uq) <- prodCollect appNotationBindsProd mexpr
    case uq of
        [] -> return ()
        _ -> throw NotationBareUnquoteError
    prodTellItem appNotationBindsProd (v, expr)
    return $ qVar v

aplist :: QExpression -> [QExpression] -> QInterpreter QExpression
aplist expr [] = return expr
aplist expr (arg:args) = do
    aprefExpr <- interpretValue "apply" Nothing
    expr' <- qApplyAllExpr aprefExpr [expr, arg]
    aplist expr' args

appNotationQuote :: QInterpreter QExpression -> QInterpreter QExpression
appNotationQuote mexpr = do
    (expr, binds) <- prodCollect appNotationBindsProd mexpr
    purerefExpr <- interpretValue "pure" Nothing
    aexpr <- qAbstractsExpr (fmap fst binds) expr
    raexpr <- qApplyExpr purerefExpr aexpr
    aplist raexpr $ fmap snd binds
