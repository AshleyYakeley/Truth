module Pinafore.Language.Interpret.AppNotation
    ( appNotationUnquote
    , appNotationQuote
    ) where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpreter
import Pinafore.Language.Type
import Pinafore.Language.VarID

allocateAppNotationVar :: FullName -> QInterpreter VarID
allocateAppNotationVar name = do
    i <- refSucc appNotationVarRef
    return $ mkVarID i name

appNotationUnquote :: QInterpreter QExpression -> QInterpreter QExpression
appNotationUnquote mexpr = do
    v <- allocateAppNotationVar $ RootFullName "%model"
    (expr, uq) <- prodCollect appNotationBindsProd mexpr
    case uq of
        [] -> return ()
        _ -> throw NotationBareUnquoteError
    prodTellItem appNotationBindsProd (v, expr)
    return $ qVarExpr v

aplist :: QExpression -> [QExpression] -> QInterpreter QExpression
aplist expr [] = return expr
aplist expr (arg:args) = do
    aprefExpr <- qName "ap"
    expr' <- qApplyAllExpr aprefExpr [expr, arg]
    aplist expr' args

appNotationQuote :: QInterpreter QExpression -> QInterpreter QExpression
appNotationQuote mexpr = do
    (expr, binds) <- prodCollect appNotationBindsProd mexpr
    purerefExpr <- qName "pure"
    aexpr <- qAbstractsExpr (fmap fst binds) expr
    raexpr <- qApplyExpr purerefExpr aexpr
    aplist raexpr $ fmap snd binds
