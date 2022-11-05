module Pinafore.Language.Grammar.Interpret.RefNotation
    ( RefNotation
    , RefExpression
    , liftRefNotation
    , hoistRefNotation
    , sourcePosRefNotation
    , runRefNotation
    , refNotationUnquote
    , refNotationQuote
    ) where

import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Language.VarID
import Shapes

type RefNotation = WriterT [(VarID, QExpression)] (StateT VarIDState QInterpreter)

sourcePosRefNotation :: SourcePos -> RefNotation --> RefNotation
sourcePosRefNotation = paramWith $ liftParam $ liftParam sourcePosParam

runRefWriterT :: RefNotation --> StateT VarIDState QInterpreter
runRefWriterT wma = do
    (a, w) <- runWriterT wma
    case w of
        [] -> return a
        _ -> throw NotationBareUnquoteError

liftRefNotation :: QInterpreter --> RefNotation
liftRefNotation = lift . lift

hoistRefNotation :: WRaised QInterpreter QInterpreter -> RefNotation --> RefNotation
hoistRefNotation (MkWRaised mm) = hoist $ hoist mm

runRefNotation :: RefNotation --> QInterpreter
runRefNotation rexpr = evalStateT (runRefWriterT rexpr) firstVarIDState

type RefExpression = RefNotation QExpression

allocateVarRefNotation :: Name -> RefNotation VarID
allocateVarRefNotation name = do
    i <- lift get
    lift $ put $ nextVarIDState i
    return $ mkVarID i name

refNotationUnquote :: RefExpression -> RefExpression
refNotationUnquote rexpr = do
    v <- allocateVarRefNotation "%model"
    expr <- lift $ runRefWriterT rexpr
    tell $ pure (v, expr)
    return $ qVarExpr v

aplist :: QExpression -> [QExpression] -> QInterpreter QExpression
aplist expr [] = return expr
aplist expr (arg:args) = do
    aprefExpr <- qName $ MkFullNameRef RootNamespaceRef "applyWholeModel"
    expr' <- qApplyAllExpr aprefExpr [expr, arg]
    aplist expr' args

refNotationQuote :: RefExpression -> RefExpression
refNotationQuote rexpr =
    lift $ do
        (expr, binds) <- runWriterT rexpr
        lift $ do
            purerefExpr <- qName $ MkFullNameRef RootNamespaceRef "pureWholeModel"
            aexpr <- qAbstractsExpr (fmap fst binds) expr
            raexpr <- qApplyExpr purerefExpr aexpr
            aplist raexpr $ fmap snd binds
