module Pinafore.Language.Grammar.Interpret.RefNotation
    ( RefNotation
    , RefExpression
    , varRefExpr
    , liftRefNotation
    , hoistRefNotation
    , sourcePosRefNotation
    , runRefNotation
    , refNotationUnquote
    , refNotationQuote
    ) where

import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Language.Var
import Pinafore.Language.VarID
import Shapes

type RefNotation = WriterT [(VarID, QExpr)] (StateT VarIDState PinaforeInterpreter)

sourcePosRefNotation :: SourcePos -> RefNotation --> RefNotation
sourcePosRefNotation = paramWith $ liftParam $ liftParam sourcePosParam

runRefWriterT :: RefNotation --> StateT VarIDState PinaforeInterpreter
runRefWriterT wma = do
    (a, w) <- runWriterT wma
    case w of
        [] -> return a
        _ -> throw NotationBareUnquoteError

liftRefNotation :: PinaforeInterpreter --> RefNotation
liftRefNotation = lift . lift

hoistRefNotation :: WMFunction PinaforeInterpreter PinaforeInterpreter -> RefNotation --> RefNotation
hoistRefNotation (MkWMFunction mm) = hoist $ hoist mm

runRefNotation :: RefNotation --> PinaforeInterpreter
runRefNotation rexpr = evalStateT (runRefWriterT rexpr) firstVarIDState

type RefExpression = RefNotation QExpr

allocateVarRefNotation :: Name -> RefNotation VarID
allocateVarRefNotation name = do
    i <- lift get
    lift $ put $ nextVarIDState i
    return $ mkVarID i name

varRefExpr :: ReferenceName -> RefExpression
varRefExpr name = do
    mexpr <- liftRefNotation $ lookupLetBinding name
    case mexpr of
        Just (Right expr) -> return expr
        Just (Left v) -> return $ qVarExpr v
        Nothing -> do
            spos <- liftRefNotation $ paramAsk sourcePosParam
            return $ qVarExpr $ mkBadVarID spos name

refNotationUnquote :: RefExpression -> RefExpression
refNotationUnquote rexpr = do
    v <- allocateVarRefNotation "%ref"
    expr <- lift $ runRefWriterT rexpr
    tell $ pure (v, expr)
    return $ qVarExpr v

purerefExpr :: QExpr
purerefExpr = qConstExpr (pure :: A -> PinaforeImmutableWholeRef A)

aprefExpr :: QExpr
aprefExpr =
    qConstExpr
        ((<*>) :: PinaforeImmutableWholeRef (A -> B) -> PinaforeImmutableWholeRef A -> PinaforeImmutableWholeRef B)

aplist :: QExpr -> [QExpr] -> PinaforeInterpreter QExpr
aplist expr [] = return expr
aplist expr (arg:args) = do
    expr' <- qApplyAllExpr aprefExpr [expr, arg]
    aplist expr' args

refNotationQuote :: RefExpression -> RefExpression
refNotationQuote rexpr =
    lift $ do
        (expr, binds) <- runWriterT rexpr
        lift $ do
            aexpr <- qAbstractsExpr (fmap fst binds) expr
            raexpr <- qApplyExpr purerefExpr aexpr
            aplist raexpr $ fmap snd binds
