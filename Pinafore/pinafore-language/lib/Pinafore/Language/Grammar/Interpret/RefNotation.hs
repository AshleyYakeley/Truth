module Pinafore.Language.Grammar.Interpret.RefNotation
    ( RefNotation
    , RefExpression
    , varRefExpr
    , liftRefNotation
    , hoistRefNotation
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

runRefWriterT :: MonadThrow ErrorMessage m => SourcePos -> MFunction (WriterT [(VarID, QExpr)] m) m
runRefWriterT spos wma = do
    (a, w) <- runWriterT wma
    case w of
        [] -> return a
        _ -> throwErrorType spos NotationBareUnquoteError

liftRefNotation :: MFunction PinaforeInterpreter RefNotation
liftRefNotation = lift . lift

hoistRefNotation :: WMFunction PinaforeInterpreter PinaforeInterpreter -> MFunction RefNotation RefNotation
hoistRefNotation (MkWMFunction mm) = hoist $ hoist mm

runRefNotation :: MFunction RefNotation PinaforeSourceInterpreter
runRefNotation rexpr = do
    spos <- askSourcePos
    liftSourcePos $ evalStateT (runRefWriterT spos rexpr) firstVarIDState

type RefExpression = RefNotation QExpr

allocateVarRefNotation :: Name -> RefNotation VarID
allocateVarRefNotation name = do
    i <- lift get
    lift $ put $ nextVarIDState i
    return $ mkVarID i name

varRefExpr :: SourcePos -> ReferenceName -> RefExpression
varRefExpr spos name = do
    mexpr <- liftRefNotation $ runSourcePos spos $ lookupLetBinding name
    case mexpr of
        Just (Right expr) -> return expr
        Just (Left v) -> return $ qVarExpr v
        Nothing -> return $ qVarExpr $ mkBadVarID spos name

refNotationUnquote :: SourcePos -> RefExpression -> RefExpression
refNotationUnquote spos rexpr = do
    v <- allocateVarRefNotation "%ref"
    expr <- lift $ runRefWriterT spos rexpr
    tell $ pure (v, expr)
    return $ qVarExpr v

purerefExpr :: QExpr
purerefExpr = qConstExpr (pure :: A -> PinaforeImmutableWholeRef A)

aprefExpr :: QExpr
aprefExpr =
    qConstExpr
        ((<*>) :: PinaforeImmutableWholeRef (A -> B) -> PinaforeImmutableWholeRef A -> PinaforeImmutableWholeRef B)

aplist :: QExpr -> [QExpr] -> PinaforeSourceInterpreter QExpr
aplist expr [] = return expr
aplist expr (arg:args) = do
    expr' <- qApplyAllExpr aprefExpr [expr, arg]
    aplist expr' args

refNotationQuote :: SourcePos -> RefExpression -> RefExpression
refNotationQuote spos rexpr =
    lift $ do
        (expr, binds) <- runWriterT rexpr
        lift $
            runSourcePos spos $ do
                aexpr <- qAbstractsExpr (fmap fst binds) expr
                raexpr <- qApplyExpr purerefExpr aexpr
                aplist raexpr $ fmap snd binds
