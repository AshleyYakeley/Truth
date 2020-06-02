module Pinafore.Language.Read.RefNotation
    ( RefNotation
    , RefExpression
    , varRefExpr
    , liftRefNotation
    , remonadRefNotation
    , runRefNotation
    , refNotationUnquote
    , refNotationQuote
    ) where

import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Name
import Pinafore.Language.Scope
import Pinafore.Language.Type
import Shapes

type RefNotation = WriterT [(Name, QExpr)] (StateT Int PinaforeScoped)

runRefWriterT :: MonadThrow ErrorMessage m => SourcePos -> WriterT [(Name, QExpr)] m a -> m a
runRefWriterT spos wma = do
    (a, w) <- runWriterT wma
    case w of
        [] -> return a
        _ -> throw $ MkErrorMessage spos NotationBareUnquoteError

liftRefNotation :: PinaforeScoped a -> RefNotation a
liftRefNotation = lift . lift

remonadRefNotation :: WMFunction PinaforeScoped PinaforeScoped -> (forall a. RefNotation a -> RefNotation a)
remonadRefNotation (MkWMFunction mm) = remonad $ remonad mm

runRefNotation :: SourcePos -> RefNotation a -> PinaforeScoped a
runRefNotation spos rexpr = evalStateT (runRefWriterT spos rexpr) 0

type RefExpression = RefNotation QExpr

varRefExpr :: SourcePos -> Name -> RefExpression
varRefExpr spos name =
    liftRefNotation $ do
        mexpr <- runSourcePos spos $ lookupBinding name
        case mexpr of
            Just expr -> return expr
            Nothing -> return $ qVarExpr name

refNotationUnquote :: SourcePos -> RefExpression -> RefExpression
refNotationUnquote spos rexpr = do
    i <- lift get
    lift $ put $ i + 1
    let varname = fromString $ "%ref" <> show i
    expr <- lift $ runRefWriterT spos rexpr
    tell $ pure (varname, expr)
    return $ qVarExpr varname

type A = UVar "a"

type B = UVar "b"

purerefExpr :: QExpr
purerefExpr = qConstExpr (pure :: A -> PinaforeImmutableRef A)

aprefExpr :: QExpr
aprefExpr = qConstExpr ((<*>) :: PinaforeImmutableRef (A -> B) -> PinaforeImmutableRef A -> PinaforeImmutableRef B)

aplist :: QExpr -> [QExpr] -> PinaforeSourceScoped QExpr
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
