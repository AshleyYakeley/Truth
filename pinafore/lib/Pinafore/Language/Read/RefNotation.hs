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
import Pinafore.Language.TypeSystem
import Shapes

type RefNotation baseupdate = WriterT [(Name, QExpr baseupdate)] (StateT Int (PinaforeScoped baseupdate))

runRefWriterT :: MonadError ErrorMessage m => SourcePos -> WriterT [(Name, QExpr baseupdate)] m a -> m a
runRefWriterT spos wma = do
    (a, w) <- runWriterT wma
    case w of
        [] -> return a
        _ -> throwError $ MkErrorMessage spos NotationBareUnquoteError

liftRefNotation :: PinaforeScoped baseupdate a -> RefNotation baseupdate a
liftRefNotation = lift . lift

remonadRefNotation ::
       WMFunction (PinaforeScoped baseupdate) (PinaforeScoped baseupdate)
    -> (forall a. RefNotation baseupdate a -> RefNotation baseupdate a)
remonadRefNotation (MkWMFunction mm) = remonad $ remonad mm

runRefNotation :: SourcePos -> RefNotation baseupdate a -> PinaforeScoped baseupdate a
runRefNotation spos rexpr = evalStateT (runRefWriterT spos rexpr) 0

type RefExpression baseupdate = RefNotation baseupdate (QExpr baseupdate)

varRefExpr :: SourcePos -> Name -> RefExpression baseupdate
varRefExpr spos name =
    liftRefNotation $ do
        mexpr <- runSourcePos spos $ lookupBinding name
        case mexpr of
            Just expr -> return expr
            Nothing -> return $ qVarExpr name

refNotationUnquote :: SourcePos -> RefExpression baseupdate -> RefExpression baseupdate
refNotationUnquote spos rexpr = do
    i <- lift get
    lift $ put $ i + 1
    let varname = fromString $ "%ref" <> show i
    expr <- lift $ runRefWriterT spos rexpr
    tell $ pure (varname, expr)
    return $ qVarExpr varname

type A = UVar "a"

type B = UVar "b"

purerefExpr :: forall baseupdate. QExpr baseupdate
purerefExpr = qConstExpr (pure :: A -> PinaforeImmutableReference baseupdate A)

aprefExpr :: forall baseupdate. QExpr baseupdate
aprefExpr =
    qConstExpr
        ((<*>) :: PinaforeImmutableReference baseupdate (A -> B) -> PinaforeImmutableReference baseupdate A -> PinaforeImmutableReference baseupdate B)

aplist :: QExpr baseupdate -> [QExpr baseupdate] -> PinaforeSourceScoped baseupdate (QExpr baseupdate)
aplist expr [] = return expr
aplist expr (arg:args) = do
    expr' <- qApplyAllExpr aprefExpr [expr, arg]
    aplist expr' args

refNotationQuote :: SourcePos -> RefExpression baseupdate -> RefExpression baseupdate
refNotationQuote spos rexpr =
    lift $ do
        (expr, binds) <- runWriterT rexpr
        lift $
            runSourcePos spos $ do
                aexpr <- qAbstractsExpr (fmap fst binds) expr
                raexpr <- qApplyExpr purerefExpr aexpr
                aplist raexpr $ fmap snd binds
