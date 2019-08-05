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
import Pinafore.Language.Type
import Shapes

type RefNotation baseedit = WriterT [(Name, QExpr baseedit)] (StateT Int (PinaforeScoped baseedit))

runRefWriterT :: MonadError ErrorMessage m => SourcePos -> WriterT [(Name, QExpr baseedit)] m a -> m a
runRefWriterT spos wma = do
    (a, w) <- runWriterT wma
    case w of
        [] -> return a
        _ -> throwError $ MkErrorMessage spos NotationBareUnquoteError

liftRefNotation :: PinaforeScoped baseedit a -> RefNotation baseedit a
liftRefNotation = lift . lift

remonadRefNotation ::
       Transform (PinaforeScoped baseedit) (PinaforeScoped baseedit)
    -> (forall a. RefNotation baseedit a -> RefNotation baseedit a)
remonadRefNotation (MkTransform mm) = remonad $ remonad mm

runRefNotation :: SourcePos -> RefNotation baseedit a -> PinaforeScoped baseedit a
runRefNotation spos rexpr = evalStateT (runRefWriterT spos rexpr) 0

type RefExpression baseedit = RefNotation baseedit (QExpr baseedit)

varRefExpr :: SourcePos -> Name -> RefExpression baseedit
varRefExpr spos name =
    liftRefNotation $ do
        mexpr <- runSourcePos spos $ lookupBinding name
        case mexpr of
            Just expr -> return expr
            Nothing -> return $ qVarExpr name

refNotationUnquote :: SourcePos -> RefExpression baseedit -> RefExpression baseedit
refNotationUnquote spos rexpr = do
    i <- lift get
    lift $ put $ i + 1
    let varname = fromString $ "%ref" <> show i
    expr <- lift $ runRefWriterT spos rexpr
    tell $ pure (varname, expr)
    return $ qVarExpr varname

type A = UVar "a"

type B = UVar "b"

purerefExpr :: forall baseedit. QExpr baseedit
purerefExpr = qConstExpr (pure :: A -> PinaforeImmutableReference baseedit A)

aprefExpr :: forall baseedit. QExpr baseedit
aprefExpr =
    qConstExpr
        ((<*>) :: PinaforeImmutableReference baseedit (A -> B) -> PinaforeImmutableReference baseedit A -> PinaforeImmutableReference baseedit B)

aplist :: QExpr baseedit -> [QExpr baseedit] -> PinaforeSourceScoped baseedit (QExpr baseedit)
aplist expr [] = return expr
aplist expr (arg:args) = do
    expr' <- qApplyAllExpr aprefExpr [expr, arg]
    aplist expr' args

refNotationQuote :: SourcePos -> RefExpression baseedit -> RefExpression baseedit
refNotationQuote spos rexpr =
    lift $ do
        (expr, binds) <- runWriterT rexpr
        lift $
            runSourcePos spos $ do
                aexpr <- qAbstractsExpr (fmap fst binds) expr
                raexpr <- qApplyExpr purerefExpr aexpr
                aplist raexpr $ fmap snd binds
