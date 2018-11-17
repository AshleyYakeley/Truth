module Pinafore.Language.RefNotation
    ( RefNotation
    , RefExpression
    , liftRefNotation
    , remonadRefNotation
    , runRefNotation
    , refNotationUnquote
    , refNotationQuote
    ) where

import Pinafore.Base
import Pinafore.Language.Expression
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes

type RefNotation baseedit = WriterT [(Name, QExpr baseedit)] (StateT Int PinaforeTypeCheck)

runRefWriterT :: MonadFail m => WriterT [(Name, QExpr baseedit)] m a -> m a
runRefWriterT wma = do
    (a, w) <- runWriterT wma
    case w of
        [] -> return a
        _ -> fail "unquote outside Ref quote"

liftRefNotation :: PinaforeTypeCheck a -> RefNotation baseedit a
liftRefNotation = lift . lift

remonadRefNotation ::
       (forall a. PinaforeTypeCheck a -> PinaforeTypeCheck a)
    -> (forall a. RefNotation baseedit a -> RefNotation baseedit a)
remonadRefNotation mm = remonad $ remonad mm

runRefNotation :: RefNotation baseedit a -> PinaforeTypeCheck a
runRefNotation rexpr = evalStateT (runRefWriterT rexpr) 0

type RefExpression baseedit = RefNotation baseedit (QExpr baseedit)

refNotationUnquote :: RefExpression baseedit -> RefExpression baseedit
refNotationUnquote rexpr = do
    i <- lift get
    lift $ put $ i + 1
    let varname = fromString $ "%ref" <> show i
    expr <- lift $ runRefWriterT rexpr
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

aplist :: QExpr baseedit -> [QExpr baseedit] -> PinaforeTypeCheck (QExpr baseedit)
aplist expr [] = return expr
aplist expr (arg:args) = do
    expr' <- qApplyAllExpr aprefExpr [expr, arg]
    aplist expr' args

refNotationQuote :: RefExpression baseedit -> RefExpression baseedit
refNotationQuote rexpr =
    lift $ do
        (expr, binds) <- runWriterT rexpr
        lift $ do
            aexpr <- qAbstractsExpr (fmap fst binds) expr
            raexpr <- qApplyExpr purerefExpr aexpr
            aplist raexpr $ fmap snd binds
