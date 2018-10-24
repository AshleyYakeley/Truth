{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Expression
    ( module Pinafore.Language.Expression
    , PinaforeTypeCheck
    ) where

import Language.Expression.Bindings
import Language.Expression.Dolan
import Language.Expression.Typed
import Pinafore.Language.Convert
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes

type QExpr baseedit = TypedExpression Name (PinaforeTypeSystem baseedit)

qConstExprAny :: forall baseedit. AnyValue (PinaforeType baseedit 'PositivePolarity) -> QExpr baseedit
qConstExprAny = constTypedExpression @(PinaforeTypeSystem baseedit)

qConstExpr ::
       forall baseedit a. ToPinaforeType baseedit a
    => a
    -> QExpr baseedit
qConstExpr a = qConstExprAny $ toValue a

qVarExpr :: forall baseedit. Name -> QExpr baseedit
qVarExpr name = varTypedExpression @(PinaforeTypeSystem baseedit) name

qAbstractExpr :: forall baseedit. Name -> QExpr baseedit -> PinaforeTypeCheck (QExpr baseedit)
qAbstractExpr name expr = abstractTypedExpression @(PinaforeTypeSystem baseedit) name expr

qAbstractsExpr :: [Name] -> QExpr baseedit -> PinaforeTypeCheck (QExpr baseedit)
qAbstractsExpr [] e = return e
qAbstractsExpr (n:nn) e = do
    e' <- qAbstractsExpr nn e
    qAbstractExpr n e'

qApplyExpr :: forall baseedit. QExpr baseedit -> QExpr baseedit -> PinaforeTypeCheck (QExpr baseedit)
qApplyExpr exprf expra = applyTypedExpression @(PinaforeTypeSystem baseedit) exprf expra

qApplyAllExpr :: QExpr baseedit -> [QExpr baseedit] -> PinaforeTypeCheck (QExpr baseedit)
qApplyAllExpr e [] = return e
qApplyAllExpr e (a:aa) = do
    e' <- qApplyExpr e a
    qApplyAllExpr e' aa

qEmptyList :: QExpr baseedit
qEmptyList = qConstExpr ([] :: [UVar "a"])

qConsList :: QExpr baseedit
qConsList = qConstExpr ((:) :: UVar "a" -> [UVar "a"] -> [UVar "a"])

qSequenceExpr :: [QExpr baseedit] -> PinaforeTypeCheck (QExpr baseedit)
qSequenceExpr [] = return $ qEmptyList
qSequenceExpr (e:ee) = do
    ee' <- qSequenceExpr ee
    qApplyAllExpr qConsList [e, ee']

type QBindList baseedit = [(Name, PinaforeTypeCheck (QExpr baseedit))]

bindListToBindings ::
       forall baseedit. QBindList baseedit -> PinaforeTypeCheck (TypedBindings Name (PinaforeTypeSystem baseedit))
bindListToBindings bl =
    fmap mconcat $
    for bl $ \(n, me) -> do
        e <- me
        return $ singleTypedBinding @(PinaforeTypeSystem baseedit) n e

qBindExpr :: forall baseedit. Name -> PinaforeTypeCheck (QExpr baseedit) -> QBindList baseedit
qBindExpr n e = pure (n, e)

qBindVal :: ToPinaforeType baseedit t => Name -> t -> QBindList baseedit
qBindVal name val = qBindExpr name $ return $ qConstExpr val

qLetExpr :: forall baseedit. Name -> QExpr baseedit -> QExpr baseedit -> PinaforeTypeCheck (QExpr baseedit)
qLetExpr name exp body = letTypedExpression @(PinaforeTypeSystem baseedit) name exp body

qBindingsLetExpr ::
       forall baseedit m. MonadFail m
    => QBindList baseedit
    -> m (QExpr baseedit -> PinaforeTypeCheck (QExpr baseedit))
qBindingsLetExpr bl = do
    checkDuplicates $ fmap fst bl
    return $ \e -> qUncheckedBindingsLetExpr bl e

qUncheckedBindingsLetExpr :: forall baseedit. QBindList baseedit -> QExpr baseedit -> PinaforeTypeCheck (QExpr baseedit)
qUncheckedBindingsLetExpr bl e = do
    bindings <- bindListToBindings bl
    uncheckedBindingsLetTypedExpression @(PinaforeTypeSystem baseedit) bindings e

qEvalExpr ::
       forall baseedit m. MonadFail m
    => QExpr baseedit
    -> m (AnyValue (PinaforeType baseedit 'PositivePolarity))
qEvalExpr expr = evalTypedExpression @(PinaforeTypeSystem baseedit) expr

typedAnyToPinaforeVal ::
       forall baseedit t. FromPinaforeType baseedit t
    => AnyValue (PinaforeType baseedit 'PositivePolarity)
    -> Result Text t
typedAnyToPinaforeVal aval =
    case fromTypeF of
        MkTypeF wit conv -> runPinaforeTypeCheck $ fmap conv $ typedAnyToVal @(PinaforeTypeSystem baseedit) wit aval
