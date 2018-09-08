{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Expression where

import Language.Expression.Typed
import Language.Expression.Unitype
import Pinafore.Language.Convert
import Pinafore.Language.Name
import Pinafore.Language.Value
import Pinafore.PredicateMorphism
import Shapes

type TS baseedit = Unitype Identity (QValue baseedit)

instance HasPinaforeEntityEdit baseedit => UnitypeValue (QValue baseedit) where
    applyValue = qapply
    abstractValue = qfunction

type QExpr baseedit = TypedExpression Name (TS baseedit)

qConstExpr ::
       forall baseedit a. ToQValue baseedit a
    => a
    -> QExpr baseedit
qConstExpr a = constTypedExpression @(TS baseedit) Refl $ toQValue a

qVarExpr ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Name
    -> QExpr baseedit
qVarExpr name = varTypedExpression @(TS baseedit) name

qAbstractExpr ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Name
    -> QExpr baseedit
    -> QExpr baseedit
qAbstractExpr name expr = runIdentity $ abstractTypedExpression @(TS baseedit) name expr

qAbstractsExpr :: HasPinaforeEntityEdit baseedit => [Name] -> QExpr baseedit -> QExpr baseedit
qAbstractsExpr [] = id
qAbstractsExpr (n:nn) = qAbstractExpr n . qAbstractsExpr nn

type QBindings baseedit = TypedBindings Name (TS baseedit)

qBindVal :: (HasPinaforeEntityEdit baseedit, ToQValue baseedit t) => Name -> t -> QBindings baseedit
qBindVal name val = qBindExpr name $ qConstExpr val

qApplyExpr ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => QExpr baseedit
    -> QExpr baseedit
    -> QExpr baseedit
qApplyExpr exprf expra = runIdentity $ applyTypedExpression @(TS baseedit) exprf expra

qApplyAllExpr :: HasPinaforeEntityEdit baseedit => QExpr baseedit -> [QExpr baseedit] -> QExpr baseedit
qApplyAllExpr e [] = e
qApplyAllExpr e (a:aa) = qApplyAllExpr (qApplyExpr e a) aa

qSequenceExpr :: [QExpr baseedit] -> QExpr baseedit
qSequenceExpr = osequenceA $ MkAny QTList

qBindExpr ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Name
    -> QExpr baseedit
    -> QBindings baseedit
qBindExpr = singleTypedBinding @(TS baseedit)

qLetExpr ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Name
    -> QExpr baseedit
    -> QExpr baseedit
    -> QExpr baseedit
qLetExpr name exp body = runIdentity $ letTypedExpression @(TS baseedit) name exp body

qBindingsLetExpr ::
       forall baseedit m. (MonadFail m, HasPinaforeEntityEdit baseedit)
    => QBindings baseedit
    -> m (QExpr baseedit -> QExpr baseedit)
qBindingsLetExpr bindings = do
    typedBindingsCheckDuplicates @(TS baseedit) bindings
    return $ qUncheckedBindingsLetExpr bindings

qUncheckedBindingsLetExpr ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => QBindings baseedit
    -> QExpr baseedit
    -> QExpr baseedit
qUncheckedBindingsLetExpr b e = runIdentity $ uncheckedBindingsLetTypedExpression @(TS baseedit) b e

qEvalExpr ::
       forall baseedit m. MonadFail m
    => QExpr baseedit
    -> m (QValue baseedit)
qEvalExpr expr = do
    MkAny Refl val <- evalTypedExpression @(TS baseedit) expr
    return val
