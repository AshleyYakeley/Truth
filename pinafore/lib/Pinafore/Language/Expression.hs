{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Expression
    ( module Pinafore.Language.Expression
    , PinaforeScoped
    ) where

import Language.Expression.Bindings
import Language.Expression.Dolan
import Language.Expression.TypeSystem
import Pinafore.Language.Convert
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes

type QExpr baseedit = TSSealedExpression (PinaforeTypeSystem baseedit)

type QValue baseedit = TSValue (PinaforeTypeSystem baseedit)

qConstExprAny :: forall baseedit. QValue baseedit -> QExpr baseedit
qConstExprAny = tsConst @(PinaforeTypeSystem baseedit)

qConstExpr ::
       forall baseedit a. ToPinaforeType baseedit a
    => a
    -> QExpr baseedit
qConstExpr a = qConstExprAny $ toValue a

qVarExpr :: forall baseedit. Name -> QExpr baseedit
qVarExpr name = tsVar @(PinaforeTypeSystem baseedit) name

qAbstractExpr :: forall baseedit. Name -> QExpr baseedit -> PinaforeSourceScoped baseedit (QExpr baseedit)
qAbstractExpr name expr = tsAbstract @(PinaforeTypeSystem baseedit) name expr

qAbstractsExpr :: [Name] -> QExpr baseedit -> PinaforeSourceScoped baseedit (QExpr baseedit)
qAbstractsExpr [] e = return e
qAbstractsExpr (n:nn) e = do
    e' <- qAbstractsExpr nn e
    qAbstractExpr n e'

qApplyExpr :: forall baseedit. QExpr baseedit -> QExpr baseedit -> PinaforeSourceScoped baseedit (QExpr baseedit)
qApplyExpr exprf expra = tsApply @(PinaforeTypeSystem baseedit) exprf expra

qApplyAllExpr :: QExpr baseedit -> [QExpr baseedit] -> PinaforeSourceScoped baseedit (QExpr baseedit)
qApplyAllExpr e [] = return e
qApplyAllExpr e (a:aa) = do
    e' <- qApplyExpr e a
    qApplyAllExpr e' aa

qEmptyList :: QExpr baseedit
qEmptyList = qConstExpr ([] :: [UVar "a"])

qConsList :: QExpr baseedit
qConsList = qConstExpr ((:) :: UVar "a" -> [UVar "a"] -> [UVar "a"])

qSequenceExpr :: [QExpr baseedit] -> PinaforeSourceScoped baseedit (QExpr baseedit)
qSequenceExpr [] = return $ qEmptyList
qSequenceExpr (e:ee) = do
    ee' <- qSequenceExpr ee
    qApplyAllExpr qConsList [e, ee']

type QBindings baseedit = TSBindings (PinaforeTypeSystem baseedit)

qBindExpr :: forall baseedit. Name -> QExpr baseedit -> QBindings baseedit
qBindExpr = singleBinding

qBindVal :: ToPinaforeType baseedit t => Name -> t -> QBindings baseedit
qBindVal name val = qBindExpr name $ qConstExpr val

qLetExpr :: forall baseedit. Name -> QExpr baseedit -> QExpr baseedit -> PinaforeSourceScoped baseedit (QExpr baseedit)
qLetExpr name exp body = tsLet @(PinaforeTypeSystem baseedit) name exp body

qBindingsLetExpr ::
       forall baseedit m. MonadFail m
    => QBindings baseedit
    -> m (QExpr baseedit -> PinaforeSourceScoped baseedit (QExpr baseedit))
qBindingsLetExpr b = do
    tsBindingsCheckDuplicates @(PinaforeTypeSystem baseedit) b
    return $ \e -> qUncheckedBindingsLetExpr b e

qUncheckedBindingsLetExpr ::
       forall baseedit. QBindings baseedit -> QExpr baseedit -> PinaforeSourceScoped baseedit (QExpr baseedit)
qUncheckedBindingsLetExpr = tsUncheckedLet @(PinaforeTypeSystem baseedit)

qValuesLetExpr ::
       forall baseedit.
       (Name -> Maybe (QValue baseedit))
    -> QExpr baseedit
    -> PinaforeSourceScoped baseedit (QExpr baseedit)
qValuesLetExpr = tsValuesLet @(PinaforeTypeSystem baseedit)

qEvalExpr ::
       forall baseedit m. MonadFail m
    => QExpr baseedit
    -> m (QValue baseedit)
qEvalExpr expr = tsEval @(PinaforeTypeSystem baseedit) expr

typedAnyToPinaforeVal ::
       forall baseedit t. FromPinaforeType baseedit t
    => SourcePos
    -> QValue baseedit
    -> Result Text t
typedAnyToPinaforeVal spos aval =
    case fromTypeF of
        MkTypeF wit conv -> runSourceScoped spos $ fmap conv $ tsAnyToVal @(PinaforeTypeSystem baseedit) wit aval
