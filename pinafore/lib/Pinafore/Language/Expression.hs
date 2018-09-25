{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Expression where

import Language.Expression.Bindings
import Language.Expression.Typed
import Language.Expression.Unitype
import Pinafore.Language.Convert
import Pinafore.Language.Name
import Pinafore.Language.Value
import Pinafore.PredicateMorphism
import Shapes

data TypeContext =
    MkTypeContext

newtype QTypeCheck a =
    MkQTypeCheck (ReaderT TypeContext (Result Text) a)
    deriving (Functor, Applicative, Monad, MonadFail)

runQTypeCheck :: QTypeCheck a -> Result Text a
runQTypeCheck (MkQTypeCheck qa) = runReaderT qa MkTypeContext

type PinaforeTypeSystem baseedit = Unitype QTypeCheck (QValue baseedit)

instance HasPinaforeEntityEdit baseedit => UnitypeValue (QValue baseedit) where
    applyValue = qapply
    abstractValue = qfunction

type QExpr baseedit = TypedExpression Name (PinaforeTypeSystem baseedit)

qConstExpr ::
       forall baseedit a. ToQValue baseedit a
    => a
    -> QTypeCheck (QExpr baseedit)
qConstExpr a = return $ constTypedExpression @(PinaforeTypeSystem baseedit) Refl $ toQValue a

qVarExpr ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Name
    -> QTypeCheck (QExpr baseedit)
qVarExpr name = return $ varTypedExpression @(PinaforeTypeSystem baseedit) name

qAbstractExpr ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Name
    -> QExpr baseedit
    -> QTypeCheck (QExpr baseedit)
qAbstractExpr name expr = abstractTypedExpression @(PinaforeTypeSystem baseedit) name expr

qAbstractsExpr :: HasPinaforeEntityEdit baseedit => [Name] -> QExpr baseedit -> QTypeCheck (QExpr baseedit)
qAbstractsExpr [] e = return e
qAbstractsExpr (n:nn) e = do
    e' <- qAbstractsExpr nn e
    qAbstractExpr n e'

qApplyExpr ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => QExpr baseedit
    -> QExpr baseedit
    -> QTypeCheck (QExpr baseedit)
qApplyExpr exprf expra = applyTypedExpression @(PinaforeTypeSystem baseedit) exprf expra

qApplyAllExpr :: HasPinaforeEntityEdit baseedit => QExpr baseedit -> [QExpr baseedit] -> QTypeCheck (QExpr baseedit)
qApplyAllExpr e [] = return e
qApplyAllExpr e (a:aa) = do
    e' <- qApplyExpr e a
    qApplyAllExpr e' aa

qSequenceExpr :: [QExpr baseedit] -> QExpr baseedit
qSequenceExpr = osequenceA $ MkAny QTList

type QBindList baseedit = [(Name, QTypeCheck (QExpr baseedit))]

bindListToBindings ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => QBindList baseedit
    -> QTypeCheck (TypedBindings Name (PinaforeTypeSystem baseedit))
bindListToBindings bl =
    fmap mconcat $
    for bl $ \(n, me) -> do
        e <- me
        return $ singleTypedBinding @(PinaforeTypeSystem baseedit) n e

qBindExpr ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Name
    -> QTypeCheck (QExpr baseedit)
    -> QBindList baseedit
qBindExpr n e = pure (n, e)

qBindVal :: (HasPinaforeEntityEdit baseedit, ToQValue baseedit t) => Name -> t -> QBindList baseedit
qBindVal name val = qBindExpr name $ qConstExpr val

qLetExpr ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Name
    -> QExpr baseedit
    -> QExpr baseedit
    -> QTypeCheck (QExpr baseedit)
qLetExpr name exp body = letTypedExpression @(PinaforeTypeSystem baseedit) name exp body

qBindingsLetExpr ::
       forall baseedit m. (MonadFail m, HasPinaforeEntityEdit baseedit)
    => QBindList baseedit
    -> m (QExpr baseedit -> QTypeCheck (QExpr baseedit))
qBindingsLetExpr bl = do
    checkDuplicates $ fmap fst bl
    return $ \e -> qUncheckedBindingsLetExpr bl e

qUncheckedBindingsLetExpr ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => QBindList baseedit
    -> QExpr baseedit
    -> QTypeCheck (QExpr baseedit)
qUncheckedBindingsLetExpr bl e = do
    bindings <- bindListToBindings bl
    uncheckedBindingsLetTypedExpression @(PinaforeTypeSystem baseedit) bindings e

qEvalExpr ::
       forall baseedit m. MonadFail m
    => QExpr baseedit
    -> m (QValue baseedit)
qEvalExpr expr = do
    MkAny Refl val <- evalTypedExpression @(PinaforeTypeSystem baseedit) expr
    return val
