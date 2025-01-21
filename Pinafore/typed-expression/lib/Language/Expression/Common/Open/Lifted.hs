module Language.Expression.Common.Open.Lifted where

import Shapes

import Language.Expression.Common.Open.Abstract
import Language.Expression.Common.Open.Error
import Language.Expression.Common.Open.Expression
import Language.Expression.Common.Open.Free

type LiftedExpression :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data LiftedExpression f w a
    = ClosedLiftedExpression (f a)
    | forall t. OpenLiftedExpression
        (w t)
        (LiftedExpression f w (t -> a))

instance Functor f => Functor (LiftedExpression f w) where
    fmap ab (ClosedLiftedExpression a) = ClosedLiftedExpression $ fmap ab a
    fmap ab (OpenLiftedExpression name expr) = OpenLiftedExpression name $ fmap (\va v -> ab $ va v) expr

instance Functor f => Invariant (LiftedExpression f w) where
    invmap ab _ = fmap ab

liftedMapLiftedExpression :: Applicative f => f (a -> b) -> LiftedExpression f w a -> LiftedExpression f w b
liftedMapLiftedExpression fab (ClosedLiftedExpression fa) = ClosedLiftedExpression $ fab <*> fa
liftedMapLiftedExpression fab (OpenLiftedExpression wt expr) =
    OpenLiftedExpression wt $ liftedMapLiftedExpression (fmap fmap fab) expr

instance Applicative f => Applicative (LiftedExpression f w) where
    pure = ClosedLiftedExpression . pure
    (ClosedLiftedExpression ab) <*> expr = liftedMapLiftedExpression ab expr
    (OpenLiftedExpression name exprab) <*> expr = OpenLiftedExpression name $ (\vab a v -> vab v a) <$> exprab <*> expr

instance Applicative f => Productable (LiftedExpression f w)

instance AllConstraint Show w => Show (LiftedExpression f w a) where
    show expr = "{" <> intercalate "; " (freeWitnesses allShow expr) <> "}"

instance AllConstraint Show w => AllConstraint Show (LiftedExpression f w) where
    allConstraint = Dict

instance HasFreeWitnesses (LiftedExpression f) where
    freeWitnesses _wr (ClosedLiftedExpression _) = []
    freeWitnesses wr (OpenLiftedExpression wt expr) = (wr wt) : freeWitnesses wr expr

instance Functor f => AbstractWitness (LiftedExpression f) where
    abstractWitness = abstractLiftedExpression

abstractLiftedExpression ::
    (Functor f, TestEquality w) => w a -> LiftedExpression f w b -> LiftedExpression f w (a -> b)
abstractLiftedExpression _ (ClosedLiftedExpression fa) = ClosedLiftedExpression $ fmap (\a _ -> a) fa
abstractLiftedExpression wa (OpenLiftedExpression wt expr)
    | Just Refl <- testEquality wa wt = fmap (\aab a -> aab a a) $ abstractLiftedExpression wa expr
abstractLiftedExpression wa (OpenLiftedExpression wt expr) =
    OpenLiftedExpression wt $ fmap (\atb t a -> atb a t) $ abstractLiftedExpression wa expr

toLiftedExpression :: Applicative f => Expression w --> LiftedExpression f w
toLiftedExpression (ClosedExpression a) = ClosedLiftedExpression $ pure a
toLiftedExpression (OpenExpression wt expr) = OpenLiftedExpression wt $ toLiftedExpression expr

liftToExpression :: f --> LiftedExpression f w
liftToExpression = ClosedLiftedExpression

evalLiftedExpressionResult :: LiftedExpression f w a -> Result (ExpressionError w) (f a)
evalLiftedExpressionResult (ClosedLiftedExpression fa) = return fa
evalLiftedExpressionResult (OpenLiftedExpression wt expr) =
    throwExc $ UndefinedBindingsError $ MkSome wt :| freeWitnesses MkSome expr

swapLiftedExpression :: LiftedExpression (Expression w2) w1 a -> LiftedExpression (Expression w1) w2 a
swapLiftedExpression (ClosedLiftedExpression expr) = toLiftedExpression expr
swapLiftedExpression (OpenLiftedExpression wt lexpr) =
    liftedMapLiftedExpression (OpenExpression wt $ ClosedExpression (\t ta -> ta t)) $ swapLiftedExpression lexpr
