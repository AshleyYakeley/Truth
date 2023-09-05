module Language.Expression.Common.Expression where

import Language.Expression.Common.Error
import Shapes

type Expression :: (Type -> Type) -> Type -> Type
data Expression w a
    = ClosedExpression a
    | forall t. OpenExpression (w t)
                               (Expression w (t -> a))

instance Functor (Expression w) where
    fmap ab (ClosedExpression a) = ClosedExpression $ ab a
    fmap ab (OpenExpression name expr) = OpenExpression name $ fmap (\va v -> ab $ va v) expr

instance Invariant (Expression w) where
    invmap ab _ = fmap ab

instance Applicative (Expression w) where
    pure = ClosedExpression
    (ClosedExpression ab) <*> expr = fmap ab expr
    (OpenExpression name exprab) <*> expr = OpenExpression name $ (\vab a v -> vab v a) <$> exprab <*> expr

instance Productable (Expression w)

instance AllConstraint Show w => Show (Expression w a) where
    show expr = "{" <> intercalate "; " (expressionFreeWitnesses allShow expr) <> "}"

instance AllConstraint Show w => AllConstraint Show (Expression w) where
    allConstraint = Dict

isClosedExpression :: Expression w t -> Bool
isClosedExpression (ClosedExpression _) = True
isClosedExpression (OpenExpression _ _) = False

expressionFreeWitnesses :: (forall t. w t -> r) -> Expression w a -> [r]
expressionFreeWitnesses _wr (ClosedExpression _) = []
expressionFreeWitnesses wr (OpenExpression wt expr) = (wr wt) : expressionFreeWitnesses wr expr

expressionFreeWitnessCount :: Expression w a -> Int
expressionFreeWitnessCount (ClosedExpression _) = 0
expressionFreeWitnessCount (OpenExpression _ expr) = succ $ expressionFreeWitnessCount expr

evalExpressionResult :: Expression w a -> Result (ExpressionError w) a
evalExpressionResult (ClosedExpression a) = return a
evalExpressionResult (OpenExpression wt expr) =
    throwExc $ UndefinedBindingsError $ MkSome wt :| expressionFreeWitnesses MkSome expr

evalExpression :: MonadThrow (ExpressionError w) m => Expression w a -> m a
evalExpression expr = fromResult $ evalExpressionResult expr

varExpression :: w t -> Expression w t
varExpression wt = OpenExpression wt $ ClosedExpression id

solveExpression :: Applicative m => (forall t. w t -> m t) -> Expression w a -> m a
solveExpression _f (ClosedExpression a) = pure a
solveExpression f (OpenExpression wt expr) = solveExpression f expr <*> f wt

mapExpressionWitnessesM ::
       Applicative m
    => (forall t r. w t -> (forall t'. w t' -> (t' -> t) -> m r) -> m r)
    -> Expression w a
    -> m (Expression w a)
mapExpressionWitnessesM _ (ClosedExpression a) = pure $ ClosedExpression a
mapExpressionWitnessesM f (OpenExpression wt expr) =
    f wt $ \wt' conv -> fmap (OpenExpression wt') $ mapExpressionWitnessesM f $ fmap (\ta -> ta . conv) expr

mapExpressionWitnesses ::
       (forall t r. w t -> (forall t'. w t' -> (t' -> t) -> r) -> r) -> Expression w a -> Expression w a
mapExpressionWitnesses m exp =
    runIdentity $ mapExpressionWitnessesM (\wt call -> Identity $ m wt $ \wt' conv -> runIdentity $ call wt' conv) exp
