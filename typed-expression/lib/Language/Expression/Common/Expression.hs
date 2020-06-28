module Language.Expression.Common.Expression where

import Language.Expression.Common.Error
import Shapes

data Expression w a
    = ClosedExpression a
    | forall t. OpenExpression (w t)
                               (Expression w (t -> a))

instance Functor (Expression w) where
    fmap ab (ClosedExpression a) = ClosedExpression $ ab a
    fmap ab (OpenExpression name expr) = OpenExpression name $ fmap (\va v -> ab $ va v) expr

instance Applicative (Expression w) where
    pure = ClosedExpression
    (ClosedExpression ab) <*> expr = fmap ab expr
    (OpenExpression name exprab) <*> expr = OpenExpression name $ (\vab a v -> vab v a) <$> exprab <*> expr

expressionFreeWitnesses :: (forall t. w t -> r) -> Expression w a -> [r]
expressionFreeWitnesses _wr (ClosedExpression _) = []
expressionFreeWitnesses wr (OpenExpression wt expr) = (wr wt) : expressionFreeWitnesses wr expr

evalExpression :: (MonadThrow ExpressionError m, AllWitnessConstraint Show w) => Expression w a -> m a
evalExpression (ClosedExpression a) = return a
evalExpression expr = throw $ UndefinedBindingsError $ nub $ expressionFreeWitnesses showAllWitness expr

varExpression :: w t -> Expression w t
varExpression wt = OpenExpression wt $ ClosedExpression id

solveExpression :: Applicative m => (forall t. w t -> m t) -> Expression w a -> m a
solveExpression _f (ClosedExpression a) = pure a
solveExpression f (OpenExpression wt expr) = solveExpression f expr <*> f wt
