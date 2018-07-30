module Language.Expression.Expression where

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

evalExpression :: (MonadFail m, AllWitnessConstraint Show w) => Expression w a -> m a
evalExpression (ClosedExpression a) = return a
evalExpression expr = fail $ "undefined: " <> intercalate ", " (expressionFreeWitnesses showAllWitness expr)

abstractExpression :: (forall t'. w t' -> Maybe (t -> t')) -> Expression w a -> Expression w (t -> a)
abstractExpression _match (ClosedExpression a) = ClosedExpression $ \_ -> a
abstractExpression match (OpenExpression wt expr)
    | Just ff <- match wt = fmap (\vva v -> vva v (ff v)) $ abstractExpression match expr
abstractExpression match (OpenExpression nw expr) =
    OpenExpression nw $ fmap (\vva v1 v2 -> vva v2 v1) $ abstractExpression match expr

varExpression :: w t -> Expression w t
varExpression wt = OpenExpression wt $ ClosedExpression id

letExpression :: (forall t'. w t' -> Maybe (t -> t')) -> Expression w t -> Expression w a -> Expression w a
letExpression match val body = abstractExpression match body <*> val
