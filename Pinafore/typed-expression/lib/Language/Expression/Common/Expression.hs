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
       forall m w1 w2 a. Applicative m
    => (forall t. w1 t -> m (Expression w2 t))
    -> Expression w1 a
    -> m (Expression w2 a)
mapExpressionWitnessesM _ (ClosedExpression a) = pure $ ClosedExpression a
mapExpressionWitnessesM f (OpenExpression wt expr) =
    liftA2 (liftA2 $ \t ta -> ta t) (f wt) (mapExpressionWitnessesM f expr)

mapExpressionWitnesses :: forall w1 w2 a. (forall t. w1 t -> Expression w2 t) -> Expression w1 a -> Expression w2 a
mapExpressionWitnesses m expr = runIdentity $ mapExpressionWitnessesM (\wt -> Identity $ m wt) expr

mergeExpressionWitnesses ::
       forall w t a.
       Expression w t
    -> (forall x. w x -> Maybe (Expression w (x, t)))
    -> Expression w (t -> a)
    -> Expression w a
mergeExpressionWitnesses newExpr matchExpr = let
    runMerge :: forall r. Expression w (t -> r) -> Expression w r
    runMerge (ClosedExpression ta) = fmap ta newExpr
    runMerge (OpenExpression wt expr) =
        case matchExpr wt of
            Just fexpr -> liftA2 (\(a, b) f -> f a b) fexpr expr
            Nothing -> OpenExpression wt $ runMerge $ fmap (\f conv t -> f t conv) expr
    in runMerge

combineExpressionWitnesses ::
       forall w r. (forall a b. w a -> w b -> Maybe (Expression w (a, b))) -> Expression w r -> Expression w r
combineExpressionWitnesses _ (ClosedExpression a) = ClosedExpression a
combineExpressionWitnesses f (OpenExpression wt expr) =
    mergeExpressionWitnesses (varExpression wt) (\wx -> f wx wt) $ combineExpressionWitnesses f expr

mapExactExpressionWitnessesM ::
       forall m w1 w2 a. Applicative m
    => (forall t. w1 t -> m (w2 t))
    -> Expression w1 a
    -> m (Expression w2 a)
mapExactExpressionWitnessesM _ (ClosedExpression a) = pure $ ClosedExpression a
mapExactExpressionWitnessesM f (OpenExpression wt expr) =
    liftA2 OpenExpression (f wt) (mapExactExpressionWitnessesM f expr)

mapExactExpressionWitnesses :: forall w1 w2 a. (forall t. w1 t -> w2 t) -> Expression w1 a -> Expression w2 a
mapExactExpressionWitnesses m expr = runIdentity $ mapExactExpressionWitnessesM (\wt -> Identity $ m wt) expr
