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

mapExpressionM ::
       forall m w1 w2 a. Applicative m
    => (forall t. w1 t -> m (Expression w2 t))
    -> Expression w1 a
    -> m (Expression w2 a)
mapExpressionM _ (ClosedExpression a) = pure $ ClosedExpression a
mapExpressionM f (OpenExpression wt expr) = liftA2 (liftA2 $ \t ta -> ta t) (f wt) (mapExpressionM f expr)

mapExpression :: forall w1 w2 a. (forall t. w1 t -> Expression w2 t) -> Expression w1 a -> Expression w2 a
mapExpression m expr = runIdentity $ mapExpressionM (\wt -> Identity $ m wt) expr

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

mapExactExpressionM ::
       forall m w1 w2 a. Applicative m
    => (forall t. w1 t -> m (w2 t))
    -> Expression w1 a
    -> m (Expression w2 a)
mapExactExpressionM _ (ClosedExpression a) = pure $ ClosedExpression a
mapExactExpressionM f (OpenExpression wt expr) = liftA2 OpenExpression (f wt) (mapExactExpressionM f expr)

mapExactExpression :: forall w1 w2 a. (forall t. w1 t -> w2 t) -> Expression w1 a -> Expression w2 a
mapExactExpression m expr = runIdentity $ mapExactExpressionM (\wt -> Identity $ m wt) expr

reverseExpression :: Expression w a -> Expression w a
reverseExpression (ClosedExpression a) = ClosedExpression a
reverseExpression (OpenExpression w expr) = reverseExpression expr <*> varExpression w

partitionExpression ::
       forall w1 w2 w3 a r.
       (forall t. w1 t -> Either (Expression w2 t) (Expression w3 t))
    -> Expression w1 a
    -> (forall b. Expression w2 (b -> a) -> Expression w3 b -> r)
    -> r
partitionExpression _tst (ClosedExpression a) call = call (pure id) (ClosedExpression a)
partitionExpression tst (OpenExpression (wt :: w1 t) expr) call =
    partitionExpression tst expr $ \eba eb ->
        case tst wt of
            Left ex -> call (liftA2 (\t bta b -> bta b t) ex eba) eb
            Right ex -> call (fmap (\bta (t, b) -> bta b t) eba) (liftA2 (,) ex eb)

-- True in the first expression, False in the second
partitionIfExpression ::
       forall w a r.
       (forall t. w t -> Bool)
    -> Expression w a
    -> (forall b. Expression w (b -> a) -> Expression w b -> r)
    -> r
partitionIfExpression tst =
    partitionExpression $ \wt ->
        if tst wt
            then Left $ varExpression wt
            else Right $ varExpression wt

findFirstExpression ::
       forall w1 w2 a r.
       (forall t. w1 t -> Maybe (w2 t))
    -> Expression w1 a
    -> (forall b. w2 b -> Expression w1 (b -> a) -> r)
    -> Maybe r
findFirstExpression _ (ClosedExpression _) _ = Nothing
findFirstExpression tst (OpenExpression wt expr) call =
    case tst wt of
        Just wt' -> Just $ call wt' expr
        Nothing ->
            findFirstExpression tst expr $ \wt' expr' -> call wt' $ OpenExpression wt $ fmap (\bta t b -> bta b t) expr'
