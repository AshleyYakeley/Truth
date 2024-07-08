module Language.Expression.Common.Expression where

import Language.Expression.Common.Error
import Language.Expression.Common.WitnessMappable
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

instance (forall t. WitnessMappable poswit negwit (w t)) => WitnessMappable poswit negwit (Expression w a) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \case
            ClosedExpression a -> pure $ ClosedExpression a
            OpenExpression wt expr ->
                liftA2
                    OpenExpression
                    (unEndoM (mapWitnessesM mapPos mapNeg) wt)
                    (unEndoM (mapWitnessesM mapPos mapNeg) expr)

isClosedExpression :: Expression w t -> Bool
isClosedExpression (ClosedExpression _) = True
isClosedExpression (OpenExpression _ _) = False

expressionFreeWitnesses :: (forall t. w t -> r) -> Expression w a -> [r]
expressionFreeWitnesses _wr (ClosedExpression _) = []
expressionFreeWitnesses wr (OpenExpression wt expr) = (wr wt) : expressionFreeWitnesses wr expr

expressionFreeWitnessCount :: Expression w a -> Int
expressionFreeWitnessCount (ClosedExpression _) = 0
expressionFreeWitnessCount (OpenExpression _ expr) = succ $ expressionFreeWitnessCount expr

evalExpressionResult :: Expression w --> Result (ExpressionError w)
evalExpressionResult (ClosedExpression a) = return a
evalExpressionResult (OpenExpression wt expr) =
    throwExc $ UndefinedBindingsError $ MkSome wt :| expressionFreeWitnesses MkSome expr

evalExpression :: MonadThrow (ExpressionError w) m => Expression w --> m
evalExpression expr = fromResult $ evalExpressionResult expr

varExpression :: w t -> Expression w t
varExpression wt = OpenExpression wt $ ClosedExpression id

runExpression ::
       forall m w. Applicative m
    => (w --> m)
    -> Expression w --> m
runExpression _f (ClosedExpression a) = pure a
runExpression f (OpenExpression wt expr) = runExpression f expr <*> f wt

runExpressionM ::
       forall m w f a. (Applicative m, Applicative f)
    => (forall t. w t -> m (f t))
    -> Expression w a
    -> m (f a)
runExpressionM f expr = getCompose $ runExpression (Compose . f) expr

mergeExpressionWitnessesM ::
       forall m w t a. Monad m
    => Expression w t
    -> (forall x. w x -> m (Maybe (Expression w (x, t))))
    -> Expression w (t -> a)
    -> m (Expression w a)
mergeExpressionWitnessesM newExpr matchExpr = let
    runMerge :: forall r. Expression w (t -> r) -> m (Expression w r)
    runMerge (ClosedExpression ta) = return $ fmap ta newExpr
    runMerge (OpenExpression wt expr) = do
        mfexpr <- matchExpr wt
        case mfexpr of
            Just fexpr -> return $ liftA2 (\(a, b) f -> f a b) fexpr expr
            Nothing -> do
                rexpr <- runMerge $ fmap (\f conv t -> f t conv) expr
                return $ OpenExpression wt rexpr
    in runMerge

combineExpressionWitnessesM ::
       forall m w r. Monad m
    => (forall a b. w a -> w b -> m (Maybe (Expression w (a, b))))
    -> Expression w r
    -> m (Expression w r)
combineExpressionWitnessesM _ expr@(ClosedExpression _) = return expr
combineExpressionWitnessesM f (OpenExpression wt expr) = do
    expr1 <- combineExpressionWitnessesM f expr
    mergeExpressionWitnessesM (varExpression wt) (\wx -> f wx wt) expr1

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

partitionExpressionM ::
       forall m w1 w2 w3 a r. Monad m
    => (forall t. w1 t -> m (Either (Expression w2 t) (Expression w3 t)))
    -> Expression w1 a
    -> (forall b. Expression w2 (b -> a) -> Expression w3 b -> m r)
    -> m r
partitionExpressionM _tst (ClosedExpression a) call = call (pure id) (ClosedExpression a)
partitionExpressionM tst (OpenExpression (wt :: w1 t) expr) call =
    partitionExpressionM tst expr $ \eba eb -> do
        eexpr <- tst wt
        case eexpr of
            Left ex -> call (liftA2 (\t bta b -> bta b t) ex eba) eb
            Right ex -> call (fmap (\bta (t, b) -> bta b t) eba) (liftA2 (,) ex eb)

-- True in the first expression, False in the second
partitionIfExpressionM ::
       forall m w a r. Monad m
    => (forall t. w t -> m Bool)
    -> Expression w a
    -> (forall b. Expression w (b -> a) -> Expression w b -> m r)
    -> m r
partitionIfExpressionM tst =
    partitionExpressionM $ \wt -> do
        b <- tst wt
        return $
            if b
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
