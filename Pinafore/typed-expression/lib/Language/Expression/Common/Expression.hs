module Language.Expression.Common.Expression where

import Language.Expression.Common.Error
import Language.Expression.Common.WitnessMappable
import Shapes

type FunctionExpression :: (Type -> Type) -> Type -> Type
data FunctionExpression w a
    = ClosedExpression a
    | forall t. OpenExpression (w t)
                               (FunctionExpression w (t -> a))

instance Functor (FunctionExpression w) where
    fmap ab (ClosedExpression a) = ClosedExpression $ ab a
    fmap ab (OpenExpression name expr) = OpenExpression name $ fmap (\va v -> ab $ va v) expr

instance Invariant (FunctionExpression w) where
    invmap ab _ = fmap ab

instance Applicative (FunctionExpression w) where
    pure = ClosedExpression
    (ClosedExpression ab) <*> expr = fmap ab expr
    (OpenExpression name exprab) <*> expr = OpenExpression name $ (\vab a v -> vab v a) <$> exprab <*> expr

instance Productable (FunctionExpression w)

instance AllConstraint Show w => Show (FunctionExpression w a) where
    show expr = "{" <> intercalate "; " (expressionFreeWitnesses allShow expr) <> "}"

instance AllConstraint Show w => AllConstraint Show (FunctionExpression w) where
    allConstraint = Dict

instance (forall t. WitnessMappable poswit negwit (w t)) => WitnessMappable poswit negwit (FunctionExpression w a) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \case
            ClosedExpression a -> pure $ ClosedExpression a
            OpenExpression wt expr ->
                liftA2
                    OpenExpression
                    (unEndoM (mapWitnessesM mapPos mapNeg) wt)
                    (unEndoM (mapWitnessesM mapPos mapNeg) expr)

isClosedExpression :: FunctionExpression w t -> Bool
isClosedExpression (ClosedExpression _) = True
isClosedExpression (OpenExpression _ _) = False

expressionFreeWitnesses :: (forall t. w t -> r) -> FunctionExpression w a -> [r]
expressionFreeWitnesses _wr (ClosedExpression _) = []
expressionFreeWitnesses wr (OpenExpression wt expr) = (wr wt) : expressionFreeWitnesses wr expr

expressionFreeWitnessCount :: FunctionExpression w a -> Int
expressionFreeWitnessCount (ClosedExpression _) = 0
expressionFreeWitnessCount (OpenExpression _ expr) = succ $ expressionFreeWitnessCount expr

evalExpressionResult :: FunctionExpression w --> Result (ExpressionError w)
evalExpressionResult (ClosedExpression a) = return a
evalExpressionResult (OpenExpression wt expr) =
    throwExc $ UndefinedBindingsError $ MkSome wt :| expressionFreeWitnesses MkSome expr

evalExpression :: MonadThrow (ExpressionError w) m => FunctionExpression w --> m
evalExpression expr = fromResult $ evalExpressionResult expr

varExpression :: w t -> FunctionExpression w t
varExpression wt = OpenExpression wt $ ClosedExpression id

abstractExpression :: TestEquality w => w a -> FunctionExpression w b -> FunctionExpression w (a -> b)
abstractExpression _ (ClosedExpression a) = ClosedExpression $ \_ -> a
abstractExpression wa (OpenExpression wt expr)
    | Just Refl <- testEquality wa wt = fmap (\aab a -> aab a a) $ abstractExpression wa expr
abstractExpression wa (OpenExpression wt expr) =
    OpenExpression wt $ fmap (\atb t a -> atb a t) $ abstractExpression wa expr

runExpression ::
       forall m w. Applicative m
    => (w --> m)
    -> FunctionExpression w --> m
runExpression _f (ClosedExpression a) = pure a
runExpression f (OpenExpression wt expr) = runExpression f expr <*> f wt

runExpressionM ::
       forall m w f a. (Applicative m, Applicative f)
    => (forall t. w t -> m (f t))
    -> FunctionExpression w a
    -> m (f a)
runExpressionM f expr = getCompose $ runExpression (Compose . f) expr

mergeExpressionWitnessesM ::
       forall m w t a. Monad m
    => FunctionExpression w t
    -> (forall x. w x -> m (Maybe (FunctionExpression w (x, t))))
    -> FunctionExpression w (t -> a)
    -> m (FunctionExpression w a)
mergeExpressionWitnessesM newExpr matchExpr = let
    runMerge :: forall r. FunctionExpression w (t -> r) -> m (FunctionExpression w r)
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
    => (forall a b. w a -> w b -> m (Maybe (FunctionExpression w (a, b))))
    -> FunctionExpression w r
    -> m (FunctionExpression w r)
combineExpressionWitnessesM _ expr@(ClosedExpression _) = return expr
combineExpressionWitnessesM f (OpenExpression wt expr) = do
    expr1 <- combineExpressionWitnessesM f expr
    mergeExpressionWitnessesM (varExpression wt) (\wx -> f wx wt) expr1

mapExactExpressionM ::
       forall m w1 w2 a. Applicative m
    => (forall t. w1 t -> m (w2 t))
    -> FunctionExpression w1 a
    -> m (FunctionExpression w2 a)
mapExactExpressionM _ (ClosedExpression a) = pure $ ClosedExpression a
mapExactExpressionM f (OpenExpression wt expr) = liftA2 OpenExpression (f wt) (mapExactExpressionM f expr)

mapExactExpression :: forall w1 w2 a. (forall t. w1 t -> w2 t) -> FunctionExpression w1 a -> FunctionExpression w2 a
mapExactExpression m expr = runIdentity $ mapExactExpressionM (\wt -> Identity $ m wt) expr

reverseExpression :: FunctionExpression w a -> FunctionExpression w a
reverseExpression (ClosedExpression a) = ClosedExpression a
reverseExpression (OpenExpression w expr) = reverseExpression expr <*> varExpression w

partitionExpressionM ::
       forall m w1 w2 w3 a r. Monad m
    => (forall t. w1 t -> m (Either (FunctionExpression w2 t) (FunctionExpression w3 t)))
    -> FunctionExpression w1 a
    -> (forall b. FunctionExpression w2 (b -> a) -> FunctionExpression w3 b -> m r)
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
    -> FunctionExpression w a
    -> (forall b. FunctionExpression w (b -> a) -> FunctionExpression w b -> m r)
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
    -> FunctionExpression w1 a
    -> (forall b. w2 b -> FunctionExpression w1 (b -> a) -> r)
    -> Maybe r
findFirstExpression _ (ClosedExpression _) _ = Nothing
findFirstExpression tst (OpenExpression wt expr) call =
    case tst wt of
        Just wt' -> Just $ call wt' expr
        Nothing ->
            findFirstExpression tst expr $ \wt' expr' -> call wt' $ OpenExpression wt $ fmap (\bta t b -> bta b t) expr'
