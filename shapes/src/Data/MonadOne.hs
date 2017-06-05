{-# OPTIONS -fno-warn-orphans #-}
module Data.MonadOne where
{
    import Prelude hiding (sequence);
    import Data.ConstFunction;
    import Data.Chain;
    import Data.Result;
    import Data.Functor.Identity;
    import Data.Functor.Single;


    newtype Limit f = MkLimit (forall a. f a);

    class (Traversable f,Monad f) => MonadOne f where
    {
        retrieveOne :: f a -> Result (Limit f) a;

        getMaybeOne :: f a -> Maybe a;
        getMaybeOne fa = resultToMaybe (retrieveOne fa);
    };
    -- retrieveOne (fmap f w) = fmap f (retrieveOne w)
    -- case (retrieveOne w) of {Left w' -> w';Right a -> fmap (\_ -> a) w;} = w

    traverseOne :: (MonadOne f,Applicative m) => (a -> m b) -> f a -> m (f b);
    traverseOne amb fa = case retrieveOne fa of
    {
        SuccessResult a -> fmap (\b -> fmap (\_ -> b) fa) (amb a);
        FailureResult (MkLimit fx) -> pure fx;
    };

    sequenceAOne :: (MonadOne f,Applicative m) => f (m a) -> m (f a);
    sequenceAOne fma = case retrieveOne fma of
    {
        SuccessResult ma -> fmap (\b -> fmap (\_ -> b) fma) ma;
        FailureResult (MkLimit fx) -> pure fx;
    };

    bindOne :: (MonadOne f) => f a -> (a -> f b) -> f b;
    bindOne fa afb = case retrieveOne fa of
    {
        SuccessResult a -> afb a;
        FailureResult (MkLimit fx) -> fx;
    };

    instance FunctorGetPure Identity where
    {
        getPure = applicativeGetPure;
    };

    instance MonadOne Identity where
    {
        retrieveOne (Identity a) = SuccessResult a;
        getMaybeOne (Identity a) = Just a;
    };

    instance FunctorGetPure Maybe where
    {
        getPure = applicativeGetPure;
    };

    instance MonadOne Maybe where
    {
        retrieveOne (Just a) = SuccessResult a;
        retrieveOne Nothing = FailureResult (MkLimit Nothing);
        getMaybeOne = id;
    };

    instance FunctorGetPure (Either p) where
    {
        getPure = applicativeGetPure;
    };

    instance MonadOne (Either p) where
    {
        retrieveOne (Right b) = SuccessResult b;
        retrieveOne (Left a) = FailureResult (MkLimit (Left a));
    };

    instance FunctorGetPure ((,) p);

    instance Monoid p => MonadOne ((,) p) where
    {
        retrieveOne (_,a) = SuccessResult a;
    };

    instance FunctorGetPure (Result e) where
    {
        getPure = applicativeGetPure;
    };

    instance MonadOne (Result e) where
    {
        retrieveOne (SuccessResult a) = SuccessResult a;
        retrieveOne (FailureResult e) = FailureResult (MkLimit (FailureResult e));
        getMaybeOne = resultToMaybe;
    };

    instance MonadOne Single where
    {
        retrieveOne _ = FailureResult $ MkLimit MkSingle;
    };

    constFunctionAp :: (MonadOne f,Applicative (t (f a)),CatFunctor t f) => f (t a b) -> t (f a) (f b);
    constFunctionAp fcab = case retrieveOne fcab of
    {
        FailureResult (MkLimit fx) -> pure fx;
        SuccessResult cab -> cfmap cab;
    };
}
