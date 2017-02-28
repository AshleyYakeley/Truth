{-# OPTIONS -fno-warn-orphans #-}
module Data.FunctorOne where
{
    import Prelude hiding (sequence);
    import Data.ConstFunction;
    import Data.Chain;
    import Data.Traversable;
    import Data.Result;
    import Data.Compose;
    import Data.Functor.Identity;
    import Control.Monad hiding (sequence);

    mmap :: (Monad m) => (a -> b) -> m a -> m b;
    mmap ab ma = ma >>= (return . ab);

    instance (Monad m,Monad n,Traversable n) => Monad (Compose m n) where
    {
        return a = MkCompose (return (return a));
        (MkCompose mna) >>= acb = MkCompose (mna >>= (\na -> mmap join (sequence (mmap (getCompose . acb) na))))
    };

    -- | should be superclass of Applicative
    ;
    class (Functor f) => FunctorAp f where
    {
        fap :: f (a -> b) -> f a -> f b;
    };

    liftF :: (Functor f) => (a -> r) -> f a -> f r;
    liftF = fmap;

    liftF2 :: (FunctorAp f) => (a -> b -> r) -> f a -> f b -> f r;
    liftF2 abr fa fb = fap (fmap abr fa) fb;

    instance FunctorAp ((-> ) p) where
    {
        fap = ap;
    };

    -- | should be superclass of Monad
    ;
    class (FunctorAp f) => FunctorBind f where
    {
        -- fap fab fa = bind fab (\ab -> fmap ab fa);
        bind :: f a -> (a -> f b) -> f b;
        exec :: f (f a) -> f a;
        exec ffa = bind ffa id;
    };

    instance FunctorBind ((-> ) p) where
    {
        bind = (>>=);
    };

    newtype Limit f = MkLimit (forall a. f a);

    class (Traversable f,FunctorBind f,FunctorGetPure f) => FunctorOne f where
    {
        retrieveOne :: f a -> Result (Limit f) a;

        getMaybeOne :: f a -> Maybe a;
        getMaybeOne fa = resultToMaybe (retrieveOne fa);
    };
    -- retrieveOne (fmap f w) = fmap f (retrieveOne w)
    -- case (retrieveOne w) of {Left w' -> w';Right a -> fmap (\_ -> a) w;} = w

    traverseOne :: (FunctorOne f,Applicative m) => (a -> m b) -> f a -> m (f b);
    traverseOne amb fa = case retrieveOne fa of
    {
        SuccessResult a -> fmap (\b -> fmap (\_ -> b) fa) (amb a);
        FailureResult (MkLimit fx) -> pure fx;
    };

    sequenceAOne :: (FunctorOne f,Applicative m) => f (m a) -> m (f a);
    sequenceAOne fma = case retrieveOne fma of
    {
        SuccessResult ma -> fmap (\b -> fmap (\_ -> b) fma) ma;
        FailureResult (MkLimit fx) -> pure fx;
    };

    bindOne :: (FunctorOne f) => f a -> (a -> f b) -> f b;
    bindOne fa afb = case retrieveOne fa of
    {
        SuccessResult a -> afb a;
        FailureResult (MkLimit fx) -> fx;
    };

    instance FunctorAp Identity where
    {
        fap = ap;
    };

    instance FunctorBind Identity where
    {
        bind = (>>=);
    };

    instance FunctorGetPure Identity where
    {
        getPure = applicativeGetPure;
    };

    instance FunctorOne Identity where
    {
        retrieveOne (Identity a) = SuccessResult a;
        getMaybeOne (Identity a) = Just a;
    };

    instance FunctorAp Maybe where
    {
        fap = ap;
    };

    instance FunctorBind Maybe where
    {
        bind = (>>=);
    };

    instance FunctorGetPure Maybe where
    {
        getPure = applicativeGetPure;
    };

    instance FunctorOne Maybe where
    {
        retrieveOne (Just a) = SuccessResult a;
        retrieveOne Nothing = FailureResult (MkLimit Nothing);
        getMaybeOne = id;
    };

    instance FunctorAp (Either p) where
    {
        fap (Left p) _ = Left p;
        fap _ (Left p) = Left p;
        fap (Right ab) (Right a) = Right (ab a);
    };

    instance FunctorBind (Either p) where
    {
        bind (Left p) _ = Left p;
        bind (Right a) afb = afb a;
    };

    instance FunctorGetPure (Either p) where
    {
        getPure = applicativeGetPure;
    };

    instance FunctorOne (Either p) where
    {
        retrieveOne (Right b) = SuccessResult b;
        retrieveOne (Left a) = FailureResult (MkLimit (Left a));
    };

    instance FunctorAp ((,) p) where
    {
        fap (_p,ab) (p,a) = (p,ab a);
    };

    instance FunctorBind ((,) p) where
    {
        bind (_p,a) afb = afb a;
    };

    instance FunctorGetPure ((,) p);

    instance FunctorOne ((,) p) where
    {
        retrieveOne (_,a) = SuccessResult a;
    };

    instance FunctorAp (Result e) where
    {
        fap = ap;
    };

    instance FunctorBind (Result e) where
    {
        bind = (>>=);
    };

    instance FunctorGetPure (Result e) where
    {
        getPure = applicativeGetPure;
    };

    instance FunctorOne (Result e) where
    {
        retrieveOne (SuccessResult a) = SuccessResult a;
        retrieveOne (FailureResult e) = FailureResult (MkLimit (FailureResult e));
        getMaybeOne = resultToMaybe;
    };

    constFunctionAp :: (FunctorOne f,Applicative (t (f a)),CatFunctor t f) => f (t a b) -> t (f a) (f b);
    constFunctionAp fcab = case retrieveOne fcab of
    {
        FailureResult (MkLimit fx) -> pure fx;
        SuccessResult cab -> cfmap cab;
    };
}
