module Control.Monad.Free where
{
    import Data.Foldable;
    import Data.Traversable;
    import Control.Applicative;
    import Data.ConstFunction;
    import Data.Result;
    import Data.FunctorOne;

    data FreeMonad f a = FreeReturn a | FreeBind (f (FreeMonad f a));

    instance (Functor f) => Functor (FreeMonad f) where
    {
        fmap ab (FreeReturn a) = FreeReturn (ab a);
        fmap ab (FreeBind fffa) = FreeBind (fmap (fmap ab) fffa);
    };

    instance (Foldable f) => Foldable (FreeMonad f) where
    {
        foldMap am (FreeReturn a) = am a;
        foldMap am (FreeBind fffa) = foldMap (foldMap am) fffa;
    };

    instance (Traversable f) => Traversable (FreeMonad f) where
    {
        sequenceA (FreeReturn ma) = fmap FreeReturn ma;
        sequenceA (FreeBind fffma) = fmap FreeBind (sequenceA (fmap sequenceA fffma));
    };

    instance (Functor f) => FunctorAp (FreeMonad f) where
    {
        fap ffab ffa = ffab >>= (\ab -> fmap ab ffa);
    };

    instance (Functor f) => FunctorBind (FreeMonad f) where
    {
        bind (FreeReturn a) afb = afb a;
        bind (FreeBind fffa) afb = FreeBind (fmap (\ffa -> bind ffa afb) fffa);
    };

    instance (Functor f) => FunctorGetPure (FreeMonad f) where
    {
        getPure = ConstConstFunction FreeReturn;
    };

    instance (FunctorOne f) => FunctorOne (FreeMonad f) where
    {
        retrieveOne (FreeReturn a) = SuccessResult a;
        retrieveOne (FreeBind fffa) = case retrieveOne fffa of
        {
            SuccessResult ffa -> retrieveOne ffa;
            FailureResult (MkLimit fb) -> FailureResult (MkLimit (toFreeMonad fb));
        };
    };

    instance (Functor f) => Applicative (FreeMonad f) where
    {
        pure = FreeReturn;
        (<*>) = fap;
    };

    instance (Functor f) => Monad (FreeMonad f) where
    {
        return = pure;
        (>>=) = bind;
    };

    toFreeMonad :: (Functor f) => f t -> FreeMonad f t;
    toFreeMonad ft = FreeBind (fmap FreeReturn ft);

    fromFreeMonad :: (forall a. a -> f a) -> (forall a b. f a -> (a -> f b) -> f b) -> FreeMonad f t -> f t;
    fromFreeMonad return' bind' = fFM where
    {
        fFM (FreeReturn a) = return' a;
        fFM (FreeBind fffa) = bind' fffa fFM;
    };
}
