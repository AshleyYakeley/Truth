module Data.Functor.Free where
{
    data FreeFunctor f t where
    {
        MkFreeFunctor :: forall p f t. (p -> t) -> f p -> FreeFunctor f t
    };

    instance Functor (FreeFunctor f) where
    {
        fmap ab (MkFreeFunctor pa fp) = MkFreeFunctor (ab . pa) fp;
    };

    toFreeFunctor :: f t -> FreeFunctor f t;
    toFreeFunctor ft = MkFreeFunctor id ft;

    fromFreeFunctor :: (forall a b. (a -> b) -> f a -> f b) -> FreeFunctor f t -> f t;
    fromFreeFunctor fmap' (MkFreeFunctor pt fp) = fmap' pt fp;
}
