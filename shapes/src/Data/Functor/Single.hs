module Data.Functor.Single where
{
    data Single a = MkSingle;

    instance Functor Single where
    {
        fmap _ _ = MkSingle;
    };

    instance Applicative Single where
    {
        pure _ = MkSingle;
        _ <*> _ = MkSingle;
    };

    instance Monad Single where
    {
        _ >>= _ = MkSingle;
    };

    instance Foldable Single where
    {
        foldMap _ _ = mempty;
    };

    instance Traversable Single where
    {
        sequenceA _ = pure MkSingle;
    };
}
