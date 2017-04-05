module Truth.Core.Read.Readable where
{
    import Truth.Core.Import;
    import Truth.Core.Read.Reader;


    newtype Readable reader a = MkReadable { unReadable :: forall m. Monad m => Structure m reader -> m a};

    readable :: reader t -> Readable reader t;
    readable rt = MkReadable (\s -> s rt);

    fromReadable :: (Reader reader) => Readable reader t -> ReaderSubject reader -> t;
    fromReadable (MkReadable rrt) a = runIdentity $ rrt $ Identity . (readFrom a);

    instance Functor (Readable reader) where
    {
        fmap ab (MkReadable sma) = MkReadable (\s -> fmap ab (sma s));
    };

    instance Applicative (Readable reader) where
    {
        pure a = MkReadable $ \_ -> pure a;
        (MkReadable smab) <*> (MkReadable sma) = MkReadable $ \s -> smab s <*> sma s;
    };

    instance Monad (Readable reader) where
    {
        return = pure;
        (MkReadable sma) >>= f = MkReadable $ \s -> do
        {
            a <- sma s;
            unReadable (f a) s;
        };
    };


    exec :: Monad m => m (m a) -> m a;
    exec mma = mma >>= id;

    type ReadableF f reader = Compose (Readable reader) f;

    readableF :: Applicative f => reader t -> ReadableF f reader t;
    readableF rt = MkCompose $ fmap pure $ readable rt;

    fromReadableF :: (Reader reader) => ReadableF f reader t -> ReaderSubject reader -> f t;
    fromReadableF (MkCompose rf) = fromReadable rf;
}
