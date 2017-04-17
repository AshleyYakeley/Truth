module Truth.Core.Read.Readable where
{
    import Truth.Core.Import;
    import Truth.Core.Read.Reader;


    class Monad m => IsReadableMonad m where
    {
        type RMReader m :: * -> *;
        readable :: forall t. RMReader m t -> m t;
    };


    newtype Readable reader a = MkReadable { unReadable :: forall m. Monad m => MutableRead m reader -> m a};

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

    instance IsReadableMonad (Readable reader) where
    {
        type RMReader (Readable reader) = reader;
        readable rt = MkReadable (\s -> s rt);
    };

    readableToM :: IsReadableMonad m => Readable (RMReader m) t -> m t;
    readableToM (MkReadable sma) = sma readable;


    exec :: Monad m => m (m a) -> m a;
    exec mma = mma >>= id;

    type ReadableF f reader = Compose (Readable reader) f;

    fromReadableF :: (Reader reader) => ReadableF f reader t -> ReaderSubject reader -> f t;
    fromReadableF (MkCompose rf) = fromReadable rf;

    instance (IsReadableMonad m,Monad f,Traversable f) => IsReadableMonad (Compose m f) where
    {
        type RMReader (Compose m f) = RMReader m;
        readable rt = MkCompose $ fmap pure $ readable rt;
    };
}
