module Truth.Core.Read.Readable where
{
    import Truth.Core.Import;
    import Truth.Core.Read.SubjectReader;


    class MonadIO m => IsReadableMonad m where
    {
        type RMReader m :: * -> *;
        readable :: forall t. RMReader m t -> m t;
    };

    newtype Readable reader a = MkReadable { unReadable :: forall m. (MonadIO m) => MutableRead m reader -> m a};

    liftReadable :: (forall m. (MonadIO m) => m a) -> Readable reader a;
    liftReadable ma = MkReadable $ \_ -> ma;

    fromReadableSubject :: (SubjectReader reader,MonadIO m) => Readable reader t -> ReaderSubject reader -> m t;
    fromReadableSubject (MkReadable rrt) a = rrt $ readFromSubjectM $ return a;

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

    instance MonadIO (Readable reader) where
    {
        liftIO ioa = MkReadable $ \_ -> liftIO ioa;
    };

    instance IsReadableMonad (Readable reader) where
    {
        type RMReader (Readable reader) = reader;
        readable rt = MkReadable (\s -> s rt);
    };

    readableToM :: forall m t. (IsReadableMonad m) => Readable (RMReader m) t -> m t;
    readableToM (MkReadable sma) = sma readable;

    instance (IsReadableMonad m,Monad f,Traversable f) => IsReadableMonad (Compose m f) where
    {
        type RMReader (Compose m f) = RMReader m;
        readable rt = Compose $ fmap pure $ readable rt;
    };

    nestReadable :: Readable reader t -> Readable (Readable reader) t;
    nestReadable rbl = MkReadable $ \mr -> mr rbl;
}
