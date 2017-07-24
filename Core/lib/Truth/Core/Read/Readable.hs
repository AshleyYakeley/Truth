module Truth.Core.Read.Readable where
{
    import Truth.Core.Import;
    import Truth.Core.Read.Reader;


    class Monad m => IsReadableMonad m where
    {
        type RMReader m :: * -> *;
        readable :: forall t. RMReader m t -> m t;
    };


    newtype GenReadable c reader a = MkReadable { unReadable :: forall m. (Monad m,c m) => MutableRead m reader -> m a};

    type Readable = GenReadable Monad;
    type IOReadable = GenReadable MonadIO;

    readableToGen :: Readable reader a -> GenReadable c reader a;
    readableToGen (MkReadable f) = MkReadable f;

    fromGenReadable :: (Reader reader,c m,Monad m) => GenReadable c reader t -> ReaderSubject reader -> m t;
    fromGenReadable (MkReadable rrt) a = rrt $ readFromM $ return a;

    fromReadable :: (Reader reader,c Identity) => GenReadable c reader t -> ReaderSubject reader -> t;
    fromReadable (MkReadable rrt) a = runIdentity $ rrt $ Identity . (readFrom a);

    instance Functor (GenReadable c reader) where
    {
        fmap ab (MkReadable sma) = MkReadable (\s -> fmap ab (sma s));
    };

    instance Applicative (GenReadable c reader) where
    {
        pure a = MkReadable $ \_ -> pure a;
        (MkReadable smab) <*> (MkReadable sma) = MkReadable $ \s -> smab s <*> sma s;
    };

    instance Monad (GenReadable c reader) where
    {
        return = pure;
        (MkReadable sma) >>= f = MkReadable $ \s -> do
        {
            a <- sma s;
            unReadable (f a) s;
        };
    };

    instance MonadIO (GenReadable MonadIO reader) where
    {
        liftIO ioa = MkReadable $ \_ -> liftIO ioa;
    };

    instance IsReadableMonad (GenReadable c reader) where
    {
        type RMReader (GenReadable c reader) = reader;
        readable rt = MkReadable (\s -> s rt);
    };

    readableToM :: (IsReadableMonad m,c m) => GenReadable c (RMReader m) t -> m t;
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


    instance Reader reader => Reader (Readable reader) where
    {
        type ReaderSubject (Readable reader) = ReaderSubject reader;

        readFrom subj rbl = fromReadable rbl subj;
        readFromM msubj rbl = unReadable rbl $ readFromM msubj;
    };

    nestReadable :: Readable reader t -> Readable (Readable reader) t;
    nestReadable rbl = MkReadable $ \mr -> mr rbl;
}
