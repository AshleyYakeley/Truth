module Truth.Core.Read.Readable where
{
    import Truth.Core.Import;
    import Truth.Core.Read.Reader;


    class Monad m => IsReadableMonad m where
    {
        type RMReader m :: * -> *;
        readable :: forall t. RMReader m t -> m t;
    };


    newtype Readable c reader a = MkReadable { unReadable :: forall m. (Monad m,c m) => MutableRead m reader -> m a};

    type PureReadable = Readable Monad;
    type IOReadable = Readable MonadIO;

    liftReadable :: (forall m. (Monad m,c m) => m a) -> Readable c reader a;
    liftReadable ma = MkReadable $ \_ -> ma;

    pureToReadable :: PureReadable reader a -> Readable c reader a;
    pureToReadable (MkReadable f) = MkReadable f;

    fromReadable :: (Reader reader,c m,Monad m) => Readable c reader t -> ReaderSubject reader -> m t;
    fromReadable (MkReadable rrt) a = rrt $ readFromM $ return a;

    fromPureReadable :: (Reader reader,c Identity) => Readable c reader t -> ReaderSubject reader -> t;
    fromPureReadable (MkReadable rrt) a = runIdentity $ rrt $ Identity . (readFrom a);

    instance Functor (Readable c reader) where
    {
        fmap ab (MkReadable sma) = MkReadable (\s -> fmap ab (sma s));
    };

    instance Applicative (Readable c reader) where
    {
        pure a = MkReadable $ \_ -> pure a;
        (MkReadable smab) <*> (MkReadable sma) = MkReadable $ \s -> smab s <*> sma s;
    };

    instance Monad (Readable c reader) where
    {
        return = pure;
        (MkReadable sma) >>= f = MkReadable $ \s -> do
        {
            a <- sma s;
            unReadable (f a) s;
        };
    };

    instance MonadIO (Readable MonadIO reader) where
    {
        liftIO ioa = MkReadable $ \_ -> liftIO ioa;
    };

    instance IsReadableMonad (Readable c reader) where
    {
        type RMReader (Readable c reader) = reader;
        readable rt = MkReadable (\s -> s rt);
    };

    readableToM :: forall c m t. (IsReadableMonad m,c m) => Readable c (RMReader m) t -> m t;
    readableToM (MkReadable sma) = sma readable;


    exec :: Monad m => m (m a) -> m a;
    exec mma = mma >>= id;

    type PureReadableF f reader = Compose (PureReadable reader) f;

    fromPureReadableF :: (Reader reader) => PureReadableF f reader t -> ReaderSubject reader -> f t;
    fromPureReadableF (Compose rf) = fromPureReadable rf;

    instance (IsReadableMonad m,Monad f,Traversable f) => IsReadableMonad (Compose m f) where
    {
        type RMReader (Compose m f) = RMReader m;
        readable rt = Compose $ fmap pure $ readable rt;
    };


    instance Reader reader => Reader (PureReadable reader) where
    {
        type ReaderSubject (PureReadable reader) = ReaderSubject reader;

        readFrom subj rbl = fromPureReadable rbl subj;
        readFromM msubj rbl = unReadable rbl $ readFromM msubj;
    };

    nestReadable :: Readable c reader t -> Readable c (Readable c reader) t;
    nestReadable rbl = MkReadable $ \mr -> mr rbl;
}
