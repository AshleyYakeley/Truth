module Truth.Core.Read.WriterReadable where
{
    import Truth.Core.Import;
    import Truth.Core.Read.Reader;
    import Truth.Core.Read.Readable;


    -- | PureWriterReadable allows copying without reading the whole thing into memory
    ;
    newtype WriterReadable c w reader a = MkWriterReadable { unWriterReadable :: forall m. (Monad m,c m) => MutableRead m reader -> (w -> m ()) -> m a};
    type PureWriterReadable = WriterReadable Monad;
    type IOWriterReadable = WriterReadable MonadIO;

    writerReadableToGen :: PureWriterReadable w reader a -> WriterReadable c w reader a;
    writerReadableToGen (MkWriterReadable f) = MkWriterReadable f;

    instance Functor (WriterReadable c w reader) where
    {
        fmap ab (MkWriterReadable sma) = MkWriterReadable (\s wm -> fmap ab (sma s wm));
    };

    instance Applicative (WriterReadable c w reader) where
    {
        pure a = MkWriterReadable $ \_ _ -> pure a;
        (MkWriterReadable smab) <*> (MkWriterReadable sma) = MkWriterReadable $ \s wm -> smab s wm <*> sma s wm;
    };

    instance Monad (WriterReadable c w reader) where
    {
        return = pure;
        (MkWriterReadable sma) >>= f = MkWriterReadable $ \s wm -> do
        {
            a <- sma s wm;
            unWriterReadable (f a) s wm;
        };
    };

    instance MonadIO (IOWriterReadable w reader) where
    {
        liftIO ioa = MkWriterReadable $ \_ _ -> liftIO ioa;
    };

    instance IsReadableMonad (WriterReadable c w reader) where
    {
        type RMReader (WriterReadable c w reader) = reader;
        readable rt = MkWriterReadable (\s _ -> s rt);
    };

    wrWrite :: w -> WriterReadable c w reader ();
    wrWrite w = MkWriterReadable $ \_ wm -> wm w;

    reWriterReadable :: (wa -> wb) -> WriterReadable c wa reader a -> WriterReadable c wb reader a;
    reWriterReadable f (MkWriterReadable swma) = MkWriterReadable $ \s wm -> swma s (wm . f);
}
