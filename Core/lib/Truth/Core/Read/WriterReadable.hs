module Truth.Core.Read.WriterReadable where
{
    import Truth.Core.Import;
    import Truth.Core.Read.Reader;
    import Truth.Core.Read.Readable;
    import Truth.Core.Read.ReadFunction;


    -- | WriterReadable allows copying without reading the whole thing into memory
    newtype GenWriterReadable c w reader a = MkWriterReadable { unWriterReadable :: forall m. (Monad m,c m) => MutableRead m reader -> (w -> m ()) -> m a};
    type WriterReadable = GenWriterReadable Monad;
    type IOWriterReadable = GenWriterReadable MonadIO;

    writerReadableToGen :: WriterReadable w reader a -> GenWriterReadable c w reader a;
    writerReadableToGen (MkWriterReadable f) = MkWriterReadable f;

    instance Functor (GenWriterReadable c w reader) where
    {
        fmap ab (MkWriterReadable sma) = MkWriterReadable (\s wm -> fmap ab (sma s wm));
    };

    instance Applicative (GenWriterReadable c w reader) where
    {
        pure a = MkWriterReadable $ \_ _ -> pure a;
        (MkWriterReadable smab) <*> (MkWriterReadable sma) = MkWriterReadable $ \s wm -> smab s wm <*> sma s wm;
    };

    instance Monad (GenWriterReadable c w reader) where
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

    instance IsReadableMonad (GenWriterReadable c w reader) where
    {
        type RMReader (GenWriterReadable c w reader) = reader;
        readable rt = MkWriterReadable (\s _ -> s rt);
    };

    instance MapReadable (WriterReadable w) where
    {
        mapReadable rf (MkWriterReadable sbwt) = MkWriterReadable $ \sa wm -> sbwt (mapMutableRead rf sa) wm;
        mapReadableF rff (MkWriterReadable sbwt) = MkWriterReadable $ \sa wm -> getCompose $ sbwt (mapStructureF rff sa) (fmap (MkCompose . fmap pure) wm);
    };

    instance MapReadable (IOWriterReadable w) where
    {
        mapReadable rf (MkWriterReadable sbwt) = MkWriterReadable $ \sa wm -> sbwt (mapMutableRead rf sa) wm;
        mapReadableF rff (MkWriterReadable sbwt) = MkWriterReadable $ \sa wm -> getCompose $ sbwt (mapStructureF rff sa) (fmap (MkCompose . fmap pure) wm);
    };

    wrWrite :: w -> GenWriterReadable c w reader ();
    wrWrite w = MkWriterReadable $ \_ wm -> wm w;

    writerToReadable :: WriterReadable w reader () -> GenReadable c reader [w];
    writerToReadable (MkWriterReadable swma) = MkReadable $ \s -> execWriterT $ swma (fmap lift s) (tell . pure);

    ioWriterToReadable :: IOWriterReadable w reader () -> IOReadable reader [w];
    ioWriterToReadable (MkWriterReadable swma) = MkReadable $ \s -> execWriterT $ swma (fmap lift s) (tell . pure);

    reWriterReadable :: (wa -> wb) -> GenWriterReadable c wa reader a -> GenWriterReadable c wb reader a;
    reWriterReadable f (MkWriterReadable swma) = MkWriterReadable $ \s wm -> swma s (wm . f);
}
