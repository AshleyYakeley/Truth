module Truth.Core.Read.WriterReadable where
{
    import Truth.Core.Import;
    import Truth.Core.Read.Reader;
    import Truth.Core.Read.Readable;
    import Truth.Core.Read.ReadFunction;


    -- | WriterReadable allows copying without reading the whole thing into memory
    newtype WriterReadable w reader a = MkWriterReadable { unWriterReadable :: forall m. Monad m => Structure m reader -> (w -> m ()) -> m a};

    instance Functor (WriterReadable w reader) where
    {
        fmap ab (MkWriterReadable sma) = MkWriterReadable (\s wm -> fmap ab (sma s wm));
    };

    instance Applicative (WriterReadable w reader) where
    {
        pure a = MkWriterReadable $ \_ _ -> pure a;
        (MkWriterReadable smab) <*> (MkWriterReadable sma) = MkWriterReadable $ \s wm -> smab s wm <*> sma s wm;
    };

    instance Monad (WriterReadable w reader) where
    {
        return = pure;
        (MkWriterReadable sma) >>= f = MkWriterReadable $ \s wm -> do
        {
            a <- sma s wm;
            unWriterReadable (f a) s wm;
        };
    };

    instance IsReadableMonad (WriterReadable w reader) where
    {
        type RMReader (WriterReadable w reader) = reader;
        readable rt = MkWriterReadable (\s _ -> s rt);
    };

    instance MapReadable (WriterReadable w) where
    {
        mapReadable rf (MkWriterReadable sbwt) = MkWriterReadable $ \sa wm -> sbwt (mapStructure rf sa) wm;
        mapReadableF rff (MkWriterReadable sbwt) = MkWriterReadable $ \sa wm -> getCompose $ sbwt (mapStructureF rff sa) (fmap (MkCompose . fmap pure) wm);
    };

    wrWrite :: w -> WriterReadable w reader ();
    wrWrite w = MkWriterReadable $ \_ wm -> wm w;

    writerToReadable :: WriterReadable w reader () -> Readable reader [w];
    writerToReadable (MkWriterReadable swma) = MkReadable $ \s -> execWriterT $ swma (fmap lift s) (tell . pure);

    reWriterReadable :: (wa -> wb) -> WriterReadable wa reader a -> WriterReadable wb reader a;
    reWriterReadable f (MkWriterReadable swma) = MkWriterReadable $ \s wm -> swma s (wm . f);
}
