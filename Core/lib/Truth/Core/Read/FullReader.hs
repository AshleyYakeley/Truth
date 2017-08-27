module Truth.Core.Read.FullReader where
{
    import Truth.Core.Import;
    import Truth.Core.Read.Reader;
    import Truth.Core.Read.Readable;


    class (Reader reader) => FullReader c (reader :: * -> *) where
    {
        -- | Construct the subject by making MutableEdit calls
        ;
        fromReader :: Readable c reader (ReaderSubject reader);
    };

    type IOFullReader = FullReader MonadIO;
    type PureFullReader = FullReader Monad;

    ioFromReader :: IOFullReader reader => IOReadable reader (ReaderSubject reader);
    ioFromReader = fromReader;

    pureFromReader :: PureFullReader reader => PureReadable reader (ReaderSubject reader);
    pureFromReader = fromReader;

    instance HasTypeInfo FullReader where
    {
        typeWitness = $(generateWitness [t|FullReader|]);
        typeName _ = "FullReader";
    };

    instance FullReader c reader => FullReader c (PureReadable reader) where
    {
        fromReader = MkReadable $ \mr -> unReadable (fromReader @c @reader) $ \rt -> mr $ readable rt;
    };
}
