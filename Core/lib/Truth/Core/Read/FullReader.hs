module Truth.Core.Read.FullReader where
{
    import Truth.Core.Import;
    import Truth.Core.Read.Reader;
    import Truth.Core.Read.Readable;


    class (Reader reader) => GenFullReader c (reader :: * -> *) where
    {
        -- | Construct the subject by making MutableEdit calls
        genFromReader :: GenReadable c reader (ReaderSubject reader);

        default genFromReader :: (IOFullReader reader,FullReader reader) => IOReadable reader (ReaderSubject reader);
        genFromReader = MkReadable $ unReadable ioFromReader;
    };

    type IOFullReader = GenFullReader MonadIO;
    type FullReader = GenFullReader Monad;

    ioFromReader :: IOFullReader reader => IOReadable reader (ReaderSubject reader);
    ioFromReader = genFromReader;

    fromReader :: FullReader reader => Readable reader (ReaderSubject reader);
    fromReader = genFromReader;

    instance HasTypeInfo GenFullReader where
    {
        typeWitness = $(generateWitness [t|GenFullReader|]);
        typeName _ = "GenFullReader";
    };

    instance GenFullReader c reader => GenFullReader c (Readable reader) where
    {
        genFromReader = MkReadable $ \mr -> unReadable (genFromReader @c @reader) $ \rt -> mr $ readable rt;
    };
}
