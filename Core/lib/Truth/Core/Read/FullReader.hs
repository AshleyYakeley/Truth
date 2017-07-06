module Truth.Core.Read.FullReader where
{
    import Truth.Core.Import;
    import Truth.Core.Read.Reader;
    import Truth.Core.Read.Readable;


    class (Reader reader) => IOFullReader (reader :: * -> *) where
    {
        -- | Construct the subject by making MutableEdit calls
        ioFromReader :: IOReadable reader (ReaderSubject reader);

        default ioFromReader :: FullReader reader => IOReadable reader (ReaderSubject reader);
        ioFromReader = MkReadable $ unReadable fromReader;
    };

    class (IOFullReader reader) => FullReader (reader :: * -> *) where
    {
        -- | Construct the subject by making MutableEdit calls
        fromReader :: Readable reader (ReaderSubject reader);
    };

    instance HasInfo IOFullReader where
    {
        info = mkSimpleInfo $(iowitness[t|IOFullReader|]) [];
    };

    instance HasInfo FullReader where
    {
        info = mkSimpleInfo $(iowitness[t|FullReader|]) [];
    };

    instance IOFullReader reader => IOFullReader (Readable reader) where
    {
        ioFromReader = MkReadable $ \mr -> unReadable ioFromReader $ \rt -> mr $ readable rt;
    };

    instance FullReader reader => FullReader (Readable reader) where
    {
        fromReader = nestReadable fromReader;
    };
}
