module Truth.Core.Read
(
    module Truth.Core.Read.SubjectReader,
    module Truth.Core.Read.Readable,
    module Truth.Core.Read.FullSubjectReader,
    module Truth.Core.Read.ReadFunction,
    module Truth.Core.Read.WriterReadable,
    module Truth.Core.Read,
)
 where
{
    import Truth.Core.Import;
    import Truth.Core.Read.SubjectReader;
    import Truth.Core.Read.Readable;
    import Truth.Core.Read.FullSubjectReader;
    import Truth.Core.Read.ReadFunction;
    import Truth.Core.Read.WriterReadable;


    -- not terribly useful
    data ConstReader a t where
    {
        MkConstReader :: a -> ConstReader a a;
    };

    -- only reason to specify readFromSubjectM instead of readFromSubject?
    instance SubjectReader (ConstReader a) where
    {
        type ReaderSubject (ConstReader a) = a;
        readFromSubjectM _ (MkConstReader a) = return a;
    };
}
