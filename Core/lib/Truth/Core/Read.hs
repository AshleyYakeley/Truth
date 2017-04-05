module Truth.Core.Read
(
    module Truth.Core.Read.Reader,
    module Truth.Core.Read.Readable,
    module Truth.Core.Read.FullReader,
    module Truth.Core.Read.ReadFunction,
    module Truth.Core.Read,
)
 where
{
    import Truth.Core.Import;
    import Truth.Core.Read.Reader;
    import Truth.Core.Read.Readable;
    import Truth.Core.Read.FullReader;
    import Truth.Core.Read.ReadFunction;


    -- not terribly useful
    data ConstReader a t where
    {
        MkConstReader :: a -> ConstReader a a;
    };

    -- only reason to specify readFromM instead of readFrom?
    instance Reader (ConstReader a) where
    {
        type ReaderSubject (ConstReader a) = a;
        readFromM _ (MkConstReader a) = return a;
    };
}
