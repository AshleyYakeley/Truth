module Truth.Edit.Read
(
    module Truth.Edit.Read.Reader,
    module Truth.Edit.Read.FullReader,
    module Truth.Edit.Read.Readable,
    module Truth.Edit.Read.ReadFunction,
    module Truth.Edit.Read,
)
 where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read.Reader;
    import Truth.Edit.Read.FullReader;
    import Truth.Edit.Read.Readable;
    import Truth.Edit.Read.ReadFunction;


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
