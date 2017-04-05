module Truth.Core.Read.FullReader where
{
    import Truth.Core.Import;
    import Truth.Core.Read.Reader;
    import Truth.Core.Read.Readable;


    class (Reader reader) => FullReader (reader :: * -> *) where
    {
        -- | Construct the subject by making API calls
        fromReader :: Readable reader (ReaderSubject reader);
    };

    instance HasInfo FullReader where
    {
        info = mkSimpleInfo $(iowitness[t|FullReader|]) [];
    };
}
