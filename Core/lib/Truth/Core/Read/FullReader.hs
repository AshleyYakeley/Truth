module Truth.Core.Read.FullReader where
{
    import Truth.Core.Import;
    import Truth.Core.Read.Reader;
    import Truth.Core.Read.Readable;


    class (Reader reader) => FullReader (reader :: * -> *) where
    {
        -- | Construct the subject by making MutableEdit calls
        ;
        fromReader :: Readable reader (ReaderSubject reader);
    };
}
