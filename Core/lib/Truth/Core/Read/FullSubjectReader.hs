module Truth.Core.Read.FullSubjectReader where
{
    import Truth.Core.Import;
    import Truth.Core.Read.SubjectReader;
    import Truth.Core.Read.Readable;


    class (SubjectReader reader) => FullSubjectReader (reader :: * -> *) where
    {
        -- | Construct the subject by making MutableEdit calls
        ;
        subjectFromReader :: Readable reader (ReaderSubject reader);
    };
}
