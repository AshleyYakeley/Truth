module Truth.Edit.Read.FullReader where
{
    import Truth.Edit.Read.Readable;
    import Truth.Edit.Read.Reader;
    import Truth.Edit.Import;

    class (Reader reader) => FullReader (reader :: * -> *) where
    {
        -- | Construct the subject by making API calls
        fromReader :: Readable reader (Subject reader);
    };

    instance HasInfo FullReader where
    {
        info = mkSimpleInfo $(iowitness[t|WrapType FullReader|]) [];
    };

    type FullReader_Inst a = ConstraintFact FullReader (WrapType a);
}
