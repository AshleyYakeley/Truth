module Truth.Edit.NoEdit where
{
    import Truth.Edit.Edit;
    import Truth.Edit.Read;
    import Truth.Edit.Import;

    newtype NoReader (a :: *) (t :: *) = MkNoReader Nothing deriving (Eq,Countable,Searchable,Finite,Empty);

    instance Reader (NoReader a) where
    {
        type Subject (NoReader a) = a;
        readFromM _ = never;
        readFrom _ = never;
    };

    instance HasInfo (Type_KTKTT NoReader) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_KTKTT NoReader |])
        [
            -- instance () => Reader_Inst (NoReader a)
            mkFacts (MkFactS (\a -> MkFactZ (do
            {
                Kind_T <- matchProp $(type1[t|Kind_T|]) a;
                return (Reader_Inst a);
            }))
            :: FactS FactZ Reader_Inst (Type_KTKTT NoReader)
            )
        ];
    };

    -- | Can't touch this.
    newtype NoEdit (reader :: * -> *) = MkNoEdit Nothing deriving (Eq,Countable,Searchable,Finite,Empty);

    instance (Reader reader) => Edit (NoEdit reader) where
    {
        type EditReader (NoEdit reader) = reader;
        applyEdit = never;
        invertEdit = never;
    };

    instance HasInfo (Type_KKTTT NoEdit) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_KKTTT NoEdit |])
        [
            -- instance () => EditInst (NoEdit a)
            mkFacts (MkFactS (\reader -> MkFactZ (do
            {
                Kind_KTT <- matchProp $(type1[t|Kind_KTT|]) reader;
                Reader_Inst _subject <- matchProp $(type1[t|Reader_Inst|]) reader;
                return (Edit_Inst reader);
            }))
            :: FactS FactZ Edit_Inst (Type_KKTTT NoEdit)
            )
        ];
    };
}
