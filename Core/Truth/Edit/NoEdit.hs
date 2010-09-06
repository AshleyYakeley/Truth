module Truth.Edit.NoEdit where
{
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    newtype NoEdit a = MkNoEdit Nothing deriving (Eq,Countable,Searchable,Finite,Empty);

    instance Edit (NoEdit a) where
    {
        type Subject (NoEdit a) = a;
        applyEdit = never;
        invertEdit = never;
    };

    instance HasInfo (Type_KTT NoEdit) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_KTT NoEdit |])
        [
            -- instance () => EditInst (NoEdit a)
            mkFacts (MkFactS (\a -> MkFactZ (do
            {
                Kind_T <- matchProp $(type1[t|Kind_T|]) a;
                return (Edit_Inst a);
            }))
            :: FactS FactZ Edit_Inst (Type_KTT NoEdit)
            )
        ];
    };
}
