module Truth.Edit.WholeEdit where
{
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    newtype WholeEdit a = MkWholeEdit a;

    instance Edit (WholeEdit a) where
    {
        type Subject (WholeEdit a) = a;
        applyEdit (MkWholeEdit a) = pure a;
        invertEdit _ = Just . MkWholeEdit;
    };

    instance FullEdit (WholeEdit a) where
    {
        replaceEdit = MkWholeEdit;
    };

    instance HasInfo (Type_KTT WholeEdit) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_KTT WholeEdit |])
        [
            -- instance Edit (WholeEdit a)
            mkFacts (MkFactS (\a -> MkFactZ (do
            {
                Kind_T <- matchProp $(type1[t|Kind_T|]) a;
                return (Edit_Inst a);
            }))
            :: FactS FactZ Edit_Inst (Type_KTT WholeEdit)
            ),
            mkFacts (MkFactS (\a -> MkFactZ (do
            {
                Kind_T <- matchProp $(type1[t|Kind_T|]) a;
                return FullEdit_Inst;
            }))
            :: FactS FactZ FullEdit_Inst (Type_KTT WholeEdit)
            )
        ];
    };
}
