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

    instance HasInfoKTT WholeEdit where
    {
        infoKTT = MkInfoKTT
            (WitKTT (unsafeIOWitnessFromString "Truth.Edit.WholeEdit.WholeEdit"))
            (
                (mkTFactsKTT (\ta -> return (MkEditInst ta))) `mappend`
                (mkTFactsKTT (\_ -> return MkFullEditInst))
            );
    };
}
