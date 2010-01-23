module Truth.Edit.NoEdit where
{
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    newtype NoEdit a = MkNoEdit Nothing;

    instance Edit (NoEdit a) where
    {
        type Subject (NoEdit a) = a;
        applyEdit (MkNoEdit n) = never n;
        invertEdit (MkNoEdit n) = never n;
    };

    instance HasInfoKTT NoEdit where
    {
        infoKTT = MkInfoKTT
            (WitKTT (unsafeIOWitnessFromString "Truth.Edit.Edit.NoEdit"))
            (mkTFactsKTT (\ta -> return (MkEditInst ta)));
    };
}
