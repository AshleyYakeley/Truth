module Truth.Edit.JustEdit where
{
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    newtype JustEdit (f :: * -> *) edit = MkJustEdit edit;

    instance (FunctorOne f,Edit edit) => Edit (JustEdit f edit) where
    {
        type Subject (JustEdit f edit) = f (Subject edit);

        applyEdit (MkJustEdit edita) = cfmap (applyEdit edita);

        invertEdit (MkJustEdit edita) molda = case retrieveOne molda of
        {
            SuccessResult olda -> fmap MkJustEdit (invertEdit edita olda);
            _ -> Nothing;
        };
    };

    instance HasInfoKKTTKTT JustEdit where
    {
        infoKKTTKTT = MkInfoKKTTKTT
            (WitKKTTKTT (unsafeIOWitnessFromString "Truth.Edit.JustEdit.JustEdit"))
            (mkTFactsKKTTKTT (\tf tedit -> do
                {
                    MkEditInst tsubj <- matchPropertyT tedit;
                    MkFullEditInst <- matchPropertyT tedit;
                    MkHasNewValueInst <- matchPropertyT tsubj;
                    MkFunctorOneInst <- matchPropertyKTT tf;
                    return (MkEditInst (applyTInfoT tf tsubj));
                })
            );
    };
}
