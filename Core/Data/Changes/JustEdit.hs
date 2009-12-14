module Data.Changes.JustEdit where
{
    import Data.Changes.Edit;
    import Data.TypeKT;
    import Data.Changes.HasNewValue;
    import Data.FunctorOne;
    import Data.Result;
    import Data.Chain;
    import Data.OpenWitness;

    newtype JustEdit (f :: * -> *) edit = MkJustEdit edit;

    instance (HasNewValue (Subject edit),FullEdit edit,FunctorOne f) => Edit (JustEdit f edit) where
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
            (WitKKTTKTT (unsafeIOWitnessFromString "Data.Changes.JustEdit.JustEdit"))
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
