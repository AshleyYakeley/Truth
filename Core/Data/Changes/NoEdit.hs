module Data.Changes.NoEdit where
{
    import Data.Changes.Edit;
    import Data.TypeKT;
    import Data.OpenWitness;
    import Data.Nothing;

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
            (WitKTT (unsafeIOWitnessFromString "Data.Changes.Edit.NoEdit"))
            (mkTFactsKTT (\ta -> return (MkEditInst ta)));
    };
}
