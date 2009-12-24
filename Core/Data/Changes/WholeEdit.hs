module Data.Changes.WholeEdit where
{
    import Data.Changes.Edit;
    import Data.TypeKT;
    import Data.OpenWitness;
    import Control.Applicative;

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
            (WitKTT (unsafeIOWitnessFromString "Data.Changes.WholeEdit.WholeEdit"))
            (
                (mkTFactsKTT (\ta -> return (MkEditInst ta))) `mappend`
                (mkTFactsKTT (\_ -> return MkFullEditInst))
            );
    };
}
