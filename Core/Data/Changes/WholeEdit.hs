module Data.Changes.WholeEdit where
{
    import Data.Changes.Edit;
    import Data.TypeKT;
    import Data.OpenWitness;
    import Control.Applicative;
    import Data.Monoid;

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

    instance HasTypeKTT WholeEdit where
    {
        typeKTT = MkTypeKTT
            (WitKTT (unsafeIOWitnessFromString "Data.Changes.WholeEdit.WholeEdit"))
            (
                (mkTInfoKTT (\ta -> return (MkEditInst ta))) `mappend`
                (mkTInfoKTT (\_ -> return MkFullEditInst))
            );
    };
}
