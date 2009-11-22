module Data.Changes.WholeEdit where
{
    import Data.Changes.Edit;
    import Data.Changes.HasTypeRep;
    import Data.Changes.EditRep;
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
        witKTT = WitKTT (unsafeIOWitnessFromString "Data.Changes.WholeEdit.WholeEdit");
        infoKTT =
         (mkInfoKTT witEditInst (\ta -> return (MkEditInst ta))) `mappend`
         (mkInfoKTT witFullEditInst (\_ -> return MkFullEditInst));
    };
}
