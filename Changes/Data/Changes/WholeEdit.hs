module Data.Changes.WholeEdit where
{
    import Data.Changes.EditScheme;
    import Data.Changes.HasTypeRep;
    import Data.OpenWitness.OpenRep;
    import Data.OpenWitness;
    import Control.Applicative;

    newtype WholeEdit a = MkWholeEdit a;
    
    instance HasTypeRep1 WholeEdit where
    {
        typeRep1 = SimpleOpenRep1 (unsafeIOWitnessFromString "Data.Changes.WholeEdit.WholeEdit");
    };

    instance EditScheme a (WholeEdit a) where
    {
        applyEdit (MkWholeEdit a) = pure a;
        invertEdit _ = Just . MkWholeEdit;
    };
    
    instance CompleteEditScheme a (WholeEdit a) where
    {
        replaceEdit = MkWholeEdit;
    };
}
