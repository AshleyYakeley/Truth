module Data.Changes.WholeEdit where
{
    import Data.Changes.EditScheme;
    import Data.Changes.HasTypeRep;
    import Data.Changes.EditRep;
    import Data.OpenWitness;
    import Control.Applicative;

    newtype WholeEdit a = MkWholeEdit a;
    
    instance HasTypeRepKTT WholeEdit where
    {
        typeRepKTT = EditRepKTT (unsafeIOWitnessFromString "Data.Changes.WholeEdit.WholeEdit");
    };

    instance Edit (WholeEdit a) where
    {
        type Subject (WholeEdit a) = a;
        applyEdit (MkWholeEdit a) = pure a;
        invertEdit _ = Just . MkWholeEdit;
        replaceEdit = MkWholeEdit;
    };
}
