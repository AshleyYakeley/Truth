module Data.Changes.WholeEdit where
{
    import Data.Changes.Edit;
    import Data.Changes.HasTypeRep;
    import Data.Changes.EditRep;
    import Data.OpenWitness;
    import Control.Applicative;

    newtype WholeEdit a = MkWholeEdit a;

    instance HasTypeRepKTT WholeEdit where
    {
        typeRepKTT = EditRepKTT (unsafeIOWitnessFromString "Data.Changes.WholeEdit.WholeEdit");
    };

    data WholeEditStructure edit where
    {
        MkWholeEditStructure :: forall a. (HasTypeRepT a) => WholeEditStructure (WholeEdit a);
    };

    instance (HasTypeRepT a) => Edit (WholeEdit a) where
    {
        type Subject (WholeEdit a) = a;
        applyEdit (MkWholeEdit a) = pure a;
        invertEdit _ = Just . MkWholeEdit;

        type EditStructure (WholeEdit a) = WholeEditStructure;
        editStructure = MkWholeEditStructure;
        matchEditStructure
    };

    instance (HasTypeRepT a) => FullEdit (WholeEdit a) where
    {
        replaceEdit = MkWholeEdit;
    };
}
