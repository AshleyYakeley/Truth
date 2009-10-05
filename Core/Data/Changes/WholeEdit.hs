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

    instance (HasTypeRepT a) => Edit (WholeEdit a) where
    {
        type Subject (WholeEdit a) = a;
        applyEdit (MkWholeEdit a) = pure a;
        invertEdit _ = Just . MkWholeEdit;

        type EditEvidence (WholeEdit a) = ();
        editEvidence _ = ();
    };

    instance (HasTypeRepT a) => FullEdit (WholeEdit a) where
    {
        replaceEdit = MkWholeEdit;
    };
}
