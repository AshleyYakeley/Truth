module Data.Changes.WholeEdit where
{
    import Data.Changes.Edit;
    import Data.Changes.HasTypeRep;
    import Data.Changes.EditRep;
    import Data.TypeKT.WitnessKT;
    import Data.OpenWitness;
    import Data.Witness;
    import Control.Applicative;

    newtype WholeEdit a = MkWholeEdit a;

    instance HasTypeRepKTT WholeEdit where
    {
        typeRepKTT = EditRepKTT (unsafeIOWitnessFromString "Data.Changes.WholeEdit.WholeEdit");
    };

    data WholeEditStructure edit where
    {
        MkWholeEditStructure :: forall a. (HasTypeRepT a) => EditRepT a -> WholeEditStructure (WholeEdit a);
    };

    instance (HasTypeRepT a) => Edit (WholeEdit a) where
    {
        type Subject (WholeEdit a) = a;
        applyEdit (MkWholeEdit a) = pure a;
        invertEdit _ = Just . MkWholeEdit;

        type EditStructure (WholeEdit a) = WholeEditStructure;
        editStructure = (MkWholeEditStructure typeRepT);
        matchEditStructure _ (TEditRepT repF repA) = do
        {
            MkEqualType <- matchWitnessKTT repF (typeRepKTT :: EditRepKTT WholeEdit);
            MkEqualType <- matchWitnessT repA (typeRepT :: EditRepT a);
            return (MkWholeEditStructure repA);
        };
        matchEditStructure _ _ = Nothing;
    };

    instance (HasTypeRepT a) => FullEdit (WholeEdit a) where
    {
        replaceEdit = MkWholeEdit;
    };
}
