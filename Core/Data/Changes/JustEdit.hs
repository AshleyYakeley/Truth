module Data.Changes.JustEdit where
{
    import Data.Changes.WholeEdit;
    import Data.Changes.Edit;
    import Data.Changes.HasTypeRep;
    import Data.Changes.EditRep;
    import Data.Changes.HasNewValue;
    import Data.FunctorOne;
    import Data.Result;
    import Data.Chain;
    import Data.OpenWitness;
    import Prelude hiding (id,(.));

    newtype JustEdit f edit = MkJustEdit edit;

    instance HasTypeRepKKTTKTT JustEdit where
    {
        typeRepKKTTKTT = EditRepKKTTKTT (unsafeIOWitnessFromString "Data.Changes.JustEdit.JustEdit");
    };

    instance (HasNewValue (Subject edit),FullEdit edit,FunctorOne f) => Edit (JustEdit f edit) where
    {
        type Subject (JustEdit f edit) = f (Subject edit);

        applyEdit (MkJustEdit edita) = cfmap (applyEdit edita);

        invertEdit (MkJustEdit edita) molda = case retrieveOne molda of
        {
            SuccessResult olda -> fmap MkJustEdit (invertEdit edita olda);
            _ -> Nothing;
        };

        type EditEvidence (JustEdit f edit) = (HasNewValueInst (Subject edit),FunctorOneInst f,FullEditInst edit);
        editEvidence _ = (MkHasNewValueInst,MkFunctorOneInst,MkFullEditInst);
    };

    type JustRepEdit f edit = Either (WholeEdit (f (Subject edit))) (JustEdit f edit);

    justWholeEditRep :: EditRepKTT f -> EditRepT edit -> EditRepT (Subject edit) -> EditRepT (JustRepEdit f edit);
    justWholeEditRep repF repEdit repSubj = applyEditRep
     (applyEditRep typeRepKTKTT (applyEditRep typeRepKTT (applyEditRep repF repSubj)))
     (applyEditRep (applyEditRep typeRepKKTTKTT repF) repEdit);

    extractJustEdit :: forall f edit. (FunctorOne f,FullEdit edit) => JustRepEdit f edit -> Maybe edit;
    extractJustEdit (Right (MkJustEdit edit)) = Just edit;
    extractJustEdit (Left (MkWholeEdit fa)) = case retrieveOne fa of
    {
        SuccessResult a -> Just (replaceEdit a);
        _ -> Nothing;
    };
}
