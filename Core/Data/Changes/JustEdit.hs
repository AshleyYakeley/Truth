module Data.Changes.JustEdit where
{
    import Data.Changes.Edit;
    import Data.Changes.HasTypeRep;
    import Data.Changes.EditRep;
    import Data.Changes.HasNewValue;
    import Data.FunctorOne;
    import Data.Result;
    import Data.Chain;
    import Data.OpenWitness;
    import Control.Applicative;
    import Prelude hiding (id,(.));

    data JustRepEdit f edit = ReplaceJustEdit (f (Subject edit)) | JustEdit edit;
    
    instance HasTypeRepKKTTKTT JustRepEdit where
    {
        typeRepKKTTKTT = EditRepKKTTKTT (unsafeIOWitnessFromString "Data.Changes.JustEdit.JustEdit");
    };

    instance (HasNewValue (Subject edit),FullEdit edit,FunctorOne f) => Edit (JustRepEdit f edit) where
    {
        type Subject (JustRepEdit f edit) = f (Subject edit);
    
        applyEdit (ReplaceJustEdit a) = pure a;
        applyEdit (JustEdit edita) = cfmap (applyEdit edita);

        invertEdit (ReplaceJustEdit _) a = Just (ReplaceJustEdit a);
        invertEdit (JustEdit edita) molda = case retrieveOne molda of
        {
            SuccessResult olda -> fmap JustEdit (invertEdit edita olda);
            _ -> Nothing;
        };

        type EditEvidence (JustRepEdit f edit) = (HasNewValueInst (Subject edit),FunctorOneInst f,FullEditInst edit);
        editEvidence _ = (MkHasNewValueInst,MkFunctorOneInst,MkFullEditInst);
    };

    instance (HasNewValue (Subject edit),FullEdit edit,FunctorOne f) => FullEdit (JustRepEdit f edit) where
    {
        replaceEdit = ReplaceJustEdit;
    };

    extractJustEdit :: forall f edit. (FunctorOne f,FullEdit edit) => JustRepEdit f edit -> Maybe edit;
    extractJustEdit (JustEdit edit) = Just edit;
    extractJustEdit (ReplaceJustEdit fa) = case retrieveOne fa of
    {
        SuccessResult a -> Just (replaceEdit a);
        _ -> Nothing;
    };
}
