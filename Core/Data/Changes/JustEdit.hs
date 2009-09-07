module Data.Changes.JustEdit where
{
    import Data.Changes.EditScheme;
    import Data.Changes.HasTypeRep;
    import Data.Changes.EditRep;
    import Data.Changes.HasNewValue;
    import Data.FunctorOne;
    import Data.Result;
    import Data.Chain;
    import Data.OpenWitness;
    import Control.Applicative;
    import Prelude hiding (id,(.));

    data JustEdit f edit = ReplaceJustEdit (f (Subject edit)) | JustEdit edit;
    
    instance HasTypeRepKKTTKTT JustEdit where
    {
        typeRepKKTTKTT = EditRepKKTTKTT (unsafeIOWitnessFromString "Data.Changes.JustEdit.JustEdit");
    };

    instance (HasNewValue (Subject edit),Edit edit,FunctorOne f) => Edit (JustEdit f edit) where
    {
        type Subject (JustEdit f edit) = f (Subject edit);
    
        applyEdit (ReplaceJustEdit a) = pure a;
        applyEdit (JustEdit edita) = cfmap (applyEdit edita);

        invertEdit (ReplaceJustEdit _) a = Just (ReplaceJustEdit a);
        invertEdit (JustEdit edita) molda = case retrieveOne molda of
        {
            SuccessResult olda -> fmap JustEdit (invertEdit edita olda);
            _ -> Nothing;
        };

        replaceEdit = ReplaceJustEdit;

        type EditEvidence (JustEdit f edit) = (HasNewValueInst (Subject edit),FunctorOneInst f,EditInst edit);
        editEvidence _ = (MkHasNewValueInst,MkFunctorOneInst,MkEditInst);
    };

    extractJustEdit :: forall f edit. (FunctorOne f,Edit edit) => JustEdit f edit -> Maybe edit;
    extractJustEdit (JustEdit edit) = Just edit;
    extractJustEdit (ReplaceJustEdit fa) = case retrieveOne fa of
    {
        SuccessResult a -> Just (replaceEdit a);
        _ -> Nothing;
    };
}
