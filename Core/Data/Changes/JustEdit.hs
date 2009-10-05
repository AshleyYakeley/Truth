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
    import Data.Witness;
    import Prelude;

    newtype JustEdit f edit = MkJustEdit edit;

    instance HasTypeRepKKTTKTT JustEdit where
    {
        typeRepKKTTKTT = EditRepKKTTKTT (unsafeIOWitnessFromString "Data.Changes.JustEdit.JustEdit");
    };

    instance (HasTypeRepKTT f,HasNewValue (Subject edit),FullEdit edit,FunctorOne f) => Edit (JustEdit f edit) where
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

    justWholeEditRep :: forall f edit. (Edit edit) =>
     EditRepKTT f -> EditRepT (JustRepEdit f edit);
    justWholeEditRep repF = applyEditRep
     (applyEditRep typeRepKTKTT (applyEditRep typeRepKTT (applyEditRep repF typeRepT)))
     (applyEditRep (applyEditRep typeRepKKTTKTT repF) typeRepT);

    matchJustWholeEditRep ::
     forall t. (Edit t) => EditRepT t -> Maybe (forall r.
     (forall f edit. EqualType t (JustRepEdit f edit) -> EditRepKTT f -> EditRepT edit -> EditRepT (Subject edit) -> r) -> r
     );
    matchJustWholeEditRep (TEditRepT (TEditRepKTT repEither (TEditRepT repWholeEdit (TEditRepT repF repSubj))) (TEditRepT (KTTEditRepKTT repJustEdit repF') repEdit)) = do
    {
        let
        {
            pp :: EditInst t;
            pp = MkEditInst;
        };
        MkEqualType <- matchEditRepKTKTT repEither (typeRepKTKTT :: EditRepKTKTT Either);
        MkEqualType <- matchEditRepKTT repWholeEdit (typeRepKTT :: EditRepKTT WholeEdit);
        MkEqualType <- matchEditRepKKTTKTT repJustEdit (typeRepKKTTKTT :: EditRepKKTTKTT JustEdit);
        MkEqualType <- matchEditRepKTT repF repF';
        let
        {
            repSubj' = ((\MkFullEditInst -> typeRepT) . foo5) pp;
        };
        et <- matchWitness repSubj repSubj';
        return (\fr -> fr (foo6 et) repF repEdit repSubj');
    } where
    {
        foo1 :: forall x. EditInst x -> EditEvidence x;
        foo1 ei@MkEditInst = editEvidence ei;

        foo5 :: EditInst (Either x (JustEdit f edit)) -> FullEditInst edit;
        foo5 e = e2 where
        {
            (_,e1) = foo1 e;
            (_,_,e2) = foo1 e1;
        };

        foo6 :: EqualType subj (Subject edit) -> EqualType (Either (WholeEdit (f subj)) (JustEdit f edit)) (JustRepEdit f edit);
        foo6 MkEqualType = MkEqualType;
    };
    matchJustWholeEditRep _ = Nothing;

    extractJustEdit :: forall f edit. (FunctorOne f,FullEdit edit) => JustRepEdit f edit -> Maybe edit;
    extractJustEdit (Right (MkJustEdit edit)) = Just edit;
    extractJustEdit (Left (MkWholeEdit fa)) = case retrieveOne fa of
    {
        SuccessResult a -> Just (replaceEdit a);
        _ -> Nothing;
    };
}
