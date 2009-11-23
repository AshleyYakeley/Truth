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
    import Prelude;

    newtype JustEdit f edit = MkJustEdit edit;

    instance (HasNewValue (Subject edit),FullEdit edit,FunctorOne f) => Edit (JustEdit f edit) where
    {
        type Subject (JustEdit f edit) = f (Subject edit);

        applyEdit (MkJustEdit edita) = cfmap (applyEdit edita);

        invertEdit (MkJustEdit edita) molda = case retrieveOne molda of
        {
            SuccessResult olda -> fmap MkJustEdit (invertEdit edita olda);
            _ -> Nothing;
        };
    };

    instance HasTypeKKTTKTT JustEdit where
    {
        typeKKTTKTT = MkTypeKKTTKTT
            (WitKKTTKTT (unsafeIOWitnessFromString "Data.Changes.JustEdit.JustEdit"))
            (mkTInfoKKTTKTT (\tf tedit -> do
                {
                    MkEditInst tsubj <- typeFactT tedit;
                    MkFullEditInst <- typeFactT tedit;
                    MkHasNewValueInst <- typeFactT tsubj;
                    MkFunctorOneInst <- typeFactKTT tf;
                    return (MkEditInst (applyTTypeT tf tsubj));
                })
            );
    };

    type JustRepEdit f edit = Either (WholeEdit (f (Subject edit))) (JustEdit f edit);
{-
    justWholeEditRep' :: forall f edit. (HasTypeRepT edit,HasTypeRepT (Subject edit),HasTypeRepKTT f) =>
     TypeRepT (JustRepEdit f edit);
    justWholeEditRep' = MkTypeRepT;

    justWholeEditRep :: forall f edit. (HasTypeRepT edit,HasTypeRepT (Subject edit)) =>
     EditRepKTT f -> EditRepT (JustRepEdit f edit);
    justWholeEditRep repF = applyEditRep
     (applyEditRep typeRepKTKTT (applyEditRep typeRepKTT (applyEditRep repF typeRepT)))
     (applyEditRep (applyEditRep typeRepKKTTKTT repF) typeRepT);
-}


{-
    f1 :: EditEvidence (Either x y) -> EitherMatch (Either x y);
    f1 (MkEditInst,MkEditInst) = MkEitherMatch;

    matchEitherEditRep :: forall t. Edit t => Maybe (EitherMatch t);
    matchEitherEditRep = case typeRepT :: EditRepT t of
    {
        (TEditRepT (TEditRepKTT repEither repX) repY) -> do
        {
            MkEqualType <- matchWitnessKTKTT repEither (typeRepKTKTT :: EditRepKTKTT Either);
            let
            {
                (MkEditInst,MkEditInst) = editEvidence (TEditRepT (TEditRepKTT repEither repX) repY);
            };
            return MkEitherMatch;
        };
        _ -> Nothing;
    };
-}
    --matchEditStructure :: (Edit a,Edit b) => EditRepT b -> Maybe (EditStructure a b);
{-
    matchJustWholeEditRep ::
     forall t. (Edit t) => EditRepT t -> Maybe (forall r.
     (forall f edit. EqualType t (JustRepEdit f edit) -> EditRepKTT f -> EditRepT edit -> EditRepT (Subject edit) -> r) -> r
     );
    matchJustWholeEditRep rep = do
    {
        (MkEitherStructure repWFA repJFE) <- matchEditStructure (Type :: Type (Either (NoEdit ()) (NoEdit ()))) rep;
        (MkWholeEditStructure repFA) <- matchEditStructure (Type :: Type (WholeEdit ())) repWFA;
        (MkJustEditStructure repF repEdit) <- matchEditStructure (Type :: Type (JustEdit Maybe (WholeEdit Char))) repJFE;
        case repFA of
        {
            TEditRepT repF' repSubj -> do
            {
                MkEqualType <- matchWitnessKTT repF repF';
                MkEqualType <- matchWitnessT (subjectRepT repEdit) repSubj;
                return (\fr -> fr MkEqualType repF' repEdit repSubj);
            };
            _ -> Nothing;
        };
    };
-}
{-
    matchJustWholeEditRep
     (TEditRepT (TEditRepKTT repEither (TEditRepT repWholeEdit (TEditRepT repF repSubj))) (TEditRepT (KTTEditRepKTT repJustEdit repF') repEdit)) = do
    {
        let
        {
            inst :: EditInst t;
            inst = MkEditInst;
        };
        MkEqualType <- matchWitnessKTKTT repEither (typeRepKTKTT :: EditRepKTKTT Either);
        MkEqualType <- matchWitnessKTT repWholeEdit (typeRepKTT :: EditRepKTT WholeEdit);
        MkEqualType <- matchWitnessKKTTKTT repJustEdit (typeRepKKTTKTT :: EditRepKKTTKTT JustEdit);
        MkEqualType <- matchWitnessKTT repF repF';
        let
        {
            repSubj' = (foo1 . (\(_,_,e) -> e) . editInstEvidence . snd . editInstEvidence) inst;
        };
        et <- matchWitness repSubj repSubj';
        return (\fr -> fr (foo et) repF repEdit repSubj');
    } where
    {
        foo1 :: FullEditInst edit -> EditRepT (Subject edit);
        foo1 MkFullEditInst = typeRepT;

        foo :: EqualType subj (Subject edit) -> EqualType (Either (WholeEdit (f subj)) (JustEdit f edit)) (JustRepEdit f edit);
        foo MkEqualType = MkEqualType;
    };
    matchJustWholeEditRep _ = Nothing;
-}
    extractJustEdit :: forall f edit. (FunctorOne f,FullEdit edit) => JustRepEdit f edit -> Maybe edit;
    extractJustEdit (Right (MkJustEdit edit)) = Just edit;
    extractJustEdit (Left (MkWholeEdit fa)) = case retrieveOne fa of
    {
        SuccessResult a -> Just (replaceEdit a);
        _ -> Nothing;
    };
}
