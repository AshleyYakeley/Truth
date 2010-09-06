module Truth.Object.GeneralLens where
{
    import Truth.Object.Object;
    import Truth.Edit;
    import Truth.Edit.Import;

    type ArgSubjWit k = forall a'. EqualType a' (Subject (k a'));

    data SubjectEditWitness a edit where
    {
        EditSubjectEditWitness :: forall edit. Info (Type_T edit) -> SubjectEditWitness (Subject edit) edit;
        SubjectSubjectEditWitness :: forall k a. ArgSubjWit k -> Info (Type_KTT k) -> SubjectEditWitness a (k a);
    };

    wholeSubjectEditWitness :: SubjectEditWitness a (WholeEdit a);
    wholeSubjectEditWitness = SubjectSubjectEditWitness MkEqualType info;

    instance SimpleWitness (SubjectEditWitness a) where
    {
        matchWitness (EditSubjectEditWitness iea) (EditSubjectEditWitness ieb) = do
        {
            MkEqualType <- matchWitness iea ieb;
            return MkEqualType;
        };
        matchWitness (SubjectSubjectEditWitness _ ika) (SubjectSubjectEditWitness _ ikb) = do
        {
            MkEqualType <- matchWitness ika ikb;
            return MkEqualType;
        };
        matchWitness (SubjectSubjectEditWitness eqSubj ika) (EditSubjectEditWitness ieb) = do
        {
            MkMatch ikb isb <- matchProp $(type1[t|Match|]) ieb;
            MkEqualType <- matchWitness ika ikb;
            Kind_T <- matchProp $(type1[t|Kind_T|]) isb;
            return (f2 isb eqSubj);
        } where
        {
            f1 :: forall k sb. Type (k ()) -> Info (Type_T sb) -> ArgSubjWit k -> EqualType sb (Subject (k sb));
            f1 _ _ e = e;

            kmap :: Type (k ()) -> EqualType a' b -> EqualType (k b) (k a');
            kmap _ MkEqualType = MkEqualType;

            f2 :: forall k sb. Info (Type_T sb) -> ArgSubjWit k -> EqualType (k (Subject (k sb))) (k sb);
            f2 i a = kmap (Type :: Type (k ()) ) (f1 (Type :: Type (k ()) ) i a);
        };
        matchWitness ia@(EditSubjectEditWitness _) ib@(SubjectSubjectEditWitness _ _) = do
        {
            MkEqualType <- matchWitness ib ia;
            return MkEqualType;
        };
    };

    data GeneralLens a b where
    {
        IdGeneralLens :: forall t. GeneralLens t t;
        SimpleGeneralLens :: forall a b. Lens a b -> GeneralLens a b;
        EditGeneralLens ::
         forall edita editb. (FullEdit edita,FullEdit editb) =>
         SubjectEditWitness (Subject edita) edita ->
         SubjectEditWitness (Subject editb) editb ->
         EditLens edita editb ->
         GeneralLens (Subject edita) (Subject editb);
    };

    toEditGeneralLens :: Lens a b -> GeneralLens a b;
    toEditGeneralLens lens = EditGeneralLens wholeSubjectEditWitness wholeSubjectEditWitness (simpleEditLens lens);

    instance Category GeneralLens where
    {
        id = IdGeneralLens;

        IdGeneralLens . lens = lens;
        lens . IdGeneralLens = lens;
        (SimpleGeneralLens bc) . (SimpleGeneralLens ab) = SimpleGeneralLens (bc . ab);
        (SimpleGeneralLens bc) . fab@(EditGeneralLens _ _ _) = (toEditGeneralLens bc) . fab;
        fbc@(EditGeneralLens _ _ _) . (SimpleGeneralLens ab) = fbc . (toEditGeneralLens ab);
        (EditGeneralLens sewb1 sewc bc) . (EditGeneralLens sewa sewb2 ab) = case matchWitness sewb1 sewb2 of
        {
            Just MkEqualType -> EditGeneralLens sewa sewc (bc . ab);
            _ -> EditGeneralLens sewa sewc (bc . convertEditLens . ab);
        };
    };

    data GeneralLensSubscribe a where
    {
        MkGeneralLensSubscribe :: forall edit. (FullEdit edit) => Subscribe edit -> GeneralLensSubscribe (Subject edit);
    };

    generalLensSubscribe :: GeneralLens a b -> GeneralLensSubscribe a -> GeneralLensSubscribe b;
    generalLensSubscribe IdGeneralLens sub = sub;
    generalLensSubscribe (SimpleGeneralLens lens) (MkGeneralLensSubscribe sub) =
     MkGeneralLensSubscribe (lensSubscribe lens (lensSubscribe convertEditLens sub));
    generalLensSubscribe (EditGeneralLens _ _ lens) (MkGeneralLensSubscribe sub) =
     MkGeneralLensSubscribe (lensSubscribe lens (lensSubscribe convertEditLens sub));
}
