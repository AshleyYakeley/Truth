module Truth.Object.GeneralLens where
{
    import Truth.Object.Object;
    import Truth.Edit;
    import Truth.Edit.Import;

    type ArgSubjWit k = forall a'. EqualTypeT a' (Subject (k a'));

    data SubjectEditWitness a edit where
    {
        EditSubjectEditWitness :: forall edit. InfoT edit -> SubjectEditWitness (Subject edit) edit;
        SubjectSubjectEditWitness :: forall k a. ArgSubjWit k -> InfoKTT k -> SubjectEditWitness a (k a);
    };

    wholeSubjectEditWitness :: SubjectEditWitness a (WholeEdit a);
    wholeSubjectEditWitness = SubjectSubjectEditWitness MkEqualType infoKTT;

    instance SimpleWitness1 SubjectEditWitness where
    {
        matchWitness1 (EditSubjectEditWitness iea) (EditSubjectEditWitness ieb) = matchWitnessT iea ieb;
        matchWitness1 (SubjectSubjectEditWitness _ ika) (SubjectSubjectEditWitness _ ikb) = do
        {
            MkEqualType <- matchWitnessKTT ika ikb;
            return MkEqualType;
        };
        matchWitness1 (SubjectSubjectEditWitness eqSubj (ika :: InfoKTT k)) (EditSubjectEditWitness ieb) = do
        {
            MkTMatchT (ika' :: InfoKTT k') (_ibs :: InfoT a') <- matchPropertyT_ (Type :: Type (SatKTT TMatchT)) ieb;
            MkEqualType <- matchWitnessKTT ika ika';
            MkEqualType <- return (eqSubj :: (EqualTypeT a' (Subject (k a'))));
            return MkEqualType;
        };
        matchWitness1 ia@(EditSubjectEditWitness _) ib@(SubjectSubjectEditWitness _ _) = do
        {
            MkEqualType <- matchWitnessT ib ia;
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
        (EditGeneralLens sewb1 sewc bc) . (EditGeneralLens sewa sewb2 ab) = case matchWitnessT sewb1 sewb2 of
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
