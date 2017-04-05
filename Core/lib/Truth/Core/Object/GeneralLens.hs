module Truth.Core.Object.GeneralLens where
{
{-
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types;
    import Truth.Core.Object.Object;


    type ArgSubjWit (k :: * -> *) = forall (a' :: *). a' :~: EditSubject (k a');

    data SubjectEditWitness (a :: *) (edit :: *) where
    {
        EditSubjectEditWitness :: forall (edit :: *). Info edit -> SubjectEditWitness (EditSubject edit) edit;
        SubjectSubjectEditWitness :: forall (k :: * -> *) (a :: *). ArgSubjWit k -> Info k -> SubjectEditWitness a (k a);
    };

--    wholeSubjectEditWitness :: forall (a :: *). SubjectEditWitness a (WholeEdit (WholeReader a));
--    wholeSubjectEditWitness = SubjectSubjectEditWitness Refl info;

    instance TestEquality (SubjectEditWitness a) where
    {
        testEquality (EditSubjectEditWitness iea) (EditSubjectEditWitness ieb) = do
        {
            Refl <- testHetEquality iea ieb;
            return Refl;
        };
        testEquality (SubjectSubjectEditWitness _ ika) (SubjectSubjectEditWitness _ ikb) = do
        {
            Refl <- testHetEquality ika ikb;
            return Refl;
        };
        testEquality (SubjectSubjectEditWitness eqSubj ika) (EditSubjectEditWitness ieb) = splitInfoMaybe ieb $ \(ikb,isb) -> do
        {
            -- eqSubj :: forall a'. a' :~: (EditSubject (k a'))
            -- ika :: Info k
            -- ieb :: Info edit
            -- EditSubject edit ~ a
            -- isb :: Info aa
            -- to show: aa ~ EditSubject edit


            Refl <- testHetEquality ika ikb;
            return Refl;
        };
        testEquality ia@(EditSubjectEditWitness _) ib@(SubjectSubjectEditWitness _ _) = do
        {
            Refl <- testEquality ib ia;
            return Refl;
        };
    };

    data GeneralLens a b where
    {
        IdGeneralLens :: forall t. GeneralLens t t;
        SimpleGeneralLens :: forall a b. Lens a b -> GeneralLens a b;
        EditGeneralLens ::
         forall edita editb. (FullEdit edita,FullEdit editb) =>
         SubjectEditWitness (EditSubject edita) edita ->
         SubjectEditWitness (EditSubject editb) editb ->
         EditLens edita editb ->
         GeneralLens (EditSubject edita) (EditSubject editb);
    };

    toEditGeneralLens :: Lens a b -> GeneralLens a b;
    toEditGeneralLens lens = EditGeneralLens wholeSubjectEditWitness wholeSubjectEditWitness (wholeEditLens lens);

    instance Category GeneralLens where
    {
        id = IdGeneralLens;

        IdGeneralLens . lens = lens;
        lens . IdGeneralLens = lens;
        (SimpleGeneralLens bc) . (SimpleGeneralLens ab) = SimpleGeneralLens (bc . ab);
        (SimpleGeneralLens bc) . fab@(EditGeneralLens _ _ _) = (toEditGeneralLens bc) . fab;
        fbc@(EditGeneralLens _ _ _) . (SimpleGeneralLens ab) = fbc . (toEditGeneralLens ab);
        (EditGeneralLens sewb1 sewc bc) . (EditGeneralLens sewa sewb2 ab) = case testEquality sewb1 sewb2 of
        {
            Just Refl -> EditGeneralLens sewa sewc (bc . ab);
            _ -> EditGeneralLens sewa sewc (bc . convertEditLens . ab);
        };
    };

    data GeneralLensSubscribe a where
    {
        MkGeneralLensSubscribe :: forall edit. (FullEdit edit) => Object edit -> GeneralLensSubscribe (EditSubject edit);
    };

    generalLensSubscribe :: GeneralLens a b -> GeneralLensSubscribe a -> GeneralLensSubscribe b;
    generalLensSubscribe IdGeneralLens sub = sub;
    generalLensSubscribe (SimpleGeneralLens lens) (MkGeneralLensSubscribe sub) =
     MkGeneralLensSubscribe (lensObject lens (lensObject convertEditLens sub));
    generalLensSubscribe (EditGeneralLens _ _ lens) (MkGeneralLensSubscribe sub) =
     MkGeneralLensSubscribe (lensObject lens (lensObject convertEditLens sub));
-}
}
