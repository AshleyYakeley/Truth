module Data.Changes.GeneralLens where
{
    import Data.Changes.Object;
    import Data.Changes.WholeEdit;
    import Data.Changes.FixedEditLens;
    import Data.Changes.Edit;
    import Data.TypeKT;
    import Data.Lens;
    import Data.Witness;
    import Control.Category;
    import Prelude hiding (id,(.));

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
        FixedGeneralLens ::
         forall edita editb. (FullEdit edita,FullEdit editb) =>
         SubjectEditWitness (Subject edita) edita ->
         SubjectEditWitness (Subject editb) editb ->
         FixedEditLens edita editb ->
         GeneralLens (Subject edita) (Subject editb);
    };

    toFixedGeneralLens :: Lens a b -> GeneralLens a b;
    toFixedGeneralLens lens = FixedGeneralLens wholeSubjectEditWitness wholeSubjectEditWitness (simpleFixedLens lens);

    instance Category GeneralLens where
    {
        id = IdGeneralLens;

        IdGeneralLens . lens = lens;
        lens . IdGeneralLens = lens;
        (SimpleGeneralLens bc) . (SimpleGeneralLens ab) = SimpleGeneralLens (bc . ab);
        (SimpleGeneralLens bc) . fab@(FixedGeneralLens _ _ _) = (toFixedGeneralLens bc) . fab;
        fbc@(FixedGeneralLens _ _ _) . (SimpleGeneralLens ab) = fbc . (toFixedGeneralLens ab);
        (FixedGeneralLens sewb1 sewc bc) . (FixedGeneralLens sewa sewb2 ab) = case matchWitnessT sewb1 sewb2 of
        {
            Just MkEqualType -> FixedGeneralLens sewa sewc (bc . ab);
            _ -> FixedGeneralLens sewa sewc (bc . convertFixedEditLens . ab);
        };
    };

    data GeneralLensSubscribe a where
    {
        MkGeneralLensSubscribe :: forall edit. (FullEdit edit) => Subscribe edit -> GeneralLensSubscribe (Subject edit);
    };

    generalLensSubscribe :: GeneralLens a b -> GeneralLensSubscribe a -> GeneralLensSubscribe b;
    generalLensSubscribe IdGeneralLens sub = sub;
    generalLensSubscribe (SimpleGeneralLens lens) (MkGeneralLensSubscribe sub) =
     MkGeneralLensSubscribe (lensSubscribe lens (lensSubscribe convertFixedEditLens sub));
    generalLensSubscribe (FixedGeneralLens _ _ lens) (MkGeneralLensSubscribe sub) =
     MkGeneralLensSubscribe (lensSubscribe lens (lensSubscribe convertFixedEditLens sub));
}
