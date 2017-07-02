module Truth.Core.Object.Aspect where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types;


    data Aspect edit where
    {
        MkAspect ::
         forall edita editb. (FullEdit editb) =>
          Info editb -> Info (EditSubject editb) -> GeneralLens edita editb -> Aspect edita;
    };

    mapAspect :: (Edit edita,Edit editb) => GeneralLens edita editb -> Aspect editb -> Aspect edita;
    mapAspect lens (MkAspect ie is lens') = MkAspect ie is $ lens' `editCompose` lens;

    mapOneWholeEditAspect :: forall f edit. (MonadOne f, Edit edit,FullReader (EditReader edit)) =>
     Info f -> Aspect edit -> Maybe (Aspect (OneWholeEdit f edit));
    mapOneWholeEditAspect infoF (MkAspect infoEditB infoSubj lens) = do
    {
        let
        {
            knowledge = mconcat [infoKnowledge infoF,infoKnowledge infoEditB,infoKnowledge infoSubj];
        };
        ValueFact (MkEditReaderInfo infoReader) <- ask knowledge $ applyInfo (info @EditReaderInfo) infoEditB;
        let
        {
            infoOneEdit = applyInfo (applyInfo (info @OneEdit) infoF) infoEditB;
            infoJustReader = applyInfo (applyInfo (info @OneReader) infoF) infoReader;

            infoEditB' = applyInfo (applyInfo (info @SumEdit) $ applyInfo (info @WholeReaderEdit) infoJustReader) infoOneEdit;
            infoSubj' = applyInfo infoF infoSubj;
            lens' = oneWholeGeneralLens lens;
        };
        return $ MkAspect infoEditB' infoSubj' lens';
    };
}
