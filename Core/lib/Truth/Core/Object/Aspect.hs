module Truth.Core.Object.Aspect where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types;


    data Aspect edit where
    {
        MkAspect ::
         forall edita editb. (Edit editb) =>
          TypeInfo editb -> TypeInfo (EditSubject editb) -> GeneralLens edita editb -> Aspect edita;
    };

    mapAspect :: (Edit edita,Edit editb) => GeneralLens edita editb -> Aspect editb -> Aspect edita;
    mapAspect lens (MkAspect ie is lens') = MkAspect ie is $ lens' `editCompose` lens;

    mapOneWholeEditAspect :: forall f edit. (MonadOne f, Edit edit,FullReader (EditReader edit)) =>
     TypeInfo f -> Aspect edit -> KnowM (Aspect (OneWholeEdit f edit));
    mapOneWholeEditAspect infoF (MkAspect infoEditB infoSubj lens) = do
    {
        let
        {
            knowledge = mconcat [typeInfoKnowledge infoF,typeInfoKnowledge infoEditB,typeInfoKnowledge infoSubj];
        };
        ValueFact (MkEditReaderTypeInfo infoReader) <- askTypeInfo knowledge $ applyTypeInfo (typeInfo @EditReaderTypeInfo) infoEditB;
        ConstraintFact <- askTypeInfo knowledge $ applyTypeInfo (typeInfo @FullEdit) infoEditB;
        let
        {
            infoOneEdit = applyTypeInfo (applyTypeInfo (typeInfo @OneEdit) infoF) infoEditB;
            infoJustReader = applyTypeInfo (applyTypeInfo (typeInfo @OneReader) infoF) infoReader;

            infoEditB' = applyTypeInfo (applyTypeInfo (typeInfo @SumEdit) $ applyTypeInfo (typeInfo @WholeReaderEdit) infoJustReader) infoOneEdit;
            infoSubj' = applyTypeInfo infoF infoSubj;
            lens' = oneWholeGeneralLens lens;
        };
        return $ MkAspect infoEditB' infoSubj' lens';
    };
}
