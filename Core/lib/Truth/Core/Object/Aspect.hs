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
          TypeInfo editb -> GeneralLens edita editb -> Aspect edita;
    };

    mapAspect :: (Edit edita,Edit editb) => GeneralLens edita editb -> Aspect editb -> Aspect edita;
    mapAspect lens (MkAspect ti lens') = MkAspect ti $ lens' <.> lens;

    mapOneWholeEditAspect :: forall f edit. (MonadOne f, Edit edit,IOFullReader (EditReader edit)) =>
     TypeInfo f -> Aspect edit -> KnowM (Aspect (OneWholeEdit f edit));
    mapOneWholeEditAspect infoF (MkAspect infoEditB lens) = do
    {
        ConstraintFact <- askTypeInfo $ applyTypeInfo (typeInfo @IOFullEdit) infoEditB;
        infoEditB' <- $(generateTypeInfoExpr [t|forall f' editb. OneWholeEdit f' editb|]) infoF infoEditB;
        return $ MkAspect infoEditB' $ toGeneralLens $ liftOneWholeGeneralLens getMaybeOne lens;
    };
}
