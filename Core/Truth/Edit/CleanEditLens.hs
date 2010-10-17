module Truth.Edit.CleanEditLens where
{
    import Truth.Edit.EditLens;
    import Truth.Edit.EditFunction;
    import Truth.Edit.WholeEdit;
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    data CleanEditLens' m edita editb = MkCleanEditLens
    {
        cleanEditLensFunction :: CleanEditFunction edita editb,
        cleanEditLensPutEdit :: editb -> (m edita)
    };

    instance IsBiMap CleanEditLens' where
    {
        mapBiMapM ff clens = MkCleanEditLens
        {
            cleanEditLensFunction = cleanEditLensFunction clens,
            cleanEditLensPutEdit = ff . (cleanEditLensPutEdit clens)
        };
    };

    instance (Monad m) => Category (CleanEditLens' m) where
    {
        id = MkCleanEditLens id return;
        bc . ab = MkCleanEditLens
         ((cleanEditLensFunction bc) . (cleanEditLensFunction ab))
         (\c -> (cleanEditLensPutEdit bc c) >>= (cleanEditLensPutEdit ab));
    };

    cleanEditLens :: CleanEditLens' m edita editb -> EditLens' m edita editb;
    cleanEditLens lens = MkEditLens
    {
        editLensFunction = cleanEditFunction (cleanEditLensFunction lens),
        editLensPutEdit = \edit -> pure (cleanEditLensPutEdit lens edit)
    };

    withWholeLens :: (Functor m,FullEdit editb) =>
     CleanEditLens' m edita editb ->
     CleanEditLens' m (Either (WholeEdit (Subject edita)) edita) editb;
    withWholeLens lens = MkCleanEditLens
    {
        cleanEditLensFunction = withWholeEditFunction (cleanEditLensFunction lens),
        cleanEditLensPutEdit = \editb -> fmap Right (cleanEditLensPutEdit lens editb)
    };
}
