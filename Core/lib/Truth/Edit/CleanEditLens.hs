module Truth.Edit.CleanEditLens where
{
    import Truth.Edit.Import;
    import Truth.Edit.Edit;
    import Truth.Edit.WholeEdit;
    import Truth.Edit.Either;
    import Truth.Edit.EditFunction;
    import Truth.Edit.EditLens;


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

    withWholeLens :: (Functor m,Edit edita,FullEdit editb) =>
     CleanEditLens' m edita editb ->
     CleanEditLens' m (EitherEdit (WholeEdit (EditReader edita)) edita) editb;
    withWholeLens lens = MkCleanEditLens
    {
        cleanEditLensFunction = withWholeEditFunction (cleanEditLensFunction lens),
        cleanEditLensPutEdit = \editb -> fmap RightEdit (cleanEditLensPutEdit lens editb)
    };
}
