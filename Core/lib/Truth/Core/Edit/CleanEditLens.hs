module Truth.Core.Edit.CleanEditLens where
{
    import Truth.Core.Import;
    import Truth.Core.Edit.EditFunction;
    import Truth.Core.Edit.EditLens;


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
}
