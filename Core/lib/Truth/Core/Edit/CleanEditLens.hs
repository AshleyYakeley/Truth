module Truth.Core.Edit.CleanEditLens where
{
    import Truth.Core.Import;
    import Truth.Core.Edit.EditFunction;
    import Truth.Core.Edit.EditLens;


    data CleanEditLens' m edita editb = MkCleanEditLens
    {
        cleanEditLensFunction :: CleanEditFunction edita editb,
        cleanEditLensPutEdit :: editb -> m [edita]
    };

    cleanEditLensPutEdits :: Applicative m => CleanEditLens' m edita editb -> [editb] -> m [edita];
    cleanEditLensPutEdits _lens [] = pure [];
    cleanEditLensPutEdits lens (e:ee) = (++) <$> (cleanEditLensPutEdit lens e) <*> (cleanEditLensPutEdits lens ee);

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
        id = MkCleanEditLens id (return . pure);
        bc . ab = MkCleanEditLens
         ((cleanEditLensFunction bc) . (cleanEditLensFunction ab))
         (\c -> (cleanEditLensPutEdit bc c) >>= (cleanEditLensPutEdits ab));
    };

    cleanEditLens :: CleanEditLens' m edita editb -> EditLens' m edita editb;
    cleanEditLens lens = MkEditLens
    {
        editLensFunction = cleanEditFunction (cleanEditLensFunction lens),
        editLensPutEdit = \edit -> pure (cleanEditLensPutEdit lens edit)
    };
}
