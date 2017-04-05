module Truth.Core.Edit.EditLens where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit.Edit;
    import Truth.Core.Edit.EditFunction;


    -- | A EditLens is a lens that converts edits
    ;
    data EditLens' m edita editb = MkEditLens
    {
        editLensFunction :: EditFunction edita editb,
        editLensPutEdit :: editb -> Readable (EditReader edita) (m edita)
    };

    editLensPutEdits :: Applicative m => EditLens' m edita editb -> [editb] -> Readable (EditReader edita) (m [edita]);
    editLensPutEdits _lens [] = return $ pure [];
    editLensPutEdits lens (e:ee) = getCompose $ (:) <$> (MkCompose $ editLensPutEdit lens e) <*> (MkCompose $ editLensPutEdits lens ee);

    editLensAllowed :: (MonadOne m) =>
     EditLens' m edita editb -> editb -> Readable (EditReader edita) Bool;
    editLensAllowed lens editb = do
    {
        medita <- editLensPutEdit lens editb;
        return (isJust (getMaybeOne medita));
    };

    type EditLens = EditLens' Maybe;

    instance IsBiMap EditLens' where
    {
        mapBiMapM ff elens = MkEditLens
        {
            editLensFunction = editLensFunction elens,
            editLensPutEdit = \editb -> fmap ff (editLensPutEdit elens editb)
        };
    };

    instance (Applicative m,MonadOne m) => Category (EditLens' m) where
    {
        id = MkEditLens
        {
            editLensFunction = id,
            editLensPutEdit = \editb -> pure (pure editb)
        };
        bc . ab = MkEditLens
        {
            editLensFunction = (editLensFunction bc) . (editLensFunction ab),
            editLensPutEdit = \editc -> do
            {
                meditb <- mapReadable (editGet (editLensFunction ab)) (editLensPutEdit bc editc);
                case retrieveOne meditb of
                {
                    SuccessResult editb -> editLensPutEdit ab editb;
                    FailureResult (MkLimit ff) -> return ff;
                };
            }
        };
    };
{-
    simpleConvertEditLens :: (Functor m,FullEdit edita,FullEdit editb) =>
     Lens' m (EditSubject edita) (EditSubject editb) -> EditLens' m edita editb;
    simpleConvertEditLens lens = MkEditLens
    {
        editLensFunction = simpleConvertEditFunction (simpleReadFunction (lensGet lens)),
        editLensPutEdit = \editb -> do
        {
            olda <- fromReader;
            let
            {
                oldb = lensGet lens olda;
                newb = fromReadFunction (applyEdit editb) oldb;
                newma = lensPutback lens newb olda;
                medita = fmap replaceEdit newma;
            };
            return medita;
        }
    };

    convertEditLens' :: (Applicative m,MonadOne m,FullEdit edita,FullEdit editb,EditSubject edita ~ EditSubject editb) => EditLens' m edita editb;
    convertEditLens' = MkEditLens
    {
        editLensFunction = convertEditFunction,

        editLensPutEdit = \editb -> do
        {
            newa <- mapReadable convertReadFunction (mapReadable (applyEdit editb) fromReader);
            return (pure (replaceEdit newa));
        }
    };

    convertEditLens :: (FullEdit edita,FullEdit editb,EditSubject edita ~ EditSubject editb) => EditLens edita editb;
    convertEditLens = convertEditLens';
-}
}
