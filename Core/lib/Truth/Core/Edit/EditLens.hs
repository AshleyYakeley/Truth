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
        editLensPutEdit :: editb -> Readable (EditReader edita) (m [edita])
    };

    editLensPutEdits :: Applicative m => EditLens' m edita editb -> [editb] -> Readable (EditReader edita) (m [edita]);
    editLensPutEdits _lens [] = return $ pure [];
    editLensPutEdits lens (e:ee) = getCompose $ (++) <$> (MkCompose $ editLensPutEdit lens e) <*> (MkCompose $ editLensPutEdits lens ee);

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
            editLensPutEdit = \editb -> pure $ pure $ pure editb
        };
        bc . ab = MkEditLens
        {
            editLensFunction = (editLensFunction bc) . (editLensFunction ab),
            editLensPutEdit = \editc -> do
            {
                meditbs <- mapReadable (editGet (editLensFunction ab)) (editLensPutEdit bc editc);
                case retrieveOne meditbs of
                {
                    SuccessResult editbs -> editLensPutEdits ab editbs;
                    FailureResult (MkLimit ff) -> return ff;
                };
            }
        };
    };

    convertEditLens :: (Applicative m,EditSubject edita ~ EditSubject editb,Edit edita,FullEdit edita,FullEdit editb) => EditLens' m edita editb;
    convertEditLens = let
    {
        editLensFunction = convertEditFunction;
        editLensPutEdit editb = do
        {
            oldsubject <- fromReader;
            let
            {
                newsubject = fromReadFunction (applyEdit editb) oldsubject;
            };
            return $ pure $ getReplaceEdits newsubject;
        };
    } in MkEditLens{..};
}
