module Truth.Core.Edit.EditLens where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit.Edit;
    import Truth.Core.Edit.EditFunction;


    -- | A EditLens is a lens that converts edits
    ;
    data GenEditLens' c m edita editb = MkEditLens
    {
        editLensFunction :: GenEditFunction c edita editb,
        editLensPutEdit :: editb -> GenReadable c (EditReader edita) (m [edita])
    };

    type EditLens' = GenEditLens' Monad;
    type IOEditLens' = GenEditLens' MonadIO;

    editLensToGen :: EditLens' m edita editb -> GenEditLens' c m edita editb;
    editLensToGen (MkEditLens f pe) = MkEditLens (editFunctionToGen f) (\eb -> readableToGen $ pe eb);

    editLensPutEdits :: Applicative m => GenEditLens' c m edita editb -> [editb] -> GenReadable c (EditReader edita) (m [edita]);
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

    instance (ReadableConstraint c,Applicative m,MonadOne m) => Category (GenEditLens' c m) where
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
                meditbs <- mapGenReadable (editGet (editLensFunction ab)) (editLensPutEdit bc editc);
                case retrieveOne meditbs of
                {
                    SuccessResult editbs -> editLensPutEdits ab editbs;
                    FailureResult (MkLimit ff) -> return ff;
                };
            }
        };
    };

    convertEditLens :: (Applicative m,EditSubject edita ~ EditSubject editb,FullEdit edita,FullEdit editb) => EditLens' m edita editb;
    convertEditLens = let
    {
        editLensFunction = convertEditFunction;
        editLensPutEdit editb = do
        {
            newsubject <- fromReadFunctionM (applyEdit editb) fromReader;
            return $ pure $ getReplaceEdits newsubject;
        };
    } in MkEditLens{..};

    invertEditLens :: ReadFunction (EditReader editb) (EditReader edita) -> EditLens' Identity edita editb -> EditLens' Identity editb edita;
    invertEditLens rfba lensab = MkEditLens
    {
        editLensFunction = MkEditFunction
        {
            editGet = rfba,
            editUpdate = \eb -> fmap runIdentity $ mapReadable rfba $ editLensPutEdit lensab eb
        },
        editLensPutEdit = \ea -> fmap pure $ mapReadable rfba $ editUpdate (editLensFunction lensab) ea
    };
}
