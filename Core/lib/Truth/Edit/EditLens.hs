module Truth.Edit.EditLens where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;
    import Truth.Edit.Edit;
    import Truth.Edit.WholeEdit;
    import Truth.Edit.MaybeReader;
    import Truth.Edit.JustEdit;
    import Truth.Edit.EditFunction;


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

    editLensAllowed :: (FunctorOne m) =>
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

    instance (Applicative m,FunctorOne m) => Category (EditLens' m) where
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

    instance (FunctorOne f,Applicative m) => CatFunctor (EditLens' m) (JustEdit f) where
    {
        cfmap lens = MkEditLens
        {
            editLensFunction = cfmap (editLensFunction lens),
            editLensPutEdit = \(MkJustEdit editb) -> do
            {
                fmedita <- liftJustReadable (editLensPutEdit lens editb);
                return (case retrieveOne fmedita of
                {
                    SuccessResult medita -> fmap MkJustEdit medita;
                    FailureResult _fx -> pure (MkJustEdit undefined); -- any JustEdit edit will do
                });
            }
        };
    };

    simpleEditLens :: (Functor m) => Lens' m a b -> EditLens' m (WholeEdit (WholeReader a)) (WholeEdit (WholeReader b));
    simpleEditLens lens = MkEditLens
    {
        editLensFunction = editFunction (lensGet lens),
        editLensPutEdit = \(MkWholeEdit newb) -> do
        {
            olda <- fromReader;
            let
            {
                newma = lensPutback lens newb olda;
                medita = fmap MkWholeEdit newma;
            };
            return medita;
        }
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

    convertEditLens' :: (Applicative m,FunctorOne m,FullEdit edita,FullEdit editb,EditSubject edita ~ EditSubject editb) => EditLens' m edita editb;
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
