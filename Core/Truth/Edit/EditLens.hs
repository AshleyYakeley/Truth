module Truth.Edit.EditLens where
{
    import Truth.Edit.EditFunction;
    import Truth.Edit.JustEdit;
    import Truth.Edit.WholeEdit;
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    -- | A EditLens is a lens without state
    ;
    data EditLens' m edita editb = MkEditLens
    {
        editLensFunction :: EditFunction edita editb,
        editLensPutEdit :: editb -> ConstFunction (Subject edita) (m edita)
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
                meditb <- cofmap1CF (editGet (editLensFunction ab)) (editLensPutEdit bc editc);
                case retrieveOne meditb of
                {
                    SuccessResult editb -> editLensPutEdit ab editb;
                    FailureResult ff -> return ff;
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
                fmedita <- cfmap (editLensPutEdit lens editb);
                return (case retrieveOne fmedita of
                {
                    SuccessResult medita -> fmap MkJustEdit medita;
                    FailureResult _fx -> pure (MkJustEdit undefined); -- any JustEdit edit will do
                });
            }
        };
    };

    simpleEditLens :: (Functor m) => Lens' m a b -> EditLens' m (WholeEdit a) (WholeEdit b);
    simpleEditLens lens = MkEditLens
    {
        editLensFunction = editFunction (lensGet lens),
        editLensPutEdit = \(MkWholeEdit newb) -> fmap (fmap MkWholeEdit) (lensPutback lens newb)
    };

    simpleConvertEditLens :: (Functor m,FullEdit edita,FullEdit editb) => Lens' m (Subject edita) (Subject editb) -> EditLens' m edita editb;
    simpleConvertEditLens lens = MkEditLens
    {
        editLensFunction = simpleConvertEditFunction (lensGet lens),
        editLensPutEdit = \editb -> do
        {
            newb <- cofmap1CF (lensGet lens) (applyEdit editb);
            ma <- lensPutback lens newb;
            return (fmap replaceEdit ma);
        }
    };

    convertEditLens' :: (Applicative m,FunctorOne m,FullEdit edita,FullEdit editb,Subject edita ~ Subject editb) => EditLens' m edita editb;
    convertEditLens' = MkEditLens
    {
        editLensFunction = convertEditFunction,
        editLensPutEdit = \edit -> fmap (pure . replaceEdit) (applyEdit edit)
    };

    convertEditLens :: (FullEdit edita,FullEdit editb,Subject edita ~ Subject editb) => EditLens edita editb;
    convertEditLens = convertEditLens';
}
