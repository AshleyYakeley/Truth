module Truth.Edit.EditLens where
{
    import Truth.Edit.FloatingEditLens;
    import Truth.Edit.JustEdit;
    import Truth.Edit.WholeEdit;
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    -- | A EditLens is a lens without state
    ;
    data EditLens' m edita editb = MkEditLens
    {
        editLensSimple :: Lens' m (Subject edita) (Subject editb),
        editLensUpdate :: edita -> ConstFunction (Subject edita) (Maybe editb),
        editLensPutEdit :: editb -> ConstFunction (Subject edita) (m edita)
    };

    type EditLens = EditLens' Maybe;

    instance IsBiMap EditLens' where
    {
        mapBiMapM ff elens = MkEditLens
        {
            editLensSimple = mapBiMapM ff (editLensSimple elens),
            editLensUpdate = editLensUpdate elens,
            editLensPutEdit = \editb -> fmap ff (editLensPutEdit elens editb)
        };
    };

    instance (Applicative m,FunctorOne m) => Category (EditLens' m) where
    {
        id = MkEditLens
        {
            editLensSimple = id,
            editLensUpdate = \edit -> pure (Just edit),
            editLensPutEdit = \editb -> pure (pure editb)
        };
        bc . ab = MkEditLens
        {
            editLensSimple = (editLensSimple bc) . (editLensSimple ab),
            editLensUpdate = \edita -> do
            {
                meb <- editLensUpdate ab edita;
                case meb of
                {
                    Just editb -> cofmap1CF (lensGet (editLensSimple ab)) (editLensUpdate bc editb);
                    _ -> return Nothing;
                };
            },
            editLensPutEdit = \editc -> do
            {
                meditb <- cofmap1CF (lensGet (editLensSimple ab)) (editLensPutEdit bc editc);
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
            editLensUpdate = \(MkJustEdit edita) ->  do
            {
                fmeditb <- cfmap (editLensUpdate lens edita);
                return (case retrieveOne fmeditb of
                {
                    SuccessResult (Just editb) -> Just (MkJustEdit editb);
                    _ -> Nothing;
                });
            },
            editLensSimple = cfmap (editLensSimple lens),
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

    fixedFloatingEditLens :: (Functor m) => EditLens' m edita editb -> FloatingEditLens' () m edita editb;
    fixedFloatingEditLens lens = MkFloatingEditLens
    {
        floatingEditLensSimple = fixedFloatingLens (editLensSimple lens),
        floatingEditLensUpdate = \edit _ -> do
        {
            meb <- editLensUpdate lens edit;
            return ((),meb);
        },
        floatingEditLensPutEdit = \_ editb -> do
        {
            mea <- editLensPutEdit lens editb;
            return (fmap (\ea -> ((),ea)) mea);
        }
    };

    data CleanEditLens' m edita editb = MkCleanEditLens
    {
        cleanEditLensSimple :: Lens' m (Subject edita) (Subject editb),
        cleanEditLensUpdate :: edita -> Maybe editb,
        cleanEditLensPutEdit :: editb -> m edita
    };

    --type CleanEditLens = CleanEditLens' Maybe;

    instance (Applicative m,Monad m,FunctorOne m) => Category (CleanEditLens' m) where
    {
        id = MkCleanEditLens
        {
            cleanEditLensUpdate = Just,
            cleanEditLensSimple = id,
            cleanEditLensPutEdit = pure
        };
        bc . ab = MkCleanEditLens
        {
            cleanEditLensUpdate = \edita -> do
            {
                editb <- cleanEditLensUpdate ab edita;
                cleanEditLensUpdate bc editb;
            },
            cleanEditLensSimple = (cleanEditLensSimple bc) . (cleanEditLensSimple ab),

            cleanEditLensPutEdit = \editc -> do
            {
                editb <- cleanEditLensPutEdit bc editc;
                cleanEditLensPutEdit ab editb;
            }
        };
    };

    instance IsBiMap CleanEditLens' where
    {
        mapBiMapM ff clens = MkCleanEditLens
        {
            cleanEditLensSimple = mapBiMapM ff (cleanEditLensSimple clens),
            cleanEditLensUpdate = cleanEditLensUpdate clens,
            cleanEditLensPutEdit = ff . (cleanEditLensPutEdit clens)
        };
    };

    cleanEditLens :: CleanEditLens' m edita editb -> EditLens' m edita editb;
    cleanEditLens lens = MkEditLens
    {
        editLensUpdate = \edit -> pure (cleanEditLensUpdate lens edit),
        editLensSimple = cleanEditLensSimple lens,
        editLensPutEdit = \edit -> pure (cleanEditLensPutEdit lens edit)
    };

    simpleEditLens :: (Functor m) => Lens' m a b -> EditLens' m (WholeEdit a) (WholeEdit b);
    simpleEditLens lens = MkEditLens
    {
        editLensUpdate = \(MkWholeEdit a) -> pure (Just (MkWholeEdit (lensGet lens a))),
        editLensSimple = lens,
        editLensPutEdit = \(MkWholeEdit newb) -> fmap (fmap MkWholeEdit) (lensPutback lens newb)
    };

    simpleConvertEditLens :: (Functor m,FullEdit edita,FullEdit editb) => Lens' m (Subject edita) (Subject editb) -> EditLens' m edita editb;
    simpleConvertEditLens lens = MkEditLens
    {
        editLensUpdate = \edit -> fmap (Just . replaceEdit . (lensGet lens)) (applyEdit edit),
        editLensSimple = lens,
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
        editLensUpdate = \edit -> fmap (Just . replaceEdit) (applyEdit edit),
        editLensSimple = id,
        editLensPutEdit = \edit -> fmap (pure . replaceEdit) (applyEdit edit)
    };

    convertEditLens :: (FullEdit edita,FullEdit editb,Subject edita ~ Subject editb) => EditLens edita editb;
    convertEditLens = convertEditLens';

    withWholeLens :: (Functor m,FullEdit editb) => CleanEditLens' m edita editb -> CleanEditLens' m (Either (WholeEdit (Subject edita)) edita) editb;
    withWholeLens lens = MkCleanEditLens
    {
        cleanEditLensUpdate = \editewa -> case editewa of
        {
            Left (MkWholeEdit a) -> Just (replaceEdit (lensGet (cleanEditLensSimple lens) a));
            Right edita -> cleanEditLensUpdate lens edita;
        },
        cleanEditLensSimple = cleanEditLensSimple lens,
        cleanEditLensPutEdit = \editb -> fmap Right (cleanEditLensPutEdit lens editb)
    };
}
