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
        editLensUpdate :: edita -> ConstFunction (Subject edita) (Maybe editb),
        editLensSimple :: Lens' m (Subject edita) (Subject editb),
        editLensPutEdit :: editb -> ConstFunction (Subject edita) (m edita)
    };

    type EditLens = EditLens' Maybe;

    toEditLens :: (FunctorOne m) => EditLens' m edita editb -> EditLens edita editb;
    toEditLens lens = MkEditLens
    {
        editLensUpdate = editLensUpdate lens,
        editLensSimple = toLens (editLensSimple lens),
        editLensPutEdit = \editb -> fmap getMaybeOne (editLensPutEdit lens editb)
    };

    instance (Applicative m,FunctorOne m) => Category (EditLens' m) where
    {
        id = MkEditLens
        {
            editLensUpdate = \edit -> pure (Just edit),
            editLensSimple = id,
            editLensPutEdit = \editb -> pure (pure editb)
        };
        bc . ab = MkEditLens
        {
            editLensUpdate = \edita -> do
            {
                meb <- editLensUpdate ab edita;
                case meb of
                {
                    Just editb -> cofmap1CF (lensGet (editLensSimple ab)) (editLensUpdate bc editb);
                    _ -> return Nothing;
                };
            },
            editLensSimple = (editLensSimple bc) . (editLensSimple ab),
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

    fixedFloatingEditLens :: (Functor m) => EditLens' m edita editb -> FloatingEditLens' m () edita editb;
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

    data CleanLens' m edita editb = MkCleanLens
    {
        cleanLensUpdate :: edita -> Maybe editb,
        cleanLensSimple :: Lens' m (Subject edita) (Subject editb),
        cleanLensPutEdit :: editb -> m edita
    };

    --type CleanLens = CleanLens' Maybe;

    instance (Applicative m,Monad m,FunctorOne m) => Category (CleanLens' m) where
    {
        id = MkCleanLens
        {
            cleanLensUpdate = Just,
            cleanLensSimple = id,
            cleanLensPutEdit = pure
        };
        bc . ab = MkCleanLens
        {
            cleanLensUpdate = \edita -> do
            {
                editb <- cleanLensUpdate ab edita;
                cleanLensUpdate bc editb;
            },
            cleanLensSimple = (cleanLensSimple bc) . (cleanLensSimple ab),

            cleanLensPutEdit = \editc -> do
            {
                editb <- cleanLensPutEdit bc editc;
                cleanLensPutEdit ab editb;
            }
        };
    };

    cleanEditLens :: CleanLens' m edita editb -> EditLens' m edita editb;
    cleanEditLens lens = MkEditLens
    {
        editLensUpdate = \edit -> pure (cleanLensUpdate lens edit),
        editLensSimple = cleanLensSimple lens,
        editLensPutEdit = \edit -> pure (cleanLensPutEdit lens edit)
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

    withWholeLens :: (Functor m,FullEdit editb) => CleanLens' m edita editb -> CleanLens' m (Either (WholeEdit (Subject edita)) edita) editb;
    withWholeLens lens = MkCleanLens
    {
        cleanLensUpdate = \editewa -> case editewa of
        {
            Left (MkWholeEdit a) -> Just (replaceEdit (lensGet (cleanLensSimple lens) a));
            Right edita -> cleanLensUpdate lens edita;
        },
        cleanLensSimple = cleanLensSimple lens,
        cleanLensPutEdit = \editb -> fmap Right (cleanLensPutEdit lens editb)
    };
}
