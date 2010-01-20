module Data.Changes.FixedEditLens where
{
    import Data.Changes.FloatingEditLens;
    import Data.Changes.JustEdit;
    import Data.Changes.WholeEdit;
    import Data.Changes.Edit;
    import Data.Lens;
    import Data.Result;
    import Data.ConstFunction;
    import Data.FunctorOne;
    import Data.Chain;
    import Control.Applicative;
    import Control.Category;
    import Prelude hiding (id,(.),sequence);

    -- | A FixedEditLens is a lens without state
    ;
    data FixedEditLens' m edita editb = MkFixedLens
    {
        fixedLensUpdate :: edita -> ConstFunction (Subject edita) (Maybe editb),
        fixedLensSimple :: Lens' m (Subject edita) (Subject editb),
        fixedLensPutEdit :: editb -> ConstFunction (Subject edita) (m edita)
    };

    type FixedEditLens = FixedEditLens' Maybe;

    toFixedLens :: (FunctorOne m) => FixedEditLens' m edita editb -> FixedEditLens edita editb;
    toFixedLens lens = MkFixedLens
    {
        fixedLensUpdate = fixedLensUpdate lens,
        fixedLensSimple = toLens (fixedLensSimple lens),
        fixedLensPutEdit = \editb -> fmap getMaybeOne (fixedLensPutEdit lens editb)
    };

    instance (Applicative m,FunctorOne m) => Category (FixedEditLens' m) where
    {
        id = MkFixedLens
        {
            fixedLensUpdate = \edit -> pure (Just edit),
            fixedLensSimple = id,
            fixedLensPutEdit = \editb -> pure (pure editb)
        };
        bc . ab = MkFixedLens
        {
            fixedLensUpdate = \edita -> do
            {
                meb <- fixedLensUpdate ab edita;
                case meb of
                {
                    Just editb -> cofmap1CF (lensGet (fixedLensSimple ab)) (fixedLensUpdate bc editb);
                    _ -> return Nothing;
                };
            },
            fixedLensSimple = (fixedLensSimple bc) . (fixedLensSimple ab),
            fixedLensPutEdit = \editc -> do
            {
                meditb <- cofmap1CF (lensGet (fixedLensSimple ab)) (fixedLensPutEdit bc editc);
                case retrieveOne meditb of
                {
                    SuccessResult editb -> fixedLensPutEdit ab editb;
                    FailureResult ff -> return ff;
                };
            }
        };
    };

    instance (FunctorOne f,Applicative m) => CatFunctor (FixedEditLens' m) (JustEdit f) where
    {
        cfmap lens = MkFixedLens
        {
            fixedLensUpdate = \(MkJustEdit edita) ->  do
            {
                fmeditb <- cfmap (fixedLensUpdate lens edita);
                return (case retrieveOne fmeditb of
                {
                    SuccessResult (Just editb) -> Just (MkJustEdit editb);
                    _ -> Nothing;
                });
            },
            fixedLensSimple = cfmap (fixedLensSimple lens),
            fixedLensPutEdit = \(MkJustEdit editb) -> do
            {
                fmedita <- cfmap (fixedLensPutEdit lens editb);
                return (case retrieveOne fmedita of
                {
                    SuccessResult medita -> fmap MkJustEdit medita;
                    FailureResult _fx -> pure (MkJustEdit undefined); -- any JustEdit edit will do
                });
            }
        };
    };

    fixedFloatingLens :: FixedEditLens' m edita editb -> FloatingEditLens' m () edita editb;
    fixedFloatingLens lens = MkFloatingLens
    {
        floatingLensInitial = (),
        floatingLensUpdate = \edit _ -> do
        {
            meb <- fixedLensUpdate lens edit;
            return ((),meb);
        },
        floatingLensSimple = \_ -> fixedLensSimple lens,
        floatingLensPutEdit = \_ -> fixedLensPutEdit lens
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

    cleanFixedLens :: CleanLens' m edita editb -> FixedEditLens' m edita editb;
    cleanFixedLens lens = MkFixedLens
    {
        fixedLensUpdate = \edit -> pure (cleanLensUpdate lens edit),
        fixedLensSimple = cleanLensSimple lens,
        fixedLensPutEdit = \edit -> pure (cleanLensPutEdit lens edit)
    };

    simpleFixedLens :: (Functor m) => Lens' m a b -> FixedEditLens' m (WholeEdit a) (WholeEdit b);
    simpleFixedLens lens = MkFixedLens
    {
        fixedLensUpdate = \(MkWholeEdit a) -> pure (Just (MkWholeEdit (lensGet lens a))),
        fixedLensSimple = lens,
        fixedLensPutEdit = \(MkWholeEdit newb) -> fmap (fmap MkWholeEdit) (lensPutback lens newb)
    };

    simpleConvertFixedLens :: (Functor m,FullEdit edita,FullEdit editb) => Lens' m (Subject edita) (Subject editb) -> FixedEditLens' m edita editb;
    simpleConvertFixedLens lens = MkFixedLens
    {
        fixedLensUpdate = \edit -> fmap (Just . replaceEdit . (lensGet lens)) (applyEdit edit),
        fixedLensSimple = lens,
        fixedLensPutEdit = \editb -> do
        {
            newb <- cofmap1CF (lensGet lens) (applyEdit editb);
            ma <- lensPutback lens newb;
            return (fmap replaceEdit ma);
        }
    };

    convertFixedEditLens' :: (Applicative m,FunctorOne m,FullEdit edita,FullEdit editb,Subject edita ~ Subject editb) => FixedEditLens' m edita editb;
    convertFixedEditLens' = MkFixedLens
    {
        fixedLensUpdate = \edit -> fmap (Just . replaceEdit) (applyEdit edit),
        fixedLensSimple = id,
        fixedLensPutEdit = \edit -> fmap (pure . replaceEdit) (applyEdit edit)
    };

    convertFixedEditLens :: (FullEdit edita,FullEdit editb,Subject edita ~ Subject editb) => FixedEditLens edita editb;
    convertFixedEditLens = convertFixedEditLens';

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
