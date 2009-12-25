module Data.Changes.FixedLens where
{
    import Data.Changes.FloatingLens;
    import Data.Changes.JustEdit;
    import Data.Changes.SimpleLens;
    import Data.Changes.WholeEdit;
    import Data.Changes.Edit;
    import Data.Result;
    import Data.ConstFunction;
    import Data.FunctorOne;
    import Data.Chain;
    import Control.Applicative;
    import Control.Category;
    import Prelude hiding (id,(.),sequence);

    -- | A FixedLens is a lens without state
    ;
    data FixedLens' m edita editb = MkFixedLens
    {
        fixedLensUpdate :: edita -> ConstFunction (Subject edita) (Maybe editb),
        fixedLensSimple :: SimpleLens' m (Subject edita) (Subject editb),
        fixedLensPutEdit :: editb -> ConstFunction (Subject edita) (m edita)
    };

    type FixedLens = FixedLens' Maybe;

    toFixedLens :: (FunctorOne m) => FixedLens' m edita editb -> FixedLens edita editb;
    toFixedLens lens = MkFixedLens
    {
        fixedLensUpdate = fixedLensUpdate lens,
        fixedLensSimple = toSimpleLens (fixedLensSimple lens),
        fixedLensPutEdit = \editb -> fmap getMaybeOne (fixedLensPutEdit lens editb)
    };

    makeFixedLensUpdate :: (Edit edita,FullEdit editb) =>
     (Subject edita -> Subject editb) -> (edita -> Maybe (ConstFunction (Subject edita) (Maybe editb))) -> (edita -> ConstFunction (Subject edita) (Maybe editb));
    makeFixedLensUpdate getter ff edit = case ff edit of
    {
        Just ameb -> ameb;
        _ -> fmap (Just . replaceEdit . getter) (applyEdit edit);
    };

    instance (Applicative m,FunctorOne m) => Category (FixedLens' m) where
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
                    Just editb -> cofmap1CF (simpleLensGet (fixedLensSimple ab)) (fixedLensUpdate bc editb);
                    _ -> return Nothing;
                };
            },
            fixedLensSimple = (fixedLensSimple bc) . (fixedLensSimple ab),
            fixedLensPutEdit = \editc -> do
            {
                meditb <- cofmap1CF (simpleLensGet (fixedLensSimple ab)) (fixedLensPutEdit bc editc);
                case retrieveOne meditb of
                {
                    SuccessResult editb -> fixedLensPutEdit ab editb;
                    FailureResult ff -> return ff;
                };
            }
        };
    };

    instance (FunctorOne f,Applicative m) => CatFunctor (FixedLens' m) (JustEdit f) where
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

    fixedFloatingLens :: FixedLens' m edita editb -> FloatingLens' m () edita editb;
    fixedFloatingLens lens = MkFloatingLens
    {
        lensInitial = (),
        lensUpdate = \edit _ -> do
        {
            meb <- fixedLensUpdate lens edit;
            return ((),meb);
        },
        lensSimple = \_ -> fixedLensSimple lens,
        lensPutEdit = \_ -> fixedLensPutEdit lens
    };

    data CleanLens' m edita editb = MkCleanLens
    {
        cleanLensUpdate :: edita -> Maybe editb,
        cleanLensSimple :: SimpleLens' m (Subject edita) (Subject editb),
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

    cleanFixedLens :: CleanLens' m edita editb -> FixedLens' m edita editb;
    cleanFixedLens lens = MkFixedLens
    {
        fixedLensUpdate = \edit -> pure (cleanLensUpdate lens edit),
        fixedLensSimple = cleanLensSimple lens,
        fixedLensPutEdit = \edit -> pure (cleanLensPutEdit lens edit)
    };

    simpleWholeFixedLens :: (Functor m) => SimpleLens' m a b -> FixedLens' m (WholeEdit a) (WholeEdit b);
    simpleWholeFixedLens lens = MkFixedLens
    {
        fixedLensUpdate = \(MkWholeEdit a) -> pure (Just (MkWholeEdit (simpleLensGet lens a))),
        fixedLensSimple = lens,
        fixedLensPutEdit = \(MkWholeEdit newb) -> fmap (fmap MkWholeEdit) (simpleLensPutback lens newb)
    };

    simpleFixedLens :: (Functor m,FullEdit edita,FullEdit editb) => SimpleLens' m (Subject edita) (Subject editb) -> FixedLens' m edita editb;
    simpleFixedLens lens = MkFixedLens
    {
        fixedLensUpdate = makeFixedLensUpdate (simpleLensGet lens) (\_ -> Nothing),
        fixedLensSimple = lens,
        fixedLensPutEdit = \editb -> do
        {
            newb <- cofmap1CF (simpleLensGet lens) (applyEdit editb);
            ma <- simpleLensPutback lens newb;
            return (fmap replaceEdit ma);
        }
    };
}
