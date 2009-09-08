module Data.Changes.FixedLens where
{
    import Data.Changes.FloatingLens;
    import Data.Changes.WholeEdit;
    import Data.Changes.Edit;
    import Data.Bijection;
    import Data.Codec;
    import Data.Result;
    import Data.ConstFunction;
    import Data.Traversable;
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
        fixedLensGet :: Subject edita -> Subject editb,
        fixedLensPutEdit :: editb -> ConstFunction (Subject edita) (m edita)
    };
    
    type FixedLens = FixedLens' Maybe;
    
    makeFixedLensUpdate :: (Edit edita,Edit editb) =>
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
            fixedLensGet = id,
            fixedLensPutEdit = \editb -> pure (pure editb)
        };
        bc . ab = MkFixedLens
        {
            fixedLensUpdate = \edita -> do
            {
                meb <- fixedLensUpdate ab edita;
                case meb of
                {
                    Just editb -> cofmap1CF (fixedLensGet ab) (fixedLensUpdate bc editb);
                    _ -> return Nothing;
                };
            },
            fixedLensGet = (fixedLensGet bc) . (fixedLensGet ab),
            fixedLensPutEdit = \editc -> do
            {
                meditb <- cofmap1CF (fixedLensGet ab) (fixedLensPutEdit bc editc);
                case retrieveOne meditb of
                {
                    SuccessResult editb -> fixedLensPutEdit ab editb;
                    FailureResult ff -> return ff;
                };
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
        lensGet = \_ -> fixedLensGet lens,
        lensPutEdit = \_ -> fixedLensPutEdit lens
    };
    
    data CleanLens' m edita editb = MkCleanLens
    {
        cleanLensUpdate :: edita -> Maybe editb,
        cleanLensGet :: (Subject edita) -> (Subject editb),
        cleanLensPutEdit :: editb -> m edita
    };
    
    --type CleanLens = CleanLens' Maybe;
    
    instance (Monad m) => Category (CleanLens' m) where
    {
        id = MkCleanLens
        {
            cleanLensUpdate = Just,
            cleanLensGet = id,
            cleanLensPutEdit = return
        };
        bc . ab = MkCleanLens
        {
            cleanLensUpdate = \edita -> do
            {
                editb <- cleanLensUpdate ab edita;
                cleanLensUpdate bc editb;
            },
            cleanLensGet = (cleanLensGet bc) . (cleanLensGet ab),

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
        fixedLensGet = cleanLensGet lens,
        fixedLensPutEdit = \edit -> pure (cleanLensPutEdit lens edit)
    };
    
    -- | A SimpleLens is a FixedLens that doesn't bother with Edits.
    ;
    data SimpleLens' m a b = MkSimpleLens
    {
        simpleLensGet :: a -> b,
        simpleLensPutback :: b -> ConstFunction a (m a)
    };
    
    type SimpleLens = SimpleLens' Maybe;
    
    instance (Applicative m,FunctorOne m) => Category (SimpleLens' m) where
    {
        id = MkSimpleLens
        {
            simpleLensGet = id,
            simpleLensPutback = \b -> pure (pure b)
        };
        bc . ab = MkSimpleLens
        {
            simpleLensGet = (simpleLensGet bc) . (simpleLensGet ab),
            simpleLensPutback = \c -> do
            {
                mb <- cofmap1CF (simpleLensGet ab) (simpleLensPutback bc c);
                case retrieveOne mb of
                {
                    SuccessResult b -> simpleLensPutback ab b;
                    FailureResult ff -> return ff;
                }
            }
        };
    };
    
    instance (Traversable f,FunctorOne f,Applicative m) => CatFunctor (SimpleLens' m) f where
    {
        cfmap lens = MkSimpleLens
        {
            simpleLensGet = fmap (simpleLensGet lens),
            simpleLensPutback = \fb -> do
            {
                ffa <- traverse (\b -> cfmap (simpleLensPutback lens b)) fb;
                return (sequenceA (joinOne ffa));
            }
        };
    };
    
    simpleWholeFixedLens :: (Functor m) => SimpleLens' m a b -> FixedLens' m (WholeEdit a) (WholeEdit b);
    simpleWholeFixedLens lens = MkFixedLens
    {
        fixedLensUpdate = \(MkWholeEdit a) -> pure (Just (MkWholeEdit (simpleLensGet lens a))),
        fixedLensGet = simpleLensGet lens,
        fixedLensPutEdit = \(MkWholeEdit newb) -> fmap (fmap MkWholeEdit) (simpleLensPutback lens newb)
    };
    
    simpleFixedLens :: (Functor m,Edit edita,Edit editb) => SimpleLens' m (Subject edita) (Subject editb) -> FixedLens' m edita editb;
    simpleFixedLens lens = MkFixedLens
    {
        fixedLensUpdate = makeFixedLensUpdate (simpleLensGet lens) (\_ -> Nothing),
        fixedLensGet = simpleLensGet lens,
        fixedLensPutEdit = \editb -> do
        {
            newb <- cofmap1CF (simpleLensGet lens) (applyEdit editb);
            ma <- simpleLensPutback lens newb;
            return (fmap replaceEdit ma);
        }
    };
    
    data WholeLens' m a b = MkWholeLens
    {
        wholeLensGet :: a -> b,
        wholeLensPutback :: b -> m a
    };
    
    type WholeLens = WholeLens' Maybe;
    
    instance (Applicative m,FunctorOne m) => Category (WholeLens' m) where
    {
        id = MkWholeLens
        {
            wholeLensGet = id,
            wholeLensPutback = pure
        };
        bc . ab = MkWholeLens
        {
            wholeLensGet = (wholeLensGet bc) . (wholeLensGet ab),
            wholeLensPutback = \c -> case retrieveOne (wholeLensPutback bc c) of
            {
                SuccessResult b -> wholeLensPutback ab b;
                FailureResult ff -> ff;
            }
        };
    };
    
    instance (Traversable f,Applicative m) => CatFunctor (WholeLens' m) f where
    {
        cfmap lens = MkWholeLens
        {
            wholeLensGet = fmap (wholeLensGet lens),
            wholeLensPutback = traverse (wholeLensPutback lens)
        };
    };
    
    wholeSimpleLens :: WholeLens' m a b -> SimpleLens' m a b;
    wholeSimpleLens lens = MkSimpleLens
    {
        simpleLensGet = wholeLensGet lens,
        simpleLensPutback = \b -> pure (wholeLensPutback lens b)
    };
    
    resultWholeLens :: (a -> Result e b) -> (b -> a) -> WholeLens' Maybe a (Result e b);
    resultWholeLens decode' encode' = MkWholeLens
    {
        wholeLensGet = decode',
        wholeLensPutback = \r -> case r of
        {
            SuccessResult b -> Just (encode' b);
            _ -> Nothing;
        }
    };
    
    codecWholeLens :: Codec a b -> WholeLens' Maybe a (Maybe b);
    codecWholeLens codec = MkWholeLens
    {
        wholeLensGet = decode codec,
        wholeLensPutback = fmap (encode codec)
    };
    
    bijectionWholeLens :: (Applicative m) => Bijection a b -> WholeLens' m a b;
    bijectionWholeLens bi = MkWholeLens
    {
        wholeLensGet = biForwards bi,
        wholeLensPutback = pure . (biBackwards bi)
    };
}
