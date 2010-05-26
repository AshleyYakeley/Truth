module Data.Lens where
{
    import Data.Codec;
    import Data.Searchable;
    import Data.Injection;
    import Data.Bijection;
    import Data.Result;
    import Data.ConstFunction;
    import Data.Traversable;
    import Data.FunctorOne;
    import Data.Chain;
    import Data.Witness;
    import Control.Monad.Identity;
    import Control.Applicative;
    import Control.Category;
    import Prelude hiding (id,(.),sequence);


    data Lens' m a b = MkLens
    {
        lensGet :: a -> b,
        lensPutback :: b -> ConstFunction a (m a)
    };

    lensModify :: Lens' m a b -> (b -> b) -> a -> m a;
    lensModify lens bb a = applyConstFunction (lensPutback lens (bb (lensGet lens a))) a;

    lensMap :: (FunctorOne m) => Lens' m a b -> (b -> b) -> (a -> a);
    lensMap lens bb a = case getMaybeOne (lensModify lens bb a) of
    {
        Just a' -> a';
        _ -> a;
    };

    type Lens = Lens' Maybe;

    instance IsBiMap Lens' where
    {
        mapBiMapM ff lens = MkLens
        {
            lensGet = lensGet lens,
            lensPutback = \b -> do
            {
                ma <- lensPutback lens b;
                return (ff ma);
            }
        };
    };

    instance (Applicative m,FunctorOne m) => Category (Lens' m) where
    {
        id = MkLens
        {
            lensGet = id,
            lensPutback = \b -> pure (pure b)
        };
        bc . ab = MkLens
        {
            lensGet = (lensGet bc) . (lensGet ab),
            lensPutback = \c -> do
            {
                mb <- cofmap1CF (lensGet ab) (lensPutback bc c);
                case retrieveOne mb of
                {
                    SuccessResult b -> lensPutback ab b;
                    FailureResult ff -> return ff;
                }
            }
        };
    };

    instance (Applicative m, FunctorOne m) => CategoryOr (Lens' m) where
    {
        ac ||| bc = MkLens
        {
            lensGet = \eab -> case eab of
            {
                Left a -> lensGet ac a;
                Right b -> lensGet bc b;
            },
            lensPutback = \c -> FunctionConstFunction (\eab -> case eab of
            {
                Left a -> fmap Left (applyConstFunction (lensPutback ac c) a);
                Right b -> fmap Right (applyConstFunction (lensPutback bc c) b);
            })
        };
    };

    instance (FunctorOne f,Applicative m) => CatFunctor (Lens' m) f where
    {
        cfmap lens = MkLens
        {
            lensGet = fmap (lensGet lens),
            lensPutback = \fb -> do
            {
                ffa <- traverse (\b -> cfmap (lensPutback lens b)) fb;
                return (sequenceA (joinOne ffa));
            }
        };
    };

    liftFLens :: (Applicative m,Finite p) => Lens' m a b -> Lens' m (p -> a) (p -> b);
    liftFLens lens = MkLens
    {
        lensGet = \pa p -> lensGet lens (pa p),
        lensPutback = (fmap (assemble . (\ppb p -> ppb p p))) . cfmapApplicative . assemble . (\pb -> (lensPutback lens) . pb)
    };

    bijectionLens :: Bijection a b -> Lens' Identity a b;
    bijectionLens (MkBijection ab ba) = MkLens ab (\b -> ConstConstFunction (return (ba b)));

    injectionLens :: Injection' m a b -> Lens' m a b;
    injectionLens lens = MkLens
    {
        lensGet = injForwards lens,
        lensPutback = \b -> pure (injBackwards lens b)
    };

    listElementLens :: (HasListElement n l) =>
       Nat n -> Lens' Identity l (ListElement n l);
    listElementLens n = MkLens
    {
        lensGet = getListElement n,
        lensPutback = \e -> FunctionConstFunction (return . (putListElement n e))
    };
}
