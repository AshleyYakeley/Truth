module Data.Lens where
{
    import Data.Injection;
    import Data.Result;
    import Data.ConstFunction;
    import Data.Traversable;
    import Data.FunctorOne;
    import Data.Chain;
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

    toLens :: (FunctorOne m) => Lens' m edita editb -> Lens edita editb;
    toLens lens = MkLens
    {
        lensGet = lensGet lens,
        lensPutback = \b -> do
        {
            ma <- lensPutback lens b;
            return (getMaybeOne ma);
        }
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

    injectionLens :: Injection a b -> Lens a b;
    injectionLens lens = MkLens
    {
        lensGet = injForwards lens,
        lensPutback = \b -> pure (injBackwards lens b)
    };
}
