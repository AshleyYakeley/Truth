module Data.Injection where
{
    import Data.Bijection;
    import Data.Codec;
    import Data.Result;
    import Data.Traversable;
    import Data.Chain;
    import Control.Applicative;
    import Control.Category;
    import Prelude hiding (id,(.),sequence);

    data Injection a b = MkInjection
    {
        injForwards :: a -> b,
        injBackwards :: b -> Maybe a
    };

    instance Category Injection where
    {
        id = MkInjection
        {
            injForwards = id,
            injBackwards = Just
        };
        bc . ab = MkInjection
        {
            injForwards = (injForwards bc) . (injForwards ab),
            injBackwards = \c -> do
            {
                b <- injBackwards bc c;
                injBackwards ab b;
            }
        };
    };

    instance (Traversable f) => CatFunctor Injection f where
    {
        cfmap lens = MkInjection
        {
            injForwards = fmap (injForwards lens),
            injBackwards = traverse (injBackwards lens)
        };
    };

    resultInjection :: (a -> Result e b) -> (b -> a) -> Injection a (Result e b);
    resultInjection decode' encode' = MkInjection
    {
        injForwards = decode',
        injBackwards = \r -> case r of
        {
            SuccessResult b -> Just (encode' b);
            _ -> Nothing;
        }
    };

    codecInjection :: Codec a b -> Injection a (Maybe b);
    codecInjection codec = MkInjection
    {
        injForwards = decode codec,
        injBackwards = fmap (encode codec)
    };

    bijectionInjection :: Bijection a b -> Injection a b;
    bijectionInjection bi = MkInjection
    {
        injForwards = biForwards bi,
        injBackwards = pure . (biBackwards bi)
    };
}
