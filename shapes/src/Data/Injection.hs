module Data.Injection where
{
    import Prelude hiding (id,(.),sequence);
    import Data.Functor.Identity;
    import Control.Category;
    import Data.Chain;
    import Data.Traversable;
    import Data.Result;
    import Data.Codec;
    import Data.Bijection;


    data Injection' m a b = MkInjection
    {
        injForwards :: a -> b,
        injBackwards :: b -> m a
    };

    instance IsBiMap Injection' where
    {
        mapBiMapM ff inj = MkInjection
        {
            injForwards = injForwards inj,
            injBackwards = \b -> ff (injBackwards inj b)
        };
    };

    type Injection = Injection' Maybe;

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

    codecInjection :: (Functor m) => Codec' m a b -> Injection' m a (m b);
    codecInjection codec = MkInjection
    {
        injForwards = decode codec,
        injBackwards = fmap (encode codec)
    };

    bijectionInjection :: Bijection a b -> Injection' Identity a b;
    bijectionInjection bi = MkInjection
    {
        injForwards = biForwards bi,
        injBackwards = Identity . (biBackwards bi)
    };
}
