module Data.Changes.WholeLens where
{
    import Data.Bijection;
    import Data.Codec;
    import Data.Result;
    import Data.Traversable;
    import Data.FunctorOne;
    import Data.Chain;
    import Control.Applicative;
    import Control.Category;
    import Prelude hiding (id,(.),sequence);

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
