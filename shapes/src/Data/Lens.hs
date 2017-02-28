module Data.Lens where
{
    import Data.Codec;
    import Data.Injection;
    import Data.Bijection;
    import Data.Result;
    import Data.Traversable;
    import Data.FunctorOne;
    import Data.Chain;
    import Data.Witness;
    import Data.Functor.Identity;
    import Control.Applicative;
    import Control.Category;
    import Data.Maybe;
    import Prelude hiding (id,(.),sequence);


    data Lens' m a b = MkLens
    {
        lensGet :: a -> b,
        lensPutback :: b -> a -> m a
    };

    lensModify :: Lens' m a b -> (b -> b) -> a -> m a;
    lensModify lens bb a = lensPutback lens (bb (lensGet lens a)) a;

    lensMap :: (FunctorOne m) => Lens' m a b -> (b -> b) -> (a -> a);
    lensMap lens bb a = case getMaybeOne (lensModify lens bb a) of
    {
        Just a' -> a';
        _ -> a;
    };

    lensAllowed :: (FunctorOne m) =>
     Lens' m a b -> b -> a -> Bool;
    lensAllowed lens b a = isJust $ getMaybeOne $ lensPutback lens b a;

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
            lensPutback = \b _ -> pure b
        };
        bc . ab = MkLens
        {
            lensGet = (lensGet bc) . (lensGet ab),
            lensPutback = \c a -> case retrieveOne (lensPutback bc c (lensGet ab a)) of
            {
                SuccessResult b -> lensPutback ab b a;
                FailureResult (MkLimit ff) -> ff;
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
            lensPutback = \c eab -> case eab of
            {
                Left a -> fmap Left (lensPutback ac c a);
                Right b -> fmap Right (lensPutback bc c b);
            }
        };
    };

    instance (Traversable f,FunctorAp f,Applicative m) => CatFunctor (Lens' m) f where
    {
        cfmap lens = MkLens
        {
            lensGet = fmap (lensGet lens),
            lensPutback = \fb fa -> sequenceA (liftF2 (lensPutback lens) fb fa)
        };
    };

    pickLens :: (Eq p) => p -> Lens' Identity (p -> a) a;
    pickLens p = MkLens
    {
        lensGet = \pa -> pa p,
        lensPutback = \a pa -> Identity (\p' -> if p == p' then a else pa p')
    };

    bijectionLens :: Bijection a b -> Lens' Identity a b;
    bijectionLens (MkBijection ab ba) = MkLens ab (\b _ -> return (ba b));

    injectionLens :: Injection' m a b -> Lens' m a b;
    injectionLens lens = MkLens
    {
        lensGet = injForwards lens,
        lensPutback = \b -> pure (injBackwards lens b)
    };

    listElementLens :: (HasListElement n l) =>
       NatType n -> Lens' Identity l (ListElement n l);
    listElementLens n = MkLens
    {
        lensGet = getListElement n,
        lensPutback = \e -> return . (putListElement n e)
    };
}
