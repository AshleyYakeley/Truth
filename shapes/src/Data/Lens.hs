module Data.Lens where
{
    import Shapes.Import;
    import Data.Chain;
    import Data.MonadOne;
    import Data.Result;
    import Data.Bijection;
    import Data.Injection;
    import Data.Codec;


    data Lens' m a b = MkLens
    {
        lensGet :: a -> b,
        lensPutback :: b -> a -> m a
    };

    lensModify :: Lens' m a b -> (b -> b) -> a -> m a;
    lensModify lens bb a = lensPutback lens (bb (lensGet lens a)) a;

    lensMap :: (MonadOne m) => Lens' m a b -> (b -> b) -> (a -> a);
    lensMap lens bb a = case getMaybeOne (lensModify lens bb a) of
    {
        Just a' -> a';
        _ -> a;
    };

    lensAllowed :: (MonadOne m) =>
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

    instance (Applicative m,MonadOne m) => Category (Lens' m) where
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

    instance (Applicative m, MonadOne m) => CategoryOr (Lens' m) where
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

    instance (Traversable f,Applicative f,Applicative m) => CatFunctor (Lens' m) f where
    {
        cfmap lens = MkLens
        {
            lensGet = fmap (lensGet lens),
            lensPutback = \fb fa -> sequenceA (liftA2 (lensPutback lens) fb fa)
        };
    };

    fstLens :: Lens' Identity (a,b) a;
    fstLens = let
    {
        lensGet = fst;
        lensPutback a (_,b) = Identity (a,b);
    } in MkLens{..};

    sndLens :: Lens' Identity (a,b) b;
    sndLens = let
    {
        lensGet = snd;
        lensPutback b (a,_) = Identity (a,b);
    } in MkLens{..};

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

    hashMapLens :: (Eq key,Hashable key) => key -> Lens' Identity (HashMap key value) (Maybe value);
    hashMapLens key = let
    {
        lensGet = lookup key;
        lensPutback Nothing hm = Identity $ deleteMap key hm;
        lensPutback (Just value) hm = Identity $ insertMap key value hm;
    } in MkLens{..};
}
