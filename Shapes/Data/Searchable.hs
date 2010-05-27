{-# OPTIONS -fno-warn-orphans #-}
module Data.Searchable where
{
    import Data.Countable;
    import Data.Monoid;
    import Data.Maybe;
    import Control.Applicative;
    import Data.Foldable hiding (find);
    import Data.Traversable;
    import Data.Nothing;

    class Searchable a where
    {
        search :: forall b. (a -> Maybe b) -> Maybe b;
    };

    forsome :: (Searchable a) => (a -> Bool) -> Bool;
    forsome = isJust . search . (\ab a -> if ab a then Just () else Nothing);

    forevery :: (Searchable a) => (a -> Bool) -> Bool;
    forevery p = not (forsome (not . p));

    instance (Searchable a) => Searchable (Maybe a) where
    {
        search mamb = case mamb Nothing of
        {
            Just b -> Just b;
            Nothing -> search (mamb . Just);
        };
    };

    instance (Searchable a,Searchable b) => Searchable (Either a b) where
    {
        search eabb = case search (eabb . Left) of
        {
            Just b -> Just b;
            _ -> search (eabb . Right);
        }
    };

    instance (Searchable a,Searchable b) => Searchable (a,b) where
    {
        search abb = search (\a -> search (\b -> abb (a,b)));
    };

    instance (Countable c,Searchable s) => Searchable (c -> s) where
    {
        search csmx = case search Just of
        {
            Just def -> let
            {
                prepend :: s -> (c -> s) -> c -> s;
                prepend s cs c = case countPrevious c of
                {
                    Just c' -> cs c';
                    Nothing -> s;
                };

                find :: ((c -> s) -> Maybe x) -> c -> s;
                find csm = let
                {
                    mx = search (\s' -> do
                    {
                        _ <- search (csm . (prepend s'));
                        return s';
                    });
                    s = case mx of
                    {
                        Just s' -> s';
                        _ -> def;
                    };
                } in prepend s (find (csm . (prepend s)));
            } in csmx (find csmx);
            Nothing -> Nothing;
        };
    };

    instance (Searchable a,Eq b) => Eq (a -> b) where
    {
        p == q = forevery (\a -> p a == q a);
    };

    class (Searchable a,Countable a) => Finite a where
    {
        allValues :: [a];

        assemble :: forall f b. (Applicative f) => (a -> f b) -> f (a -> b);
        assemble afb = fmap listLookup (traverse (\a -> fmap (\b -> (a,b)) (afb a)) allValues) where
        {
            listLookup :: [(a,b)] -> a -> b;
            listLookup [] _ = error "missing value";    -- this should never happen
            listLookup ((a,b):_) a' | a == a' = b;
            listLookup (_:l) a' = listLookup l a';
        };
    };

    instance (Finite t) => Foldable ((->) t) where
    {
        foldMap am ta = mconcat (fmap (am . ta) allValues);
    };

    instance (Finite a) => Traversable ((->) a) where
    {
        sequenceA = assemble;
    };

    firstJust :: [Maybe a] -> Maybe a;
    firstJust [] = Nothing;
    firstJust ((Just a):_) = Just a;
    firstJust (Nothing:mas) = firstJust mas;

    finiteSearch :: (Finite a) => (a -> Maybe b) -> Maybe b;
    finiteSearch p = firstJust (fmap p allValues);

    finiteCountPrevious :: (Finite a) => a -> Maybe a;
    finiteCountPrevious x = findp Nothing allValues where
    {
        findp ma (a:_) | a == x = ma;
        findp _ (a:as) = findp (Just a) as;
        findp _ [] = seq x (error "missing value");
    };

    firstItem :: [a] -> Maybe a;
    firstItem [] = Nothing;
    firstItem (a:_) = Just a;

    finiteCountMaybeNext :: (Finite a) => Maybe a -> Maybe a;
    finiteCountMaybeNext Nothing = firstItem allValues;
    finiteCountMaybeNext (Just x) = findmn allValues where
    {
        findmn (a:as) | x == a = firstItem as;
        findmn (_:as) = findmn as;
        findmn [] = seq x (error "missing value");
    };

    instance Searchable Nothing where
    {
        search = finiteSearch;
    };

    instance Finite Nothing where
    {
        allValues = [];
    };

    instance Searchable () where
    {
        search = finiteSearch;
    };

    instance Finite () where
    {
        allValues = [()];
        assemble afb = liftA (\v _ -> v) (afb ());
    };

    instance Searchable Bool where
    {
        search = finiteSearch;
    };

    instance Finite Bool where
    {
        allValues = [False,True];
        assemble afb = liftA2 (\f t x -> if x then t else f) (afb False) (afb True);
    };

    instance (Finite a) => Finite (Maybe a) where
    {
        allValues = Nothing:(fmap Just allValues);
    };

    instance (Finite a,Finite b) => Finite (Either a b) where
    {
        allValues = (fmap Left allValues) ++ (fmap Right allValues);
    };

    instance (Finite a,Finite b) => Finite (a,b) where
    {
        allValues = liftA2 (,) allValues allValues;
    };
}
