{-# OPTIONS -fno-warn-orphans #-}
module Data.Searchable where
{
    import Data.Countable;
    import Data.Maybe;
    import Control.Applicative;
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
            Just first -> let
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
                        search (csm . (prepend s'));
                        return s';
                    });
                    s = case mx of
                    {
                        Just s' -> s';
                        _ -> first;
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
    };

    firstInList :: [Maybe a] -> Maybe a;
    firstInList [] = Nothing;
    firstInList ((Just a):_) = Just a;
    firstInList (Nothing:mas) = firstInList mas;

    finiteSearch :: (Finite a) => (a -> Maybe b) -> Maybe b;
    finiteSearch p = firstInList (fmap p allValues);

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
    };

    instance Searchable Bool where
    {
        search = finiteSearch;
    };

    instance Finite Bool where
    {
        allValues = [False,True];
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
