module Data.Countable where
{
    import Data.Nothing;

    class (Eq a) => Countable a where
    {
        countPrevious :: a -> Maybe a;
        countMaybeNext :: Maybe a -> Maybe a;
    };

    countDown :: (Countable a) => a -> [a];
    countDown a = case countPrevious a of
    {
        Just a' -> a':(countDown a');
        Nothing -> [];
    };

    instance Countable Nothing where
    {
        countPrevious = never;
        countMaybeNext Nothing = Nothing;
        countMaybeNext (Just n) = never n;
    };

    instance Countable () where
    {
        countPrevious () = Nothing;
        countMaybeNext Nothing = Just ();
        countMaybeNext (Just ()) = Nothing;
    };

    instance Countable Bool where
    {
        countPrevious True = Just False;
        countPrevious False = Nothing;
        countMaybeNext Nothing = Just False;
        countMaybeNext (Just False) = Just True;
        countMaybeNext (Just True) = Nothing;
    };

    instance Countable Integer where
    {
        countPrevious 0 = Nothing;
        countPrevious a | a < 0 = Just (- a - 1);
        countPrevious a = Just (- a);
        countMaybeNext = Just . countNext;
    };

    instance (Countable a) => Countable (Maybe a) where
    {
        countPrevious = fmap countPrevious;
        countMaybeNext Nothing = Just Nothing;
        countMaybeNext (Just ma) = fmap Just (countMaybeNext ma);
    };

    maybeRecount :: (Countable a,Countable b) => a -> Maybe b;
    maybeRecount a = case countPrevious a of
    {
        Just a' -> do
        {
            b' <- maybeRecount a';
            countMaybeNext b';
        };
        Nothing -> countMaybeNext Nothing;
    };

    {-
    Right 0
    Left 0
    Right 1
    Left 1
    Left 2
    Left 3
    -}

    instance (Countable a,Countable b) => Countable (Either a b) where
    {
        countPrevious (Right b) = case countPrevious b of
        {
            Just b' -> case maybeRecount b' of
            {
                Just a -> Just (Left a);
                Nothing -> Just (Right b);
            };
            Nothing -> Nothing;
        };
        countPrevious (Left a) = case maybeRecount a of
        {
            Just b -> Just (Right b);
            Nothing -> fmap Left (countPrevious a);
        };

        countMaybeNext Nothing = case countMaybeNext Nothing of
        {
            Just b -> Just (Right b);
            Nothing -> fmap Left (countMaybeNext Nothing);
        };
        countMaybeNext (Just (Right b)) = case maybeRecount b of
        {
            Just a -> Just (Left a);
            Nothing -> fmap Right (countMaybeNext (Just b));
        };
        countMaybeNext (Just (Left a)) = case maybeRecount a >>= (countMaybeNext . Just) of
        {
            Just b -> (Just (Right b));
            Nothing -> fmap Left (countMaybeNext (Just a));
        };
    };

    countDownUp :: (Countable down,Countable up) => (down,up) -> Maybe (down,up);
    countDownUp (down,up) = do
    {
        down' <- countPrevious down;
        up' <- countMaybeNext (Just up);
        return (down',up');
    };

    countUpDown :: (Countable up,Countable down) => (up,down) -> Maybe (up,down);
    countUpDown (up,down) = do
    {
        up' <- countMaybeNext (Just up);
        down' <- countPrevious down;
        return (up',down');
    };

    finalIteration :: (a -> Maybe a) -> a -> a;
    finalIteration f a = case f a of
    {
        Just a' -> finalIteration f a';
        Nothing -> a;
    };

    instance (Countable a,Countable b) => Countable (a,b) where
    {
        countPrevious ab = case countUpDown ab of
        {
            Just ab' -> Just ab';
            _ -> let
            {
                (a',b') = finalIteration countDownUp ab;
            } in case countPrevious a' of
            {
                Just a'' -> Just (a'',b');
                Nothing -> case countPrevious b' of
                {
                    Just b'' -> Just (a',b'');
                    Nothing -> Nothing;
                };
            };
        };

        countMaybeNext Nothing = do
        {
            a <- countMaybeNext Nothing;
            b <- countMaybeNext Nothing;
            return (a,b);
        };
        countMaybeNext (Just ab) = case countDownUp ab of
        {
            Just ab' -> Just ab';
            _ -> let
            {
                (a',b') = finalIteration countUpDown ab;
            } in case countMaybeNext (Just a') of
            {
                Just a'' -> Just (a'',b');
                Nothing -> case countMaybeNext (Just b') of
                {
                    Just b'' -> Just (a',b'');
                    Nothing -> Nothing;
                };
            };
        };
    };

    class (Countable a) => AtLeastOneCountable a where
    {
        countFirst :: a;
    };

    instance AtLeastOneCountable Integer where
    {
        countFirst = 0;
    };

    instance (Countable a) => AtLeastOneCountable (Maybe a) where
    {
        countFirst = Nothing;
    };

    instance (Countable a,AtLeastOneCountable b) => AtLeastOneCountable (Either a b) where
    {
        countFirst = Right countFirst;
    };

    instance (AtLeastOneCountable a,AtLeastOneCountable b) => AtLeastOneCountable (a,b) where
    {
        countFirst = (countFirst,countFirst);
    };

    class (AtLeastOneCountable a) => InfiniteCountable a where
    {
        countNext :: Maybe a -> a;
    };

    instance InfiniteCountable Integer where
    {
        countNext Nothing = 0;
        countNext (Just a) | a < 0 = - a;
        countNext (Just a) = - a - 1;
    };

    instance (InfiniteCountable a) => InfiniteCountable (Maybe a) where
    {
        countNext = fmap countNext;
    };

    instance (AtLeastOneCountable a,InfiniteCountable b) => InfiniteCountable (a,b) where
    {
        countNext Nothing = (countFirst,countNext Nothing);
        countNext (Just ab) = case countDownUp ab of
        {
            Just ab' -> ab';
            _ -> let
            {
                (a',b') = finalIteration countUpDown ab;
            } in case countMaybeNext (Just a') of
            {
                Just a'' -> (a'',b');
                Nothing -> (a',countNext (Just b'));
            };
        };
    };

    recount :: (Countable a,InfiniteCountable b) => a -> b;
    recount = countNext . (fmap recount) . countPrevious;

    instance (Countable a,InfiniteCountable b) => InfiniteCountable (Either a b) where
    {
        countNext Nothing = Right (countNext Nothing);
        countNext (Just (Right b)) = case maybeRecount b of
        {
            Just a -> Left a;
            Nothing -> Right (countNext (Just b));
        };
        countNext (Just (Left a)) = Right (countNext (recount a));
    };
}
