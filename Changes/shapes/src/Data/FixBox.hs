module Data.FixBox
    ( FixBox
    , mkFixBox
    , boxFix
    , boxesFix
    , boxSeq
    ) where

import Shapes.Import

data FixBox m a b =
    forall t. MkFixBox (t -> WMFunction m m)
                       (a -> m (t, b))

instance Functor m => Functor (FixBox m a) where
    fmap pq (MkFixBox f m) = MkFixBox f $ \a -> fmap (\(t, p) -> (t, pq p)) $ m a

instance Applicative m => Applicative (FixBox m a) where
    pure b = MkFixBox (\_ -> id) $ \_ -> pure ((), b)
    MkFixBox tmmp mtbc <*> MkFixBox tmmq mtb =
        MkFixBox (\(~(tp, tq)) -> tmmp tp . tmmq tq) $ \a ->
            liftA2 (\(tp, bc) (tq, b) -> ((tp, tq), bc b)) (mtbc a) (mtb a)

instance (Applicative m, Semigroup b) => Semigroup (FixBox m a b) where
    (<>) = liftA2 (<>)

instance (Applicative m, Monoid b) => Monoid (FixBox m a b) where
    mempty = pure mempty

instance Monad m => Category (FixBox m) where
    id = MkFixBox (\_ -> id) $ \a -> pure ((), a)
    MkFixBox tmmp bmtc . MkFixBox tmmq amtb =
        MkFixBox (\(~(tp, tq)) -> tmmp tp . tmmq tq) $ \a -> do
            (tq, b) <- amtb a
            (tp, c) <- bmtc b
            pure ((tp, tq), c)

instance Monad m => Arrow (FixBox m) where
    arr f = MkFixBox (\_ -> id) $ \a -> pure ((), f a)
    first (MkFixBox tmm bmtc) =
        MkFixBox tmm $ \(b, d) -> do
            (t, c) <- bmtc b
            return (t, (c, d))
    second (MkFixBox tmm bmtc) =
        MkFixBox tmm $ \(d, b) -> do
            (t, c) <- bmtc b
            return (t, (d, c))

mkFixBox :: (t -> WMFunction m m) -> (a -> m (t, b)) -> FixBox m a b
mkFixBox = MkFixBox

boxFix :: MonadFix m => FixBox m a b -> m x -> a -> m (x, b)
boxFix (MkFixBox twmm mt) mx a = do
    (_, xb) <- mfix $ \(~(t, _)) -> runWMFunction (twmm t) $ liftA2 (\(t0, b) x -> (t0, (x, b))) (mt a) $ mx
    return xb

boxesFix :: (MonadFix m, Monoid b) => [FixBox m a b] -> m x -> a -> m (x, b)
boxesFix boxes = boxFix $ mconcat boxes

boxSeq :: Monad m => FixBox m a b -> m x -> a -> m (x, b)
boxSeq (MkFixBox twmm mtb) mx a = do
    (t, b) <- mtb a
    x <- runWMFunction (twmm t) mx
    return (x, b)
