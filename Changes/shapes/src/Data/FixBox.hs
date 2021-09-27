module Data.FixBox
    ( FixBox
    , mkFixBox
    , boxFix
    , boxesFix
    , boxSeq
    ) where

import Shapes.Import

data FixBox m a =
    forall t. MkFixBox (t -> WMFunction m m)
                       (m (t, a))

instance Functor m => Functor (FixBox m) where
    fmap ab (MkFixBox f m) = MkFixBox f $ fmap (\(t, a) -> (t, ab a)) m

instance Applicative m => Applicative (FixBox m) where
    pure a = MkFixBox (\_ -> id) (pure ((), a))
    MkFixBox twmmp mtab <*> MkFixBox twmmq mta =
        MkFixBox (\(~(tp, tq)) -> twmmp tp . twmmq tq) $ liftA2 (\(tp, ab) (tq, a) -> ((tp, tq), ab a)) mtab mta

instance (Applicative m, Monoid a) => Semigroup (FixBox m a) where
    (<>) = liftA2 (<>)

instance (Applicative m, Monoid a) => Monoid (FixBox m a) where
    mempty = pure mempty

mkFixBox :: (t -> WMFunction m m) -> m (t, a) -> FixBox m a
mkFixBox = MkFixBox

boxFix :: MonadFix m => FixBox m a -> m x -> m (x, a)
boxFix (MkFixBox twmm mt) mx = do
    (_, xa) <- mfix $ \(~(t, _)) -> runWMFunction (twmm t) $ liftA2 (\(t0, a) x -> (t0, (x, a))) mt mx
    return xa

boxesFix :: (MonadFix m, Monoid a) => [FixBox m a] -> m x -> m (x, a)
boxesFix boxes = boxFix $ mconcat boxes

boxSeq :: Monad m => FixBox m a -> m x -> m (x, a)
boxSeq (MkFixBox twmm mta) mx = do
    (t, a) <- mta
    x <- runWMFunction (twmm t) mx
    return (x, a)
