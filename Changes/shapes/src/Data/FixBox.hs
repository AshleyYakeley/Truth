module Data.FixBox
    ( FixBox
    , mkFixBox
    , mkConstructFixBox
    , mkRegisterFixBox
    , boxRecursiveFix
    , boxRecursiveIO
    , boxSequential
    ) where

import Control.FixIO
import Shapes.Import

data FixBox m a b =
    forall t. MkFixBox (t -> m ())
                       (a -> m (t, b))

instance Functor m => Functor (FixBox m a) where
    fmap pq (MkFixBox f m) = MkFixBox f $ \a -> fmap (\(t, p) -> (t, pq p)) $ m a

instance Applicative m => Applicative (FixBox m a) where
    pure b = MkFixBox (\_ -> pure ()) $ \_ -> pure ((), b)
    MkFixBox tmmp mtbc <*> MkFixBox tmmq mtb =
        MkFixBox (\(~(tp, tq)) -> liftA2 (\() () -> ()) (tmmp tp) (tmmq tq)) $ \a ->
            liftA2 (\(tp, bc) (tq, b) -> ((tp, tq), bc b)) (mtbc a) (mtb a)

instance (Applicative m, Semigroup b) => Semigroup (FixBox m a b) where
    (<>) = liftA2 (<>)

instance (Applicative m, Monoid b) => Monoid (FixBox m a b) where
    mempty = pure mempty

instance Monad m => Category (FixBox m) where
    id = MkFixBox (\_ -> pure ()) $ \a -> pure ((), a)
    MkFixBox tmmp bmtc . MkFixBox tmmq amtb =
        MkFixBox (\(~(tp, tq)) -> tmmp tp >> tmmq tq) $ \a -> do
            (tq, b) <- amtb a
            (tp, c) <- bmtc b
            pure ((tp, tq), c)

instance Monad m => Arrow (FixBox m) where
    arr f = MkFixBox (\_ -> pure ()) $ \a -> pure ((), f a)
    first (MkFixBox tmm bmtc) =
        MkFixBox tmm $ \(b, d) -> do
            (t, c) <- bmtc b
            return (t, (c, d))
    second (MkFixBox tmm bmtc) =
        MkFixBox tmm $ \(d, b) -> do
            (t, c) <- bmtc b
            return (t, (d, c))

mkFixBox :: (t -> m ()) -> (a -> m (t, b)) -> FixBox m a b
mkFixBox = MkFixBox

mkConstructFixBox :: Monad m => (a -> m b) -> FixBox m a b
mkConstructFixBox construct =
    mkFixBox (\_ -> return ()) $ \a -> do
        b <- construct a
        return ((), b)

mkRegisterFixBox :: Monad m => (a -> m ()) -> FixBox m a ()
mkRegisterFixBox register = mkFixBox register $ \a -> return (a, ())

boxRecursive :: Monad m => (forall r. (r -> m r) -> m r) -> FixBox m a b -> a -> m b
boxRecursive mf (MkFixBox register construct) a = do
    (_, b) <-
        mf $ \(~(t, _)) -> do
            register t
            construct a
    return b

boxRecursiveFix :: MonadFix m => FixBox m a b -> a -> m b
boxRecursiveFix = boxRecursive mfix

boxRecursiveIO :: MonadIO m => FixBox m a b -> a -> m b
boxRecursiveIO = boxRecursive mfixIO

boxSequential :: Monad m => FixBox m a b -> a -> m b
boxSequential (MkFixBox register construct) a = do
    (t, b) <- construct a
    register t
    return b
