module Control.Monad.Ology.Trans.Transform where

import Control.Monad.Ology.Data
import Control.Monad.Ology.Function
import Control.Monad.Ology.Trans.Tunnel
import Control.Monad.Ology.Trans.Unlift
import Import

type TransformT :: forall k. (k -> Type) -> Type -> Type
newtype TransformT f a = MkTransformT
    { runTransformT :: forall r. (a -> f r) -> f r
    }

instance Functor (TransformT f) where
    fmap ab (MkTransformT aff) = MkTransformT $ \bf -> aff $ bf . ab

instance Applicative (TransformT f) where
    pure a = MkTransformT $ \af -> af a
    MkTransformT f <*> MkTransformT x = MkTransformT $ \bf -> f $ \ab -> x (bf . ab)

instance Monad (TransformT f) where
    return = pure
    MkTransformT m >>= f = MkTransformT $ \bf -> m (\a -> runTransformT (f a) bf)

instance MonadTrans TransformT where
    lift m = MkTransformT $ \af -> m >>= af

instance MonadIO m => MonadIO (TransformT m) where
    liftIO = lift . liftIO

instance (Functor f, Semigroup a) => Semigroup (TransformT f a) where
    (<>) = liftA2 (<>)

instance (Functor f, Monoid a) => Monoid (TransformT f a) where
    mempty = pure mempty

mapTransformT :: (f --> f) -> TransformT f ()
mapTransformT ff = MkTransformT $ \uf -> ff $ uf ()

execMapTransformT :: Monad f => f (TransformT f a) -> TransformT f a
execMapTransformT ffa =
    MkTransformT $ \af -> do
        MkTransformT aff <- ffa
        aff af

transformParamRef ::
       forall m a. Monad m
    => Param m a
    -> Ref (TransformT m) a
transformParamRef MkParam {..} = let
    getD :: TransformT m a
    getD =
        MkTransformT $ \afr -> do
            a <- askD
            afr a
    modifyD :: (a -> a) -> TransformT m ()
    modifyD aa = MkTransformT $ \ufr -> localD aa $ ufr ()
    in MkRef {..}

liftTransformT ::
       forall t m. (MonadTransUnlift t, MonadTunnelIO m)
    => TransformT m --> TransformT (t m)
liftTransformT (MkTransformT aff) = MkTransformT $ \atf -> liftWithUnlift $ \unlift -> aff $ unlift . atf
