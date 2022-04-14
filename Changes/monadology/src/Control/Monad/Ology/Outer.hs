module Control.Monad.Ology.Outer where

import Import

-- | Instances of this type are isomorphic to @P -> a@ for some type @P@.
class Monad m => MonadOuter m where
    outerCommute ::
           forall f a. Functor f
        => f (m a)
        -> m (f a)

instance MonadOuter Identity where
    outerCommute fia = Identity $ fmap runIdentity fia

instance MonadOuter ((->) r) where
    outerCommute fma r = fmap (\ra -> ra r) fma

instance MonadOuter m => MonadOuter (IdentityT m) where
    outerCommute fima = IdentityT $ outerCommute $ fmap runIdentityT fima

instance MonadOuter m => MonadOuter (ReaderT r m) where
    outerCommute fra = ReaderT $ \r -> outerCommute $ fmap (\(ReaderT rma) -> rma r) fra
