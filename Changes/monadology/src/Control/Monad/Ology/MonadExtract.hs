module Control.Monad.Ology.MonadExtract where

import Control.Monad.Ology.MonadInner
import Import

-- | Instances of this type are isomorphic to @(Q,a)@ for some type @Q@ (with @Monoid Q@).
class MonadInner m => MonadExtract m where
    mToValue :: forall a. m a -> a

instance MonadExtract Identity where
    mToValue (Identity a) = a

instance Monoid p => MonadExtract ((,) p) where
    mToValue (_, a) = a

instance MonadExtract (Either Void) where
    mToValue (Left p) = absurd p
    mToValue (Right a) = a

instance MonadExtract m => MonadExtract (IdentityT m) where
    mToValue (IdentityT ma) = mToValue ma

instance (MonadExtract m, Monoid w) => MonadExtract (WriterT w m) where
    mToValue (WriterT maw) = fst $ mToValue maw
