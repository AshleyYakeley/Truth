module Control.Monad.Ology.General.Extract where

import Control.Monad.Ology.General.Inner
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
