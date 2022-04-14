module Control.Monad.Ology.MonadIdentity where

import Control.Monad.Ology.MonadExtract
import Control.Monad.Ology.MonadOuter
import Import

-- | Instances of this type are isomorphic to @Identity@.
class (MonadOuter m, MonadExtract m) => MonadIdentity m

instance MonadIdentity Identity

instance MonadIdentity m => MonadIdentity (IdentityT m)
