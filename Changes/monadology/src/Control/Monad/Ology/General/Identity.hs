module Control.Monad.Ology.General.Identity where

import Control.Monad.Ology.General.Extract
import Control.Monad.Ology.General.Outer
import Import

-- | Instances of this type are isomorphic to @Identity@.
class (MonadOuter m, MonadExtract m) => MonadIdentity m

instance MonadIdentity Identity
