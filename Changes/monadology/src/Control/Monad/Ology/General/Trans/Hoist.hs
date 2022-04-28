module Control.Monad.Ology.General.Trans.Hoist where

import Control.Monad.Ology.General.Function
import Control.Monad.Ology.General.IO
import Control.Monad.Ology.General.Trans.Constraint
import Control.Monad.Ology.General.Trans.Trans
import Import

type MonadTransHoist :: TransKind -> Constraint
class (MonadTrans t, TransConstraint Monad t) => MonadTransHoist t where
    hoist ::
           forall m1 m2. (Monad m1, Monad m2)
        => (m1 --> m2)
        -> t m1 --> t m2

hoistTransform :: (MonadTransHoist t, Monad m1, Monad m2) => (m1 --> m2) -> WMFunction (t m2) --> WMFunction (t m1)
hoistTransform ff (MkWMFunction r2) = MkWMFunction $ \m1a -> r2 $ hoist ff m1a

class MonadIO m => MonadHoistIO m where
    hoistIO :: (IO --> IO) -> m --> m

instance MonadHoistIO IO where
    hoistIO f = f

instance (MonadTransHoist t, MonadHoistIO m, MonadIO (t m)) => MonadHoistIO (t m) where
    hoistIO f = hoist $ hoistIO f

instance (MonadTransHoist t, TransConstraint MonadIO t) => TransConstraint MonadHoistIO t where
    hasTransConstraint = withTransConstraintDict @MonadIO $ Dict
