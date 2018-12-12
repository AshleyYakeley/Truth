module Control.Monad.Trans.StackIO where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Transform
import Control.Monad.Trans.Tunnel
import Control.Monad.Trans.Unlift
import Data.Constraint
import Data.Kind
import Prelude

class (MonadTransUnlift (MonadStackTrans m), MonadUnliftIO m) => MonadStackIO m where
    type MonadStackTrans m :: (* -> *) -> (* -> *)
    toMonadStack :: forall a. m a -> MonadStackTrans m IO a
    fromMonadStack :: forall a. MonadStackTrans m IO a -> m a

instance MonadStackIO IO where
    type MonadStackTrans IO = IdentityT
    toMonadStack = IdentityT
    fromMonadStack = runIdentityT

instance (MonadTransUnlift t, MonadStackIO m, MonadUnliftIO (t m)) => MonadStackIO (t m) where
    type MonadStackTrans (t m) = ComposeT t (MonadStackTrans m)
    toMonadStack tma = MkComposeT $ remonad toMonadStack tma
    fromMonadStack (MkComposeT tt) = remonad fromMonadStack tt

instance MonadTransUnlift t => MonadTransConstraint MonadStackIO t where
    hasTransConstraint ::
           forall m. MonadStackIO m
        => Dict (MonadStackIO (t m))
    hasTransConstraint =
        case hasTransConstraint @MonadUnliftIO @t @m of
            Dict -> Dict

isCombineMonadIO ::
       forall ma mb. (MonadStackIO ma, MonadStackIO mb)
    => Dict (MonadStackIO (CombineMonadIO ma mb))
isCombineMonadIO =
    case hasTransConstraint @MonadUnliftIO @(MonadStackTrans ma) @mb of
        Dict -> Dict

type CombineMonadIO ma mb = MonadStackTrans ma mb

combineLiftFst ::
       forall ma mb r. (MonadStackIO ma, MonadIO mb)
    => ma r
    -> CombineMonadIO ma mb r
combineLiftFst mar = remonad liftIO $ toMonadStack mar

combineLiftSnd ::
       forall ma mb r. (MonadStackIO ma, Monad mb)
    => mb r
    -> CombineMonadIO ma mb r
combineLiftSnd = lift

combineUnliftIOs :: MonadStackIO ma => UnliftIO ma -> UnliftIO mb -> UnliftIO (CombineMonadIO ma mb)
combineUnliftIOs (MkTransform unlifta) (MkTransform unliftb) =
    MkTransform $ \cmr -> unlifta $ fromMonadStack $ remonad unliftb cmr
