module Control.Monad.Trans.StackIO where

import Control.Monad.Trans.Compose
import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Tunnel
import Control.Monad.Trans.Unlift
import Shapes.Import

class (MonadTransUnlift (MonadStackTrans m), MonadUnliftIO m) =>
      MonadStackIO m where
    type MonadStackTrans m :: (* -> *) -> (* -> *)
    toMonadStack :: forall a. m a -> MonadStackTrans m IO a
    fromMonadStack :: forall a. MonadStackTrans m IO a -> m a

instance MonadStackIO IO where
    type MonadStackTrans IO = IdentityT
    toMonadStack = IdentityT
    fromMonadStack = runIdentityT

instance (MonadTransUnlift t, MonadStackIO m, MonadIO (t m)) => MonadStackIO (t m) where
    type MonadStackTrans (t m) = ComposeT t (MonadStackTrans m)
    toMonadStack tma = MkComposeT $ remonad toMonadStack tma
    fromMonadStack (MkComposeT tt) = remonad fromMonadStack tt

instance MonadTransUnlift t => MonadTransConstraint MonadStackIO t where
    hasTransConstraint ::
           forall m. MonadStackIO m
        => Dict (MonadStackIO (t m))
    hasTransConstraint =
        case hasTransConstraint @MonadIO @t @m of
            Dict -> Dict

isCombineMonadIO ::
       forall ma mb. (MonadStackIO ma, MonadStackIO mb)
    => Dict (MonadStackIO (CombineMonadIO ma mb))
isCombineMonadIO =
    case hasTransConstraint @MonadIO @(MonadStackTrans ma) @mb of
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
combineUnliftIOs unlifta unliftb cmr = unlifta $ fromMonadStack $ remonad unliftb cmr
