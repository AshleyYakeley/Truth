module Control.Monad.Trans.StackIO where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Function
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Tunnel
import Control.Monad.Trans.Unlift
import Data.Constraint
import Data.Kind
import Prelude

class (MonadTransUntrans (CombineMonadIO m), MonadUnliftIO m) => MonadStackIO m where
    type CombineMonadIO m :: (Type -> Type) -> (Type -> Type)
    toMonadStack :: forall a. m a -> CombineMonadIO m IO a
    fromMonadStack :: forall a. CombineMonadIO m IO a -> m a

instance MonadStackIO IO where
    type CombineMonadIO IO = IdentityT
    toMonadStack = IdentityT
    fromMonadStack = runIdentityT

instance (MonadTransUntrans t, MonadStackIO m, MonadUnliftIO (t m)) => MonadStackIO (t m) where
    type CombineMonadIO (t m) = ComposeT t (CombineMonadIO m)
    toMonadStack tma = MkComposeT $ remonad' toMonadStack tma
    fromMonadStack (MkComposeT tt) = remonad' fromMonadStack tt

instance MonadTransUntrans t => MonadTransConstraint MonadStackIO t where
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
    case hasTransConstraint @MonadUnliftIO @(CombineMonadIO ma) @mb of
        Dict -> Dict

combineFstMFunction ::
       forall ma mb. (MonadStackIO ma, MonadIO mb)
    => MFunction ma (CombineMonadIO ma mb)
combineFstMFunction mar = remonad liftIO $ toMonadStack mar

combineSndMFunction ::
       forall ma mb. (MonadStackIO ma, Monad mb)
    => MFunction mb (CombineMonadIO ma mb)
combineSndMFunction = lift

combineUnliftIOs :: MonadStackIO ma => WIOFunction ma -> WIOFunction mb -> WIOFunction (CombineMonadIO ma mb)
combineUnliftIOs (MkWMFunction unlifta) (MkWMFunction unliftb) =
    MkWMFunction $ \cmr -> unlifta $ fromMonadStack $ remonad' unliftb cmr
