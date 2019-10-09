module Control.Monad.Trans.StackIO where

import Control.Category
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
import Prelude hiding ((.), id)

class (MonadTransUntrans (CombineMonadIO m), MonadUnliftIO m) => MonadStackIO m where
    type CombineMonadIO m :: (Type -> Type) -> (Type -> Type)
    toMonadStack :: MFunction m (CombineMonadIO m IO)
    fromMonadStack :: MFunction (CombineMonadIO m IO) m

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

combineFstMBackFunction ::
       forall ma mb. (MonadStackIO ma, MonadUnliftIO mb)
    => MBackFunction ma (CombineMonadIO ma mb)
combineFstMBackFunction mar = liftMBackFunction liftIOWithUnlift $ \tr -> toMonadStack $ mar $ fromMonadStack . tr

combineSndMFunction ::
       forall ma mb. (MonadStackIO ma, Monad mb)
    => MFunction mb (CombineMonadIO ma mb)
combineSndMFunction = lift

combineSndMBackFunction ::
       forall ma mb. (MonadStackIO ma, MonadUnliftIO mb)
    => MBackFunction mb (CombineMonadIO ma mb)
combineSndMBackFunction = liftWithUnlift

combineIOFunctions :: MonadStackIO ma => IOFunction ma -> IOFunction mb -> IOFunction (CombineMonadIO ma mb)
combineIOFunctions unlifta unliftb cmr = unlifta $ fromMonadStack $ remonad' unliftb cmr

combineWIOFunctions :: MonadStackIO ma => WIOFunction ma -> WIOFunction mb -> WIOFunction (CombineMonadIO ma mb)
combineWIOFunctions (MkWMFunction unlifta) (MkWMFunction unliftb) = MkWMFunction $ combineIOFunctions unlifta unliftb
