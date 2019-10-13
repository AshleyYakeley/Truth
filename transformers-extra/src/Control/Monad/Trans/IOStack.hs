module Control.Monad.Trans.IOStack where

import Control.Category
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Function
import Control.Monad.Trans.Tunnel
import Control.Monad.Trans.Unlift
import Data.Constraint
import Data.Kind
import Data.Type.Equality
import Prelude hiding ((.), id)

type family CombineIOStack (ma :: Type -> Type) (mb :: Type -> Type) :: Type -> Type where
    CombineIOStack IO mb = mb
    CombineIOStack (t ma) mb = t (CombineIOStack ma mb)

data IOStackWitness (c :: ((Type -> Type) -> (Type -> Type)) -> Constraint) (m :: Type -> Type) where
    IOMonadStackWitness :: IOStackWitness c IO
    TransMonadStackWitness :: c t => IOStackWitness c m -> IOStackWitness c (t m)

class IOStack c m where
    ioStackWitness :: IOStackWitness c m

isMonadStack :: IOStackWitness c m -> m :~: CombineIOStack m IO
isMonadStack IOMonadStackWitness = Refl
isMonadStack (TransMonadStackWitness w) =
    case isMonadStack w of
        Refl -> Refl

instance IOStack c IO where
    ioStackWitness = IOMonadStackWitness

instance (c t, IOStack c m) => IOStack c (t m) where
    ioStackWitness = TransMonadStackWitness ioStackWitness

instance (c t, MonadTrans t) => MonadTransConstraint (IOStack c) t where
    hasTransConstraint ::
           forall m. IOStack c m
        => Dict (IOStack c (t m))
    hasTransConstraint = Dict

isCombineIOStack ::
       forall c ma mb. (IOStack c ma, IOStack c mb)
    => Dict (IOStack c (CombineIOStack ma mb))
isCombineIOStack = isCombineIOStack' $ ioStackWitness @c @ma
  where
    isCombineIOStack' :: forall m. IOStackWitness c m -> Dict (IOStack c (CombineIOStack m mb))
    isCombineIOStack' IOMonadStackWitness = Dict
    isCombineIOStack' (TransMonadStackWitness w) =
        case isCombineIOStack' w of
            Dict -> Dict

combineFstMFunction ::
       forall (c :: ((Type -> Type) -> (Type -> Type)) -> Constraint) (ma :: Type -> Type) (mb :: Type -> Type).
       (forall (t :: (Type -> Type) -> (Type -> Type)). c t => MonadTransSemiTunnel t, IOStack c ma, MonadIO mb)
    => MFunction ma (CombineIOStack ma mb)
combineFstMFunction = let
    build ::
           forall m.
           IOStackWitness c m
        -> (WMFunction m (CombineIOStack m mb), Dict (Monad m), Dict (Monad (CombineIOStack m mb)))
    build IOMonadStackWitness = (MkWMFunction liftIO, Dict, Dict)
    build (TransMonadStackWitness w) =
        case build w of
            (wmf, dm@Dict, dcm@Dict) ->
                (liftWMFunction wmf, transConstraintDict @Monad dm, transConstraintDict @Monad dcm)
    in case build $ ioStackWitness @c @ma of
           (wmf, _, _) -> runWMFunction wmf

combineFstMBackFunction ::
       forall c ma mb. (forall t. c t => MonadTransSemiTunnel t, IOStack c ma, MonadUnliftIO mb)
    => MBackFunction ma (CombineIOStack ma mb)
combineFstMBackFunction = let
    build ::
           forall m.
           IOStackWitness c m
        -> (WMBackFunction m (CombineIOStack m mb), Dict (Monad m), Dict (Monad (CombineIOStack m mb)))
    build IOMonadStackWitness = (MkWMBackFunction liftIOWithUnlift, Dict, Dict)
    build (TransMonadStackWitness w) =
        case build w of
            (wmf, dm@Dict, dcm@Dict) ->
                (liftWMBackFunction wmf, transConstraintDict @Monad dm, transConstraintDict @Monad dcm)
    in case build $ ioStackWitness @c @ma of
           (wmf, _, _) -> runWMBackFunction wmf

combineSndMFunction ::
       forall c ma mb. (forall t. c t => MonadTransConstraint Monad t, IOStack c ma, Monad mb)
    => MFunction mb (CombineIOStack ma mb)
combineSndMFunction = let
    build :: forall m. IOStackWitness c m -> (WMFunction mb (CombineIOStack m mb), Dict (Monad (CombineIOStack m mb)))
    build IOMonadStackWitness = (id, Dict)
    build (TransMonadStackWitness w) =
        case build w of
            (wmf, dcm@Dict) -> (wLift . wmf, transConstraintDict @Monad dcm)
    in case build $ ioStackWitness @c @ma of
           (wmf, _) -> runWMFunction wmf

combineSndMBackFunction ::
       forall c ma mb. (forall t. c t => MonadTransUnlift t, IOStack c ma, MonadUnliftIO mb)
    => MBackFunction mb (CombineIOStack ma mb)
combineSndMBackFunction = let
    build ::
           forall m.
           IOStackWitness c m
        -> (WMBackFunction mb (CombineIOStack m mb), Dict (MonadUnliftIO (CombineIOStack m mb)))
    build IOMonadStackWitness = (id, Dict)
    build (TransMonadStackWitness w) =
        case build w of
            (wmf, dcm@Dict) -> (liftWithUnliftW . wmf, transConstraintDict @MonadUnliftIO dcm)
    in case build $ ioStackWitness @c @ma of
           (wmf, _) -> runWMBackFunction wmf

combineWIOFunctions ::
       forall c ma mb. (forall t. c t => MonadTransSemiTunnel t, IOStack c ma, Monad mb)
    => WIOFunction ma
    -> WIOFunction mb
    -> WIOFunction (CombineIOStack ma mb)
combineWIOFunctions fa fb = let
    build ::
           forall m.
           IOStackWitness c m
        -> (WMFunction (CombineIOStack m mb) m, Dict (Monad m), Dict (Monad (CombineIOStack m mb)))
    build IOMonadStackWitness = (fb, Dict, Dict)
    build (TransMonadStackWitness w) =
        case build w of
            (wmf, dm@Dict, dcm@Dict) ->
                (liftWMFunction wmf, transConstraintDict @Monad dm, transConstraintDict @Monad dcm)
    in case build (ioStackWitness @c @ma) of
           (fb', _, _) -> fa . fb'

combineIOFunctions ::
       forall c ma mb. (forall t. c t => MonadTransSemiTunnel t, IOStack c ma, Monad mb)
    => IOFunction ma
    -> IOFunction mb
    -> IOFunction (CombineIOStack ma mb)
combineIOFunctions fa fb = runWMFunction $ combineWIOFunctions @c (MkWMFunction fa) (MkWMFunction fb)
