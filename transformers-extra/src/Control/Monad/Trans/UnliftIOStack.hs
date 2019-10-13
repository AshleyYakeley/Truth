module Control.Monad.Trans.UnliftIOStack where

import Control.Monad.IO.Class
import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Function
import Control.Monad.Trans.IOStack
import Control.Monad.Trans.Unlift
import Data.Constraint
import Data.Kind
import Prelude hiding ((.), id)

class (IOStack MonadTransUntrans m, MonadUnliftIO m) => MonadUnliftIOStack m

instance (IOStack MonadTransUntrans m, MonadUnliftIO m) => MonadUnliftIOStack m

instance MonadTransUntrans t => MonadTransConstraint MonadUnliftIOStack t where
    hasTransConstraint ::
           forall m. MonadUnliftIOStack m
        => Dict (MonadUnliftIOStack (t m))
    hasTransConstraint =
        case hasTransConstraint @(IOStack MonadTransUntrans) @t @m of
            Dict ->
                case hasTransConstraint @MonadUnliftIO @t @m of
                    Dict -> Dict

isCombineMonadUnliftIOStack ::
       forall ma mb. (MonadUnliftIOStack ma, MonadUnliftIOStack mb)
    => Dict (MonadUnliftIOStack (CombineIOStack ma mb))
isCombineMonadUnliftIOStack = isCombineMonadUnliftIOStack' $ ioStackWitness @MonadTransUntrans @ma
  where
    isCombineMonadUnliftIOStack' ::
           forall m. IOStackWitness MonadTransUntrans m -> Dict (MonadUnliftIOStack (CombineIOStack m mb))
    isCombineMonadUnliftIOStack' IOMonadStackWitness = Dict
    isCombineMonadUnliftIOStack' (TransMonadStackWitness (w :: IOStackWitness MonadTransUntrans m')) =
        case isCombineMonadUnliftIOStack' w of
            d -> transConstraintDict @MonadUnliftIOStack d

combineUnliftFstMFunction ::
       forall (ma :: Type -> Type) (mb :: Type -> Type). (MonadUnliftIOStack ma, MonadIO mb)
    => MFunction ma (CombineIOStack ma mb)
combineUnliftFstMFunction = combineFstMFunction @MonadTransUntrans @ma @mb

combineUnliftFstMBackFunction ::
       forall ma mb. (MonadUnliftIOStack ma, MonadUnliftIO mb)
    => MBackFunction ma (CombineIOStack ma mb)
combineUnliftFstMBackFunction = combineFstMBackFunction @MonadTransUntrans @ma @mb

combineUnliftSndMFunction ::
       forall ma mb. (MonadUnliftIOStack ma, Monad mb)
    => MFunction mb (CombineIOStack ma mb)
combineUnliftSndMFunction = combineSndMFunction @MonadTransUntrans @ma @mb

combineUnliftSndMBackFunction ::
       forall ma mb. (MonadUnliftIOStack ma, MonadUnliftIO mb)
    => MBackFunction mb (CombineIOStack ma mb)
combineUnliftSndMBackFunction = combineSndMBackFunction @MonadTransUntrans @ma @mb

combineUnliftWIOFunctions ::
       forall ma mb. (MonadUnliftIOStack ma, Monad mb)
    => WIOFunction ma
    -> WIOFunction mb
    -> WIOFunction (CombineIOStack ma mb)
combineUnliftWIOFunctions = combineWIOFunctions @MonadTransUntrans @ma @mb

combineUnliftIOFunctions ::
       forall ma mb. (MonadUnliftIOStack ma, Monad mb)
    => IOFunction ma
    -> IOFunction mb
    -> IOFunction (CombineIOStack ma mb)
combineUnliftIOFunctions = combineIOFunctions @MonadTransUntrans @ma @mb
