module Control.Monad.Trans.Constraint where

import Control.Monad
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Constraint
import Data.Kind
import Data.Monoid

class MonadTrans t => MonadTransConstraint (c :: (Type -> Type) -> Constraint) t where
    hasTransConstraint ::
           forall (m :: Type -> Type). c m
        => Dict (c (t m))

transConstraintDict ::
       forall c t m. MonadTransConstraint c t
    => Dict (c m)
    -> Dict (c (t m))
transConstraintDict Dict = hasTransConstraint @c @t @m

withTransConstraintTM ::
       forall c t m a. (MonadTransConstraint c t, c m)
    => (c (t m) => t m a)
    -> t m a
withTransConstraintTM tma =
    case hasTransConstraint @c @t @m of
        Dict -> tma

withTransConstraintTM' ::
       forall c t' t m a. (MonadTransConstraint c t, c m)
    => (c (t m) => t' (t m) a)
    -> t' (t m) a
withTransConstraintTM' tma =
    case hasTransConstraint @c @t @m of
        Dict -> tma

withTransConstraintDict ::
       forall c t m c'. (MonadTransConstraint c t, c m)
    => (c (t m) => Dict (c' (t m)))
    -> Dict (c' (t m))
withTransConstraintDict dict =
    case hasTransConstraint @c @t @m of
        Dict -> dict

instance MonadTransConstraint Monad IdentityT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO IdentityT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFail IdentityT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFix IdentityT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadPlus IdentityT where
    hasTransConstraint = Dict

instance MonadTransConstraint Monad (ReaderT s) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO (ReaderT s) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFail (ReaderT s) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFix (ReaderT s) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadPlus (ReaderT s) where
    hasTransConstraint = Dict

instance Monoid s => MonadTransConstraint Monad (WriterT s) where
    hasTransConstraint = Dict

instance Monoid s => MonadTransConstraint MonadIO (WriterT s) where
    hasTransConstraint = Dict

instance Monoid s => MonadTransConstraint MonadFail (WriterT s) where
    hasTransConstraint = Dict

instance Monoid s => MonadTransConstraint MonadFix (WriterT s) where
    hasTransConstraint = Dict

instance Monoid s => MonadTransConstraint MonadPlus (WriterT s) where
    hasTransConstraint = Dict

instance MonadTransConstraint Monad (StateT s) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO (StateT s) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFail (StateT s) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFix (StateT s) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadPlus (StateT s) where
    hasTransConstraint = Dict

instance MonadTransConstraint Monad MaybeT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO MaybeT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFail MaybeT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFix MaybeT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadPlus MaybeT where
    hasTransConstraint = Dict

instance MonadTransConstraint Monad (ExceptT e) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO (ExceptT e) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFail (ExceptT e) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFix (ExceptT e) where
    hasTransConstraint = Dict

instance Monoid e => MonadTransConstraint MonadPlus (ExceptT e) where
    hasTransConstraint = Dict

instance MonadTransConstraint Monad (ContT s) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO (ContT s) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFail (ContT s) where
    hasTransConstraint = Dict
