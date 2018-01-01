module Control.Monad.Trans.Constraint where

import Shapes.Import

class MonadTrans t =>
      MonadTransConstraint (c :: (* -> *) -> Constraint) t where
    hasTransConstraint ::
           forall (m :: * -> *). c m
        => Dict (c (t m))

withTransConstraintTM ::
       forall c t m a. (MonadTransConstraint c t, c m)
    => (c (t m) =>
            t m a)
    -> t m a
withTransConstraintTM tma =
    case hasTransConstraint @c @t @m of
        Dict -> tma

withTransConstraintTM' ::
       forall c t' t m a. (MonadTransConstraint c t, c m)
    => (c (t m) =>
            t' (t m) a)
    -> t' (t m) a
withTransConstraintTM' tma =
    case hasTransConstraint @c @t @m of
        Dict -> tma

instance MonadTransConstraint Monad IdentityT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO IdentityT where
    hasTransConstraint = Dict

instance MonadTransConstraint Monad (ReaderT s) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO (ReaderT s) where
    hasTransConstraint = Dict

instance Monoid s => MonadTransConstraint Monad (WriterT s) where
    hasTransConstraint = Dict

instance Monoid s => MonadTransConstraint MonadIO (WriterT s) where
    hasTransConstraint = Dict

instance MonadTransConstraint Monad (StateT s) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO (StateT s) where
    hasTransConstraint = Dict

instance MonadTransConstraint Monad MaybeT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO MaybeT where
    hasTransConstraint = Dict

instance MonadTransConstraint Monad (ExceptT e) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO (ExceptT e) where
    hasTransConstraint = Dict

instance MonadTransConstraint Monad ListT where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO ListT where
    hasTransConstraint = Dict

instance MonadTransConstraint Monad (ContT s) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO (ContT s) where
    hasTransConstraint = Dict
