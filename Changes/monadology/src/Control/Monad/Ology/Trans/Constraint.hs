module Control.Monad.Ology.Trans.Constraint where

import Control.Monad.Ology.Inner
import Control.Monad.Ology.Outer
import Import

type TransKind = (Type -> Type) -> (Type -> Type)

type TransConstraint :: ((Type -> Type) -> Constraint) -> TransKind -> Constraint
class TransConstraint c t where
    hasTransConstraint ::
           forall (m :: Type -> Type). c m
        => Dict (c (t m))

transConstraintDict ::
       forall c t m. TransConstraint c t
    => Dict (c m)
    -> Dict (c (t m))
transConstraintDict Dict = hasTransConstraint @c @t @m

withTransConstraintTM ::
       forall c t m a. (TransConstraint c t, c m)
    => (c (t m) => t m a)
    -> t m a
withTransConstraintTM tma =
    case hasTransConstraint @c @t @m of
        Dict -> tma

withTransConstraintTM' ::
       forall c t' t m a. (TransConstraint c t, c m)
    => (c (t m) => t' (t m) a)
    -> t' (t m) a
withTransConstraintTM' tma =
    case hasTransConstraint @c @t @m of
        Dict -> tma

withTransConstraintDict ::
       forall c t m c'. (TransConstraint c t, c m)
    => (c (t m) => Dict (c' (t m)))
    -> Dict (c' (t m))
withTransConstraintDict dict =
    case hasTransConstraint @c @t @m of
        Dict -> dict

instance TransConstraint Functor IdentityT where
    hasTransConstraint = Dict

instance TransConstraint Applicative IdentityT where
    hasTransConstraint = Dict

instance TransConstraint Monad IdentityT where
    hasTransConstraint = Dict

instance TransConstraint MonadIO IdentityT where
    hasTransConstraint = Dict

instance TransConstraint MonadFail IdentityT where
    hasTransConstraint = Dict

instance TransConstraint MonadFix IdentityT where
    hasTransConstraint = Dict

instance TransConstraint MonadPlus IdentityT where
    hasTransConstraint = Dict

instance TransConstraint MonadInner IdentityT where
    hasTransConstraint = Dict

instance TransConstraint MonadOuter IdentityT where
    hasTransConstraint = Dict

instance TransConstraint MonadExtract IdentityT where
    hasTransConstraint = Dict

instance TransConstraint Functor (ReaderT s) where
    hasTransConstraint = Dict

instance TransConstraint Applicative (ReaderT s) where
    hasTransConstraint = Dict

instance TransConstraint Monad (ReaderT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadIO (ReaderT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadFail (ReaderT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadFix (ReaderT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadPlus (ReaderT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadOuter (ReaderT s) where
    hasTransConstraint = Dict

instance Monoid s => TransConstraint Functor (WriterT s) where
    hasTransConstraint = Dict

instance Monoid s => TransConstraint Applicative (WriterT s) where
    hasTransConstraint = Dict

instance Monoid s => TransConstraint Monad (WriterT s) where
    hasTransConstraint = Dict

instance Monoid s => TransConstraint MonadIO (WriterT s) where
    hasTransConstraint = Dict

instance Monoid s => TransConstraint MonadFail (WriterT s) where
    hasTransConstraint = Dict

instance Monoid s => TransConstraint MonadFix (WriterT s) where
    hasTransConstraint = Dict

instance Monoid s => TransConstraint MonadPlus (WriterT s) where
    hasTransConstraint = Dict

instance Monoid s => TransConstraint MonadInner (WriterT s) where
    hasTransConstraint = Dict

instance Monoid s => TransConstraint MonadExtract (WriterT s) where
    hasTransConstraint = Dict

instance TransConstraint Functor (StateT s) where
    hasTransConstraint = Dict

instance TransConstraint Monad (StateT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadIO (StateT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadFail (StateT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadFix (StateT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadPlus (StateT s) where
    hasTransConstraint = Dict

instance TransConstraint Functor MaybeT where
    hasTransConstraint = Dict

instance TransConstraint Monad MaybeT where
    hasTransConstraint = Dict

instance TransConstraint MonadIO MaybeT where
    hasTransConstraint = Dict

instance TransConstraint MonadFail MaybeT where
    hasTransConstraint = Dict

instance TransConstraint MonadFix MaybeT where
    hasTransConstraint = Dict

instance TransConstraint MonadPlus MaybeT where
    hasTransConstraint = Dict

instance TransConstraint MonadInner MaybeT where
    hasTransConstraint = Dict

instance TransConstraint Functor (ExceptT e) where
    hasTransConstraint = Dict

instance TransConstraint Monad (ExceptT e) where
    hasTransConstraint = Dict

instance TransConstraint MonadIO (ExceptT e) where
    hasTransConstraint = Dict

instance TransConstraint MonadFail (ExceptT e) where
    hasTransConstraint = Dict

instance TransConstraint MonadFix (ExceptT e) where
    hasTransConstraint = Dict

instance Monoid e => TransConstraint MonadPlus (ExceptT e) where
    hasTransConstraint = Dict

instance TransConstraint MonadInner (ExceptT e) where
    hasTransConstraint = Dict

instance TransConstraint Functor (ContT s) where
    hasTransConstraint = Dict

instance TransConstraint Applicative (ContT s) where
    hasTransConstraint = Dict

instance TransConstraint Monad (ContT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadIO (ContT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadFail (ContT s) where
    hasTransConstraint = Dict
