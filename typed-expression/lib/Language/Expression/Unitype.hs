module Language.Expression.Unitype where

import Data.Shim
import Language.Expression.Common
import Shapes

newtype UnitypeNamespaceT (val :: Type) m a =
    MkUnitypeNamespaceT (m a)
    deriving (Functor, Applicative, Monad)

instance MonadTrans (UnitypeNamespaceT val) where
    lift = MkUnitypeNamespaceT

instance MonadTransConstraint Monad (UnitypeNamespaceT val) where
    hasTransConstraint = Dict

newtype UnitypeRenamerT (val :: Type) m a =
    MkUnitypeRenamerT (m a)
    deriving (Functor, Applicative, Monad)

instance MonadTrans (UnitypeRenamerT val) where
    lift = MkUnitypeRenamerT

instance MonadTransConstraint Monad (UnitypeRenamerT val) where
    hasTransConstraint = Dict

unitypeShimWit ::
       forall polarity (val :: Type). Is PolarityType polarity
    => ShimWit (->) ((:~:) val) polarity val
unitypeShimWit = mkShimWit Refl

instance Renamer (UnitypeRenamerT val) where
    type RenamerNegWitness (UnitypeRenamerT val) = ((:~:) val)
    type RenamerPosWitness (UnitypeRenamerT val) = ((:~:) val)
    type RenamerShim (UnitypeRenamerT val) = (->)
    renameNegWitness Refl = return unitypeShimWit
    renamePosWitness Refl = return unitypeShimWit
    type RenamerNamespaceT (UnitypeRenamerT val) = UnitypeNamespaceT val
    renameNewVar = return $ MkNewVar unitypeShimWit unitypeShimWit
    namespace (MkUnitypeNamespaceT ia) = ia
    runRenamer (MkUnitypeRenamerT ia) = ia

newtype UnitypeUnifier (m :: Type -> Type) (name :: Type) (val :: Type) a =
    MkUnitypeUnifier (Identity a)
    deriving (Functor, Applicative)

instance (Monad m, Eq name) => Unifier (UnitypeUnifier m name val) where
    type UnifierName (UnitypeUnifier m name val) = name
    type UnifierMonad (UnitypeUnifier m name val) = m
    type UnifierNegWitness (UnitypeUnifier m name val) = ((:~:) val)
    type UnifierPosWitness (UnitypeUnifier m name val) = ((:~:) val)
    type UnifierShim (UnitypeUnifier m name val) = (->)
    type UnifierSubstitutions (UnitypeUnifier m name val) = ()
    unifyNegWitnesses Refl Refl = return $ uuLiftNegShimWit $ MkShimWit Refl $ polarF id id
    unifyPosWitnesses Refl Refl = return $ uuLiftPosShimWit $ MkShimWit Refl $ polarF id id
    unifyPosNegWitnesses Refl Refl = return id
    solveUnifier (MkUnitypeUnifier ia) = pure $ (runIdentity ia, ())
    unifierPosSubstitute () Refl = return unitypeShimWit
    unifierNegSubstitute () Refl = return unitypeShimWit
    simplify = return

newtype UnitypeSubsumer (m :: Type -> Type) (val :: Type) a =
    MkUnitypeSubsumer (Identity a)
    deriving (Functor, Applicative)

instance Monad m => Subsumer (UnitypeSubsumer m val) where
    type SubsumerMonad (UnitypeSubsumer m val) = m
    type SubsumerNegWitness (UnitypeSubsumer m val) = ((:~:) val)
    type SubsumerPosWitness (UnitypeSubsumer m val) = ((:~:) val)
    type SubsumerShim (UnitypeSubsumer m val) = (->)
    type SubsumerSubstitutions (UnitypeSubsumer m val) = ()
    solveSubsumer (MkUnitypeSubsumer ia) = pure $ (runIdentity ia, ())
    subsumerNegSubstitute () Refl = return $ unitypeShimWit
    subsumePosWitnesses Refl Refl = return $ pure id
    simplifyPosType t = return $ mkShimWit t

class UnitypeValue val where
    applyValue :: val -> val -> val
    abstractValue :: (val -> val) -> val

data Unitype (m :: Type -> Type) (name :: Type) (val :: Type)

instance (Monad m, Eq name, UnitypeValue val) => TypeSystem (Unitype m name val) where
    type TSRenamer (Unitype m name val) = UnitypeRenamerT val
    type TSUnifier (Unitype m name val) = UnitypeUnifier (UnitypeRenamerT val m) name val
    type TSSubsumer (Unitype m name val) = UnitypeSubsumer (UnitypeRenamerT val m) val
    type TSScoped (Unitype m name val) = m
    tsFunctionPosWitness Refl Refl = mkPosShimWit Refl abstractValue
    tsFunctionNegWitness Refl Refl = mkNegShimWit Refl applyValue
