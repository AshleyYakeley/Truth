module Language.Expression.Unitype where

import Data.Shim
import Language.Expression.Common
import Shapes

type Unitype :: (Type -> Type) -> Type -> Type -> Type
data Unitype m name val

instance (Monad m, Eq name) => TypeSystem (Unitype m name val) where
    type TSOuter (Unitype m name val) = IdentityT m
    type TSNegWitness (Unitype m name val) = ((:~:) val)
    type TSPosWitness (Unitype m name val) = ((:~:) val)
    type TSShim (Unitype m name val) = (->)
    type TSName (Unitype m name val) = name

unitypeShimWit ::
       forall polarity (val :: Type). Is PolarityType polarity
    => ShimWit (->) ((:~:) val) polarity val
unitypeShimWit = mkShimWit Refl

instance (Monad m, Eq name) => RenameTypeSystem (Unitype m name val) where
    type RenamerT (Unitype m name val) = IdentityT
    renameNegWitness Refl = return Refl
    renamePosWitness Refl = return Refl
    type RenamerNamespaceT (Unitype m name val) = IdentityT
    renameNewVar = return $ MkNewVar unitypeShimWit unitypeShimWit
    namespace = runIdentityT
    runRenamer = runIdentityT

instance (Monad m, Eq name) => UnifyTypeSystem (Unitype m name val) where
    type Unifier (Unitype m name val) = Identity
    type UnifierSubstitutions (Unitype m name val) = ()
    unifyNegWitnesses Refl Refl = return $ uuLiftNegShimWit $ MkShimWit Refl $ polarF id id
    unifyPosWitnesses Refl Refl = return $ uuLiftPosShimWit $ MkShimWit Refl $ polarF id id
    unifyPosNegWitnesses Refl Refl = return id
    solveUnifier ia = pure $ (runIdentity ia, ())
    unifierPosSubstitute () Refl = return unitypeShimWit
    unifierNegSubstitute () Refl = return unitypeShimWit

instance (Monad m, Eq name) => SimplifyTypeSystem (Unitype m name val) where
    simplify = return

instance (Monad m, Eq name) => SubsumeTypeSystem (Unitype m name val) where
    type Subsumer (Unitype m name val) = Identity
    type SubsumerSubstitutions (Unitype m name val) = ()
    solveSubsumer ia = pure $ (runIdentity ia, ())
    subsumerNegSubstitute () Refl = return $ unitypeShimWit
    subsumePosWitnesses Refl Refl = return $ pure id

instance (Monad m, Ord name) => AbstractTypeSystem (Unitype m name val) where
    type TSInner (Unitype m name val) = m

class UnitypeValue val where
    applyValue :: val -> val -> val
    abstractValue :: (val -> val) -> val

instance (Monad m, Ord name, UnitypeValue val) => CompleteTypeSystem (Unitype m name val) where
    tsFunctionPosWitness Refl Refl = mkPosShimWit Refl abstractValue
    tsFunctionNegWitness Refl Refl = mkNegShimWit Refl applyValue
