module Language.Expression.Unitype where

import Data.Shim
import Language.Expression.Common
import Shapes

type Unitype :: (Type -> Type) -> Type -> Type -> Type
data Unitype m name val

instance (Monad m, Eq name, Show name) => TypeSystem (Unitype m name val) where
    type TSOuter (Unitype m name val) = IdentityT m
    type TSNegWitness (Unitype m name val) = ((:~:) val)
    type TSPosWitness (Unitype m name val) = ((:~:) val)
    type TSShim (Unitype m name val) = (->)
    type TSVarID (Unitype m name val) = name

unitypeShimWit ::
       forall polarity (val :: Type). Is PolarityType polarity
    => PolarShimWit (->) ((:~:) val) polarity val
unitypeShimWit = mkPolarShimWit Refl

instance (Monad m, Eq name, Show name) => RenameTypeSystem (Unitype m name val) where
    type RenamerT (Unitype m name val) = IdentityT
    renameNegWitness Refl = return Refl
    renamePosWitness Refl = return Refl
    type RenamerNamespaceT (Unitype m name val) = IdentityT
    renameNewFreeVar = return $ MkNewVar unitypeShimWit unitypeShimWit
    namespace _ = runIdentityT
    runRenamer = runIdentityT
    finalRenamer = id

instance (Monad m, Eq name, Show name) => UnifyTypeSystem (Unitype m name val) where
    type Unifier (Unitype m name val) = Identity
    type UnifierSubstitutions (Unitype m name val) = ()
    unifyNegWitnesses Refl Refl = return $ uuLiftNegShimWit @(Unitype m name val) $ MkShimWit Refl $ polarF id id
    unifyPosWitnesses Refl Refl = return $ uuLiftPosShimWit @(Unitype m name val) $ MkShimWit Refl $ polarF id id
    unifyPosNegWitnesses Refl Refl = return id
    solveUnifier ia = pure $ (pure $ runIdentity ia, ())
    unifierPosSubstitute () Refl = return unitypeShimWit
    unifierNegSubstitute () Refl = return unitypeShimWit

instance (Monad m, Eq name, Show name) => SimplifyTypeSystem (Unitype m name val) where
    simplify = return

instance (Monad m, Eq name, Show name) => SubsumeTypeSystem (Unitype m name val) where
    type Subsumer (Unitype m name val) = Identity
    type SubsumerSubstitutions (Unitype m name val) = ()
    showSubsumer _ = ""
    usubSubsumer () ia = lift $ return $ pure $ runIdentity ia
    solveSubsumer ia = pure $ (pure $ runIdentity ia, ())
    subsumerNegSubstitute () Refl = return $ unitypeShimWit
    subsumePosWitnesses Refl Refl = return $ pure id

instance (MonadThrow ExpressionError m, Ord name, Show name) => AbstractTypeSystem (Unitype m name val) where
    type TSInner (Unitype m name val) = m

class UnitypeValue val where
    applyValue :: val -> val -> val
    abstractValue :: (val -> val) -> val

instance (MonadThrow ExpressionError m, Ord name, Show name, UnitypeValue val) =>
             CompleteTypeSystem (Unitype m name val) where
    tsFunctionPosWitness Refl Refl = mkPosShimWit Refl abstractValue
    tsFunctionNegWitness Refl Refl = mkNegShimWit Refl applyValue
