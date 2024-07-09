module Language.Expression.Unitype where

import Data.Shim
import Language.Expression.Common
import Shapes

type Unitype :: (Type -> Type) -> Type -> Type -> Type
data Unitype m name val

type UnitypeVar m name val polarity = NameWitness name (UniShimWit val polarity)

instance (Monad m, Eq name, Show name) => TypeSystem (Unitype m name val) where
    type TSOuter (Unitype m name val) = IdentityT m
    type TSNegWitness (Unitype m name val) = ((:~:) val)
    type TSPosWitness (Unitype m name val) = ((:~:) val)
    type TSShim (Unitype m name val) = (->)
    type TSVarID (Unitype m name val) = name

type UniShimWit val polarity = PolarShimWit (->) ((:~:) val) polarity

unitypeShimWit ::
       forall polarity (val :: Type). Is PolarityType polarity
    => UniShimWit val polarity val
unitypeShimWit = mkPolarShimWit Refl

instance (Monad m, Eq name, Show name) => RenameTypeSystem (Unitype m name val) where
    type RenamerT (Unitype m name val) = IdentityT
    namespaceRenameSource = MkRenameSource mempty mempty
    type RenamerNamespaceT (Unitype m name val) = IdentityT
    renameNewFreeVar = return $ MkNewVar unitypeShimWit unitypeShimWit
    namespace _ _ = runIdentityT
    runRenamer _ _ = runIdentityT
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
    simplify = mempty

instance (Monad m, Eq name, Show name) => SubsumeTypeSystem (Unitype m name val) where
    type Subsumer (Unitype m name val) = Identity
    type SubsumerSubstitutions (Unitype m name val) = ()
    usubSubsumer () ia = lift $ return $ pure $ runIdentity ia
    solveSubsumer ia = pure $ (pure $ runIdentity ia, ())
    subsumerPosSubstitute () Refl = return unitypeShimWit
    subsumerNegSubstitute () Refl = return unitypeShimWit
    subsumePosWitnesses Refl Refl = return $ pure id

instance (Monad m, Eq name, Show name) => ShowSubsumeTypeSystem (Unitype m name val) where
    showSubsumer _ = ""

instance (Monad m, MonadThrow PatternError m, Ord name, Show name) => AbstractTypeSystem (Unitype m name val) where
    type TSInner (Unitype m name val) = m
    bottomShimWit = MkSome unitypeShimWit

class UnitypeValue val where
    applyValue :: val -> val -> val
    abstractValue :: (val -> val) -> val

instance ( Monad m
         , MonadThrow PatternError m
         , MonadThrow (ExpressionError (UnitypeVar m name val 'Negative)) m
         , Ord name
         , Show name
         , UnitypeValue val
         ) => CompleteTypeSystem (Unitype m name val) where
    tsFunctionPosWitness Refl Refl = mkPosShimWit Refl abstractValue
    tsFunctionNegWitness Refl Refl = mkNegShimWit Refl applyValue
