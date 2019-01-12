module Language.Expression.Unitype where

import Language.Expression.Renamer
import Language.Expression.Subsumer
import Language.Expression.TypeF
import Language.Expression.TypeSystem
import Language.Expression.Unifier
import Shapes

newtype UnitypeNamespace (val :: Type) m a =
    MkUnitypeNamespace (m a)
    deriving (Functor, Applicative, Monad)

instance MonadTrans (UnitypeNamespace val) where
    lift = MkUnitypeNamespace

instance MonadTransConstraint Monad (UnitypeNamespace val) where
    hasTransConstraint = Dict

newtype UnitypeRenamer (val :: Type) m a =
    MkUnitypeRenamer (m a)
    deriving (Functor, Applicative, Monad)

instance MonadTrans (UnitypeRenamer val) where
    lift = MkUnitypeRenamer

instance MonadTransConstraint Monad (UnitypeRenamer val) where
    hasTransConstraint = Dict

instance Renamer (UnitypeRenamer val) where
    type RenamerNegWitness (UnitypeRenamer val) = ((:~:) val)
    type RenamerPosWitness (UnitypeRenamer val) = ((:~:) val)
    renameTSNegWitness Refl = return $ mkTypeF Refl
    renameTSPosWitness Refl = return $ mkTypeF Refl
    type RenamerNamespace (UnitypeRenamer val) = UnitypeNamespace val
    renameNewVar = return $ MkNewVar Refl Refl id
    namespace (MkUnitypeNamespace ia) = ia
    runRenamer (MkUnitypeRenamer ia) = ia

newtype UnitypeUnifier (m :: Type -> Type) (name :: Type) (val :: Type) a =
    MkUnitypeUnifier (Identity a)
    deriving (Functor, Applicative)

instance (Monad m, Eq name) => Unifier (UnitypeUnifier m name val) where
    type UnifierName (UnitypeUnifier m name val) = name
    type UnifierMonad (UnitypeUnifier m name val) = m
    type UnifierNegWitness (UnitypeUnifier m name val) = ((:~:) val)
    type UnifierPosWitness (UnitypeUnifier m name val) = ((:~:) val)
    type UnifierSubstitutions (UnitypeUnifier m name val) = ()
    unifyNegWitnesses Refl Refl cont = cont Refl $ pure (id, id)
    unifyPosWitnesses Refl Refl cont = cont Refl $ pure (id, id)
    unifyPosNegWitnesses Refl Refl = return $ pure id
    solveUnifier (MkUnitypeUnifier ia) = pure $ (runIdentity ia, ())
    unifierPosSubstitute () Refl = return $ mkTypeF Refl
    unifierNegSubstitute () Refl = return $ mkTypeF Refl
    simplify = return

newtype UnitypeSubsumer (m :: Type -> Type) (val :: Type) a =
    MkUnitypeSubsumer (Identity a)
    deriving (Functor, Applicative)

instance Monad m => Subsumer (UnitypeSubsumer m val) where
    type SubsumerMonad (UnitypeSubsumer m val) = m
    type SubsumerNegWitness (UnitypeSubsumer m val) = ((:~:) val)
    type SubsumerPosWitness (UnitypeSubsumer m val) = ((:~:) val)
    type SubsumerSubstitutions (UnitypeSubsumer m val) = ()
    solveSubsumer (MkUnitypeSubsumer ia) = pure $ (runIdentity ia, ())
    subsumerNegSubstitute () Refl = return $ mkTypeF Refl
    subsumePosWitnesses Refl Refl = return $ pure id
    simplifyPosType = id

class UnitypeValue val where
    applyValue :: val -> val -> val
    abstractValue :: (val -> val) -> val

data Unitype (m :: Type -> Type) (name :: Type) (val :: Type)

instance (Monad m, Eq name, UnitypeValue val) => TypeSystem (Unitype m name val) where
    type TSRenamer (Unitype m name val) = UnitypeRenamer val
    type TSUnifier (Unitype m name val) = UnitypeUnifier (UnitypeRenamer val m) name val
    type TSSubsumer (Unitype m name val) = UnitypeSubsumer (UnitypeRenamer val m) val
    type TSScoped (Unitype m name val) = m
    tsFunctionPosWitness Refl Refl cont = cont Refl abstractValue
    tsFunctionNegWitness Refl Refl cont = cont Refl applyValue
