module Language.Expression.Unitype where

import Language.Expression.Renamer
import Language.Expression.Typed
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
    renameNegWitness Refl cont = cont Refl id
    renamePosWitness Refl cont = cont Refl id
    type RenamerNamespace (UnitypeRenamer val) = UnitypeNamespace val
    renameNewVar cont = cont Refl Refl id
    namespace (MkUnitypeNamespace ia) = ia
    runRenamer (MkUnitypeRenamer ia) = ia

newtype UnitypeUnifier (m :: Type -> Type) (val :: Type) a =
    MkUnitypeUnifier (Identity a)
    deriving (Functor, Applicative)

instance Monad m => Unifier (UnitypeUnifier m val) where
    type UnifierMonad (UnitypeUnifier m val) = UnitypeRenamer val m
    type UnifierNegWitness (UnitypeUnifier m val) = ((:~:) val)
    type UnifierPosWitness (UnitypeUnifier m val) = ((:~:) val)
    type UnifierSubstitutions (UnitypeUnifier m val) = ()
    unifyNegWitnesses Refl Refl cont = cont Refl $ pure (id, id)
    unifyPosWitnesses Refl Refl cont = cont Refl $ pure (id, id)
    unifyPosNegWitnesses Refl Refl = return $ pure id
    solveUnifier (MkUnitypeUnifier ia) = pure $ (runIdentity ia, ())
    unifierPosSubstitute () Refl cont = cont Refl id
    unifierNegSubstitute () Refl cont = cont Refl id
    simplifyExpressionType expr = return expr

class UnitypeValue val where
    applyValue :: val -> val -> val
    abstractValue :: (val -> val) -> val

data Unitype (m :: Type -> Type) (val :: Type)

instance (Monad m, UnitypeValue val) => TypeSystem (Unitype m val) where
    type TypeRenamer (Unitype m val) = UnitypeRenamer val
    type TypeUnifier (Unitype m val) = UnitypeUnifier m val
    type NegWitness (Unitype m val) = ((:~:) val)
    type PosWitness (Unitype m val) = ((:~:) val)
    type TypeCheck (Unitype m val) = m
    typeSystemFunctionPosWitness Refl Refl cont = cont Refl abstractValue
    typeSystemFunctionNegWitness Refl Refl cont = cont Refl applyValue
