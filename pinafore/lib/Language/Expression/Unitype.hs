module Language.Expression.Unitype where

import Language.Expression.Renamer
import Language.Expression.Typed
import Language.Expression.Unifier
import Shapes

data Unitype (m :: Type -> Type) (val :: Type)

newtype UnitypeRenamer (val :: Type) a =
    MkUnitypeRenamer (Identity a)
    deriving (Functor, Applicative, Monad)

instance Namespace (UnitypeRenamer val) where
    type NamespaceNegWitness (UnitypeRenamer val) = ((:~:) val)
    type NamespacePosWitness (UnitypeRenamer val) = ((:~:) val)
    renameNegWitness Refl cont = cont Refl id
    renamePosWitness Refl cont = cont Refl id

instance Renamer (UnitypeRenamer val) where
    type RenamerNamespace (UnitypeRenamer val) = UnitypeRenamer val
    renameNewVar cont = cont Refl Refl
    namespace = id
    runRenamer (MkUnitypeRenamer ia) = runIdentity ia

newtype UnitypeUnifier (m :: Type -> Type) (val :: Type) a =
    MkUnitypeUnifier (Identity a)
    deriving (Functor, Applicative)

instance Monad m => Unifier (UnitypeUnifier m val) where
    type UnifierMonad (UnitypeUnifier m val) = m
    type UnifierNegWitness (UnitypeUnifier m val) = ((:~:) val)
    type UnifierPosWitness (UnitypeUnifier m val) = ((:~:) val)
    type UnifierSubstitutions (UnitypeUnifier m val) = ()
    unifyNegWitnesses Refl Refl cont = cont Refl $ pure (id, id)
    unifyPosWitnesses Refl Refl cont = cont Refl $ pure (id, id)
    unifyPosNegWitnesses Refl Refl = return $ pure id
    solveUnifier (MkUnitypeUnifier ia) = return $ (runIdentity ia, ())
    unifierPosSubstitute () Refl cont = cont Refl id
    unifierNegSubstitute () Refl cont = cont Refl id

class UnitypeValue val where
    applyValue :: val -> val -> val
    abstractValue :: (val -> val) -> val

instance (Monad m, UnitypeValue val) => TypeSystem (Unitype m val) where
    type TypeRenamer (Unitype m val) = UnitypeRenamer val
    type TypeUnifier (Unitype m val) = UnitypeUnifier m val
    type NegWitness (Unitype m val) = ((:~:) val)
    type PosWitness (Unitype m val) = ((:~:) val)
    type TSMonad (Unitype m val) = m
    typeSystemFunctionPosWitness Refl Refl cont = cont Refl abstractValue
    typeSystemFunctionNegWitness Refl Refl cont = cont Refl applyValue
