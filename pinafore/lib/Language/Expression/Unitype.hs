module Language.Expression.Unitype where

import Language.Expression.NameWit
import Language.Expression.Sealed
import Language.Expression.Typed
import Shapes

data Unitype (m :: Type -> Type) (val :: Type)

class UnitypeValue val where
    applyValue :: val -> val -> val
    abstractValue :: (val -> val) -> val

instance (Monad m, UnitypeValue val) => TypeSystem (Unitype m val) where
    type VarWitness (Unitype m val) = ((:~:) val)
    type ValWitness (Unitype m val) = ((:~:) val)
    type TSMonad (Unitype m val) = m
    typeSystemVarJoiner = MkTypeJoiner Refl $ \Refl Refl -> return $ MkJoiner Refl id id
    typeSystemChecker = MkTypeChecker $ \Refl Refl -> return id
    typeSystemApplyWitness Refl Refl cont = cont Refl applyValue
    typeSystemAbstractWitness Refl Refl cont = cont Refl abstractValue
    typeSystemGenerateVariable cont = cont Refl Refl
