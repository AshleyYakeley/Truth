module Language.Expression.Type.UniType where

import Language.Expression.Expression
import Language.Expression.Typed
import Shapes

data UniType :: k -> k -> Type where
    MkUniType :: forall (k :: Type) (t :: k). UniType t t

class FunctionKind k => ApplicableValue (t :: k) where
    applyValue :: KindMorphism k t (KindFunction k t t)

instance ApplicableValue t => FunctionWitness (UniType (t :: k)) where
    applyWitness MkUniType MkUniType cont = return $ cont MkUniType applyValue id

instance Category (KindMorphism k) => UnifyWitness (UniType (t :: k)) where
    freeWitness cont = cont MkUniType
    unifyWitnesses MkUniType MkUniType cont = cont MkUniType id id

data UniTypeSystem (t :: k)

type instance TypeSystemKind (UniTypeSystem (t :: k)) = k

-- to break mutual recursion
$(return [])

instance ApplicableValue t => TypeSystem (UniTypeSystem (t :: k)) where
    type PositiveWitness (UniTypeSystem t) = UniType t
    type NegativeWitness (UniTypeSystem t) = UniType t
