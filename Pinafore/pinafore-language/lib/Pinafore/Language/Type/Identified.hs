module Pinafore.Language.Type.Identified
    ( TypeID
    , TypeIDType
    , IdentifiedKind
    , Identified
    , unsafeIdentifyKind
    , unsafeIdentify
    ) where

import Shapes
import Shapes.Numeric
import Shapes.Unsafe (unsafeGetRefl)

newtype TypeID =
    MkTypeID Natural
    deriving newtype (Eq, Sequential)

newtype TypeIDType (bn :: Nat) =
    MkTypeIDType (NaturalType bn)
    deriving (TestEquality, TestOrder)

instance WitnessValue TypeIDType where
    type WitnessValueType TypeIDType = TypeID
    witnessToValue (MkTypeIDType bnt) = MkTypeID $ witnessToValue bnt
    valueToWitness (MkTypeID n) cont = valueToWitness n $ \bnt -> cont $ MkTypeIDType bnt

type IdentifiedKind :: Nat -> Type
type family IdentifiedKind tid

type Identified :: forall (tid :: Nat) -> IdentifiedKind tid
type family Identified tid

unsafeIdentifyKind ::
       forall (tid :: Nat) (k :: Type) m. Applicative m
    => TypeIDType tid
    -> m (IdentifiedKind tid :~: k)
unsafeIdentifyKind (MkTypeIDType _) = unsafeGetRefl

unsafeIdentify ::
       forall (tid :: Nat) (t :: IdentifiedKind tid) m. Applicative m
    => TypeIDType tid
    -> m (Identified tid :~: t)
unsafeIdentify (MkTypeIDType _) = unsafeGetRefl
