module Pinafore.Language.Type.Identified
    ( TypeID
    , zeroTypeID
    , succTypeID
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
    deriving (Eq)

zeroTypeID :: TypeID
zeroTypeID = MkTypeID 0

succTypeID :: TypeID -> TypeID
succTypeID (MkTypeID n) = MkTypeID $ succ n

newtype TypeIDType (bn :: BigNat) =
    MkTypeIDType (BigNatType bn)
    deriving (TestEquality)

instance WitnessValue TypeIDType where
    type WitnessValueType TypeIDType = TypeID
    witnessToValue (MkTypeIDType bnt) = MkTypeID $ witnessToValue bnt
    valueToWitness (MkTypeID n) cont = valueToWitness n $ \bnt -> cont $ MkTypeIDType bnt

type IdentifiedKind :: BigNat -> Type
type family IdentifiedKind tid = k | k -> tid

type Identified :: forall (tid :: BigNat) -> IdentifiedKind tid
type family Identified tid = v | v -> tid

unsafeIdentifyKind ::
       forall (tid :: BigNat) (k :: Type) m. Applicative m
    => TypeIDType tid
    -> m (IdentifiedKind tid :~: k)
unsafeIdentifyKind (MkTypeIDType _) = unsafeGetRefl

unsafeIdentify ::
       forall (tid :: BigNat) (t :: IdentifiedKind tid) m. Applicative m
    => TypeIDType tid
    -> m (Identified tid :~: t)
unsafeIdentify (MkTypeIDType _) = unsafeGetRefl
