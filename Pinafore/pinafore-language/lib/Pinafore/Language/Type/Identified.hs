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

newtype TypeIDType (bn :: TNatural) =
    MkTypeIDType (NaturalType bn)
    deriving (TestEquality)

instance WitnessValue TypeIDType where
    type WitnessValueType TypeIDType = TypeID
    witnessToValue (MkTypeIDType bnt) = MkTypeID $ witnessToValue bnt
    valueToWitness (MkTypeID n) cont = valueToWitness n $ \bnt -> cont $ MkTypeIDType bnt

type IdentifiedKind :: TNatural -> Type
type family IdentifiedKind tid = k | k -> tid

type Identified :: forall (tid :: TNatural) -> IdentifiedKind tid
type family Identified tid = v | v -> tid

unsafeIdentifyKind ::
       forall (tid :: TNatural) (k :: Type) m. Applicative m
    => TypeIDType tid
    -> m (IdentifiedKind tid :~: k)
unsafeIdentifyKind (MkTypeIDType _) = unsafeGetRefl

unsafeIdentify ::
       forall (tid :: TNatural) (t :: IdentifiedKind tid) m. Applicative m
    => TypeIDType tid
    -> m (Identified tid :~: t)
unsafeIdentify (MkTypeIDType _) = unsafeGetRefl
