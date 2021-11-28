module Pinafore.Language.Type.Identified
    ( TypeID
    , zeroTypeID
    , succTypeID
    , TypeIDType
    , Identified
    , IdentifiedFamily(..)
    , unsafeIdentify
    ) where

import Pinafore.Language.Type.Family
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

type Identified :: BigNat -> Type
type family Identified tid = v | v -> tid

data IdentifiedFamily :: FamilyKind where
    MkIdentifiedFamily :: forall (tid :: BigNat). TypeIDType tid -> IdentifiedFamily (Identified tid)

instance TestHetEquality IdentifiedFamily where
    testHetEquality (MkIdentifiedFamily ia) (MkIdentifiedFamily ib) = do
        Refl <- testEquality ia ib
        return HRefl

unsafeIdentify ::
       forall (tid :: BigNat) (t :: Type) m. Applicative m
    => m (Identified tid :~: t)
unsafeIdentify = unsafeGetRefl
