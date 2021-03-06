module Pinafore.Language.Type.Identified
    ( TypeID
    , zeroTypeID
    , succTypeID
    , TypeIDType
    , Identified
    , IdentifiedType(..)
    , unsafeGetIdentification
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

type family Identified (tid :: BigNat) = (v :: Type) | v -> tid where

type IdentifiedType :: forall k. k -> Type
data IdentifiedType t where
    MkIdentifiedType :: forall (tid :: BigNat). TypeIDType tid -> IdentifiedType (Identified tid)

instance TestHetEquality IdentifiedType where
    testHetEquality (MkIdentifiedType ia) (MkIdentifiedType ib) = do
        Refl <- testEquality ia ib
        return HRefl

unsafeGetIdentification ::
       forall (tid :: BigNat) (t :: Type) m. Applicative m
    => m (Identified tid :~: t)
unsafeGetIdentification = unsafeGetRefl
