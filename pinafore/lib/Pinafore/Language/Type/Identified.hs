module Pinafore.Language.Type.Identified
    ( TypeID
    , zeroTypeID
    , succTypeID
    , TypeIDType
    , IdentifiedValue(..)
    , IdentifiedType(..)
    , unsafeGetIdentification
    ) where

import GHC.Exts (Any)
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

newtype IdentifiedValue (tid :: BigNat) =
    MkIdentifiedValue Any

data IdentifiedType :: Type -> forall k. k -> Type where
    MkIdentifiedType
        :: forall (baseupdate :: Type) (tid :: BigNat).
           TypeIDType tid
        -> IdentifiedType baseupdate (IdentifiedValue tid)

instance TestHetEquality (IdentifiedType baseupdate) where
    testHetEquality (MkIdentifiedType ia) (MkIdentifiedType ib) = do
        Refl <- testEquality ia ib
        return HRefl

unsafeGetIdentification ::
       forall (tid :: BigNat) (t :: Type) m. Applicative m
    => m (IdentifiedValue tid :~: t)
unsafeGetIdentification = unsafeGetRefl
