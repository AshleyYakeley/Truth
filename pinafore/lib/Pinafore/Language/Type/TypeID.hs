module Pinafore.Language.Type.TypeID
    ( TypeID
    , zeroTypeID
    , succTypeID
    , TypeIDType
    , IdentifiedValue(..)
    , IdentifiedType(..)
    ) where

import Shapes
import Shapes.Numeric

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

newtype IdentifiedValue (tid :: BigNat) (t :: Type) = MkIdentifiedValue
    { unIdentifiedValue :: t
    } deriving (Eq)

data IdentifiedType :: (Type -> forall k. k -> Type) -> Type -> forall k. k -> Type where
    MkIdentifiedType
        :: forall (w :: Type -> forall k. k -> Type) (baseupdate :: Type) (tid :: BigNat) (t :: Type).
           TypeIDType tid
        -> w baseupdate t
        -> IdentifiedType w baseupdate (IdentifiedValue tid t)

instance forall (w :: Type -> forall k. k -> Type) (baseupdate :: Type). TestHetEquality (w baseupdate) =>
             TestHetEquality (IdentifiedType w baseupdate) where
    testHetEquality (MkIdentifiedType ia wa) (MkIdentifiedType ib wb) = do
        Refl <- testEquality ia ib
        HRefl <- testHetEquality wa wb
        return HRefl
