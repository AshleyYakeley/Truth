module Pinafore.Language.TypeID
    ( TypeID
    , zeroTypeID
    , succTypeID
    , TypeIDType
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
