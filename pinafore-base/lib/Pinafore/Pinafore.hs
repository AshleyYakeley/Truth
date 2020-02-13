module Pinafore.Pinafore where
{-
import Data.Time
import Pinafore.Base
import Pinafore.Storage.File
import Shapes
import Truth.Core

data PinaforeSelector t where
    PinaforeSelectPoint :: PinaforeSelector PinaforeEntityUpdate
    PinaforeSelectFile :: PinaforeSelector PinaforeFileUpdate
    PinaforeSelectMemory :: PinaforeSelector MemoryCellUpdate
    PinaforeSelectClock :: PinaforeSelector (ROWUpdate UTCTime)
    PinaforeSelectTimeZone :: PinaforeSelector (ROWUpdate TimeZone)

instance TestEquality PinaforeSelector where
    testEquality PinaforeSelectPoint PinaforeSelectPoint = Just Refl
    testEquality PinaforeSelectFile PinaforeSelectFile = Just Refl
    testEquality PinaforeSelectMemory PinaforeSelectMemory = Just Refl
    testEquality PinaforeSelectClock PinaforeSelectClock = Just Refl
    testEquality PinaforeSelectTimeZone PinaforeSelectTimeZone = Just Refl
    testEquality _ _ = Nothing

instance IsFiniteConsWitness PinaforeSelector where
    type FiniteConsWitness PinaforeSelector = '[ PinaforeEntityUpdate, PinaforeFileUpdate, MemoryCellUpdate, ROWUpdate UTCTime, ROWUpdate TimeZone]
    toLTW PinaforeSelectPoint = FirstElementType
    toLTW PinaforeSelectFile = RestElementType FirstElementType
    toLTW PinaforeSelectMemory = RestElementType $ RestElementType FirstElementType
    toLTW PinaforeSelectClock = RestElementType $ RestElementType $ RestElementType FirstElementType
    toLTW PinaforeSelectTimeZone =
        RestElementType $ RestElementType $ RestElementType $ RestElementType FirstElementType
    fromLTW FirstElementType = PinaforeSelectPoint
    fromLTW (RestElementType FirstElementType) = PinaforeSelectFile
    fromLTW (RestElementType (RestElementType FirstElementType)) = PinaforeSelectMemory
    fromLTW (RestElementType (RestElementType (RestElementType FirstElementType))) = PinaforeSelectClock
    fromLTW (RestElementType (RestElementType (RestElementType (RestElementType FirstElementType)))) =
        PinaforeSelectTimeZone
    fromLTW (RestElementType (RestElementType (RestElementType (RestElementType (RestElementType lt))))) = never lt

instance TupleEditWitness InvertibleEdit PinaforeSelector where
    tupleEditWitness PinaforeSelectPoint = Dict
    tupleEditWitness PinaforeSelectFile = Dict
    tupleEditWitness PinaforeSelectMemory = Dict
    tupleEditWitness PinaforeSelectClock = Dict
    tupleEditWitness PinaforeSelectTimeZone = Dict

type PinaforeUpdate = TupleUpdate PinaforeSelector
-}
