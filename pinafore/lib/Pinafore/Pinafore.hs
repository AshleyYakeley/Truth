module Pinafore.Pinafore where

import Data.Time.Clock
import Pinafore.Base
import Pinafore.Storage.File
import Shapes
import Truth.Core

data PinaforeSelector t where
    PinaforeSelectPoint :: PinaforeSelector PinaforeEntityEdit
    PinaforeSelectFile :: PinaforeSelector PinaforeFileEdit
    PinaforeSelectMemory :: PinaforeSelector MemoryCellEdit
    PinaforeSelectClock :: PinaforeSelector (WholeEdit UTCTime)

instance TestEquality PinaforeSelector where
    testEquality PinaforeSelectPoint PinaforeSelectPoint = Just Refl
    testEquality PinaforeSelectFile PinaforeSelectFile = Just Refl
    testEquality PinaforeSelectMemory PinaforeSelectMemory = Just Refl
    testEquality PinaforeSelectClock PinaforeSelectClock = Just Refl
    testEquality _ _ = Nothing

instance IsFiniteConsWitness PinaforeSelector where
    type FiniteConsWitness PinaforeSelector = '[ PinaforeEntityEdit, PinaforeFileEdit, MemoryCellEdit, WholeEdit UTCTime]
    toLTW PinaforeSelectPoint = FirstElementType
    toLTW PinaforeSelectFile = RestElementType FirstElementType
    toLTW PinaforeSelectMemory = RestElementType $ RestElementType FirstElementType
    toLTW PinaforeSelectClock = RestElementType $ RestElementType $ RestElementType FirstElementType
    fromLTW FirstElementType = PinaforeSelectPoint
    fromLTW (RestElementType FirstElementType) = PinaforeSelectFile
    fromLTW (RestElementType (RestElementType FirstElementType)) = PinaforeSelectMemory
    fromLTW (RestElementType (RestElementType (RestElementType FirstElementType))) = PinaforeSelectClock
    fromLTW (RestElementType (RestElementType (RestElementType (RestElementType lt)))) = never lt

instance TupleWitness InvertibleEdit PinaforeSelector where
    tupleWitness PinaforeSelectPoint = Dict
    tupleWitness PinaforeSelectFile = Dict
    tupleWitness PinaforeSelectMemory = Dict
    tupleWitness PinaforeSelectClock = Dict

type PinaforeEdit = TupleEdit PinaforeSelector

type PinaforeRead = EditReader PinaforeEdit

instance BaseEditLens PinaforeEntityEdit PinaforeEdit where
    baseEditLens = tupleEditLens PinaforeSelectPoint

instance BaseEditLens PinaforeFileEdit PinaforeEdit where
    baseEditLens = tupleEditLens PinaforeSelectFile

instance BaseEditLens MemoryCellEdit PinaforeEdit where
    baseEditLens = tupleEditLens PinaforeSelectMemory

instance BaseEditLens (WholeEdit UTCTime) PinaforeEdit where
    baseEditLens = tupleEditLens PinaforeSelectClock
