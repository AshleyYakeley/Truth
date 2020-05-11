module Pinafore.Pinafore where

import Pinafore.Base
import Pinafore.Storage.File
import Shapes
import Truth.Core

data PinaforeSelector t where
    PinaforeSelectPoint :: PinaforeSelector PinaforeEntityUpdate
    PinaforeSelectFile :: PinaforeSelector PinaforeFileUpdate
    PinaforeSelectMemory :: PinaforeSelector MemoryCellUpdate

instance TestEquality PinaforeSelector where
    testEquality PinaforeSelectPoint PinaforeSelectPoint = Just Refl
    testEquality PinaforeSelectFile PinaforeSelectFile = Just Refl
    testEquality PinaforeSelectMemory PinaforeSelectMemory = Just Refl
    testEquality _ _ = Nothing

instance IsFiniteConsWitness PinaforeSelector where
    type FiniteConsWitness PinaforeSelector = '[ PinaforeEntityUpdate, PinaforeFileUpdate, MemoryCellUpdate]
    toLTW PinaforeSelectPoint = FirstElementType
    toLTW PinaforeSelectFile = RestElementType FirstElementType
    toLTW PinaforeSelectMemory = RestElementType $ RestElementType FirstElementType
    fromLTW FirstElementType = PinaforeSelectPoint
    fromLTW (RestElementType FirstElementType) = PinaforeSelectFile
    fromLTW (RestElementType (RestElementType FirstElementType)) = PinaforeSelectMemory
    fromLTW (RestElementType (RestElementType (RestElementType lt))) = never lt

instance TupleEditWitness InvertibleEdit PinaforeSelector where
    tupleEditWitness PinaforeSelectPoint = Dict
    tupleEditWitness PinaforeSelectFile = Dict
    tupleEditWitness PinaforeSelectMemory = Dict

type PinaforeUpdate = TupleUpdate PinaforeSelector

instance BaseChangeLens PinaforeEntityUpdate PinaforeUpdate where
    baseChangeLens = tupleChangeLens PinaforeSelectPoint

instance BaseChangeLens PinaforeFileUpdate PinaforeUpdate where
    baseChangeLens = tupleChangeLens PinaforeSelectFile

instance BaseChangeLens MemoryCellUpdate PinaforeUpdate where
    baseChangeLens = tupleChangeLens PinaforeSelectMemory
