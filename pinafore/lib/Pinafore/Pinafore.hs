module Pinafore.Pinafore where

import Pinafore.Base
import Pinafore.Storage.File
import Shapes
import Truth.Core

data PinaforeSelector t where
    PinaforeSelectPoint :: PinaforeSelector PinaforeEntityEdit
    PinaforeSelectFile :: PinaforeSelector PinaforeFileEdit
    PinaforeSelectMemory :: PinaforeSelector MemoryCellEdit

instance TestEquality PinaforeSelector where
    testEquality PinaforeSelectPoint PinaforeSelectPoint = Just Refl
    testEquality PinaforeSelectFile PinaforeSelectFile = Just Refl
    testEquality PinaforeSelectMemory PinaforeSelectMemory = Just Refl
    testEquality _ _ = Nothing

instance IsFiniteConsWitness PinaforeSelector where
    type FiniteConsWitness PinaforeSelector = '[ PinaforeEntityEdit, PinaforeFileEdit, MemoryCellEdit]
    toLTW PinaforeSelectPoint = FirstElementType
    toLTW PinaforeSelectFile = RestElementType FirstElementType
    toLTW PinaforeSelectMemory = RestElementType $ RestElementType FirstElementType
    fromLTW FirstElementType = PinaforeSelectPoint
    fromLTW (RestElementType FirstElementType) = PinaforeSelectFile
    fromLTW (RestElementType (RestElementType FirstElementType)) = PinaforeSelectMemory
    fromLTW (RestElementType (RestElementType (RestElementType lt))) = never lt

instance TupleWitness InvertibleEdit PinaforeSelector where
    tupleWitness PinaforeSelectPoint = Dict
    tupleWitness PinaforeSelectFile = Dict
    tupleWitness PinaforeSelectMemory = Dict

type PinaforeEdit = TupleEdit PinaforeSelector

type PinaforeRead = EditReader PinaforeEdit

instance BaseEditLens PinaforeEntityEdit PinaforeEdit where
    baseEditLens = tupleEditLens PinaforeSelectPoint

instance BaseEditLens PinaforeFileEdit PinaforeEdit where
    baseEditLens = tupleEditLens PinaforeSelectFile

instance BaseEditLens MemoryCellEdit PinaforeEdit where
    baseEditLens = tupleEditLens PinaforeSelectMemory
