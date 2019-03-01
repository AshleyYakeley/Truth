module Pinafore.Pinafore where

import Pinafore.Base.Edit
import Pinafore.Base.PredicateMorphism
import Pinafore.Storage.File
import Shapes
import Truth.Core

data PinaforeSelector t where
    PinaforeSelectPoint :: PinaforeSelector PinaforeEntityEdit
    PinaforeSelectFile :: PinaforeSelector PinaforeFileEdit

instance TestEquality PinaforeSelector where
    testEquality PinaforeSelectPoint PinaforeSelectPoint = Just Refl
    testEquality PinaforeSelectFile PinaforeSelectFile = Just Refl
    testEquality _ _ = Nothing

instance IsFiniteConsWitness PinaforeSelector where
    type FiniteConsWitness PinaforeSelector = '[ PinaforeEntityEdit, PinaforeFileEdit]
    toLTW PinaforeSelectPoint = FirstElementType
    toLTW PinaforeSelectFile = RestElementType FirstElementType
    fromLTW FirstElementType = PinaforeSelectPoint
    fromLTW (RestElementType FirstElementType) = PinaforeSelectFile
    fromLTW (RestElementType (RestElementType lt)) = never lt

instance TupleWitness InvertibleEdit PinaforeSelector where
    tupleWitness PinaforeSelectPoint = Dict
    tupleWitness PinaforeSelectFile = Dict

type PinaforeEdit = TupleEdit PinaforeSelector

type PinaforeRead = EditReader PinaforeEdit

instance HasPinaforeEntityEdit PinaforeEdit where
    pinaforeEntityLens = tupleEditLens PinaforeSelectPoint

instance HasPinaforeFileEdit PinaforeEdit where
    pinaforeFileLens = tupleEditLens PinaforeSelectFile
