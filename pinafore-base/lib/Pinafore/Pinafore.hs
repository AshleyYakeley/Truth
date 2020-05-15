module Pinafore.Pinafore where

import Pinafore.Base
import Pinafore.Storage.File
import Shapes
import Truth.Core

data PinaforeSelector t where
    PinaforeSelectPoint :: PinaforeSelector PinaforeEntityUpdate
    PinaforeSelectFile :: PinaforeSelector PinaforeFileUpdate

instance TestEquality PinaforeSelector where
    testEquality PinaforeSelectPoint PinaforeSelectPoint = Just Refl
    testEquality PinaforeSelectFile PinaforeSelectFile = Just Refl
    testEquality _ _ = Nothing

instance IsFiniteConsWitness PinaforeSelector where
    type FiniteConsWitness PinaforeSelector = '[ PinaforeEntityUpdate, PinaforeFileUpdate]
    toLTW PinaforeSelectPoint = FirstElementType
    toLTW PinaforeSelectFile = RestElementType FirstElementType
    fromLTW FirstElementType = PinaforeSelectPoint
    fromLTW (RestElementType FirstElementType) = PinaforeSelectFile
    fromLTW (RestElementType (RestElementType lt)) = never lt

instance TupleEditWitness InvertibleEdit PinaforeSelector where
    tupleEditWitness PinaforeSelectPoint = Dict
    tupleEditWitness PinaforeSelectFile = Dict

type PinaforeUpdate = TupleUpdate PinaforeSelector

instance BaseChangeLens PinaforeEntityUpdate PinaforeUpdate where
    baseChangeLens = tupleChangeLens PinaforeSelectPoint

instance BaseChangeLens PinaforeFileUpdate PinaforeUpdate where
    baseChangeLens = tupleChangeLens PinaforeSelectFile
