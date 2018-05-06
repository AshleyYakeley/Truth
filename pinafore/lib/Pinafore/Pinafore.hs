module Pinafore.Pinafore where

import Pinafore.File
import Pinafore.Point
import Pinafore.PredicateMorphism
import Shapes
import Truth.Core

data PinaforeSelector t where
    PinaforeSelectPoint :: PinaforeSelector PinaforePointEdit
    PinaforeSelectFile :: PinaforeSelector PinaforeFileEdit

instance TestEquality PinaforeSelector where
    testEquality PinaforeSelectPoint PinaforeSelectPoint = Just Refl
    testEquality PinaforeSelectFile PinaforeSelectFile = Just Refl
    testEquality _ _ = Nothing

instance IsFiniteConsWitness PinaforeSelector where
    type FiniteConsWitness PinaforeSelector = '[ PinaforePointEdit, PinaforeFileEdit]
    toLTW PinaforeSelectPoint = FirstListElementWitness
    toLTW PinaforeSelectFile = RestListElementWitness FirstListElementWitness
    fromLTW FirstListElementWitness = PinaforeSelectPoint
    fromLTW (RestListElementWitness FirstListElementWitness) = PinaforeSelectFile
    fromLTW (RestListElementWitness (RestListElementWitness lt)) = never lt

type PinaforeEdit = TupleEdit PinaforeSelector

type PinaforeRead = EditReader PinaforeEdit

instance HasPinaforePointEdit PinaforeEdit where
    pinaforePointLens = tupleEditLens PinaforeSelectPoint

instance HasPinaforeFileEdit PinaforeEdit where
    pinaforeFileLens = tupleEditLens PinaforeSelectFile
