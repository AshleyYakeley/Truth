module Pinafore.Pinafore where

import Pinafore.File
import Pinafore.Table
import Shapes
import Truth.Core

data PinaforeSelector t where
    PinaforeSelectTable :: PinaforeSelector PinaforeTableEdit
    PinaforeSelectFile :: PinaforeSelector PinaforeFileEdit

instance TestEquality PinaforeSelector where
    testEquality PinaforeSelectTable PinaforeSelectTable = Just Refl
    testEquality PinaforeSelectFile PinaforeSelectFile = Just Refl
    testEquality _ _ = Nothing

instance IsFiniteConsWitness PinaforeSelector where
    type FiniteConsWitness PinaforeSelector = '[ PinaforeTableEdit, PinaforeFileEdit]
    toLTW PinaforeSelectTable = FirstListThingWitness
    toLTW PinaforeSelectFile = RestListThingWitness FirstListThingWitness
    fromLTW FirstListThingWitness = PinaforeSelectTable
    fromLTW (RestListThingWitness FirstListThingWitness) = PinaforeSelectFile
    fromLTW (RestListThingWitness (RestListThingWitness lt)) = never lt

type PinaforeEdit = TupleEdit PinaforeSelector

type PinaforeRead = EditReader PinaforeEdit

instance HasPinaforeTableEdit PinaforeEdit where
    pinaforeTableLens = tupleEditLens PinaforeSelectTable

instance HasPinaforeFileEdit PinaforeEdit where
    pinaforeFileLens = tupleEditLens PinaforeSelectFile
