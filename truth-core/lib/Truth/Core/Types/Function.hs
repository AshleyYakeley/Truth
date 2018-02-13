module Truth.Core.Types.Function where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Tuple

data FunctionSelector a (eb :: *) (et :: *) where
    MkFunctionSelector :: a -> FunctionSelector a edit edit

instance (Eq a) => TestEquality (FunctionSelector a editb) where
    testEquality (MkFunctionSelector a1) (MkFunctionSelector a2)
        | a1 == a2 = Just Refl
    testEquality _ _ = Nothing

instance (Finite a, SubjectReader (EditReader edit)) => SubjectTupleSelector (FunctionSelector a edit) where
    type TupleSubject (FunctionSelector a edit) = a -> EditSubject edit
    tupleReadFromSubject (MkFunctionSelector a) ab = ab a

instance Finite a => FiniteTupleSelector (FunctionSelector a edit) where
    tupleConstruct f = assemble (\a -> f (MkFunctionSelector a))

instance (c (EditReader edit)) => TupleReaderWitness c (FunctionSelector a edit) where
    tupleReaderWitness (MkFunctionSelector _) = Dict

instance (c edit) => TupleWitness c (FunctionSelector a edit) where
    tupleWitness (MkFunctionSelector _) = Dict
