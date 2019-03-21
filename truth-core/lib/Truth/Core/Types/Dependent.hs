module Truth.Core.Types.Dependent where

import Truth.Core.Edit
import Truth.Core.Import

--import Truth.Core.Read
--import Truth.Core.Types.Pair
import Truth.Core.Types.Tuple
import Truth.Core.Types.Whole

data DependentSelector (wit :: Type -> Type) (edit :: Type) where
    MkDependentSelector :: wit a -> DependentSelector wit (WholeEdit a)

instance TestEquality wit => TestEquality (DependentSelector wit) where
    testEquality (MkDependentSelector a1) (MkDependentSelector a2) = do
        Refl <- testEquality a1 a2
        return Refl

{-
instance (Finite a, SubjectReader (EditReader edit)) => SubjectTupleSelector (FunctionSelector a edit) where
    type TupleSubject (FunctionSelector a edit) = a -> EditSubject edit
    tupleReadFromSubject (MkFunctionSelector a) ab = ab a
    tupleWriteToSubject (MkFunctionSelector a) b _ a'
        | a == a' = b
    tupleWriteToSubject _ _ ab a' = ab a'

instance Finite a => FiniteTupleSelector (FunctionSelector a edit) where
    tupleConstruct f = assemble (\a -> f (MkFunctionSelector a))

instance (c (EditReader edit)) => TupleReaderWitness c (FunctionSelector a edit) where
    tupleReaderWitness (MkFunctionSelector _) = Dict

instance (c edit) => TupleWitness c (FunctionSelector a edit) where
    tupleWitness (MkFunctionSelector _) = Dict
-}
type DependentEdit wit = TupleEdit (DependentSelector wit)

dependentEditLens :: TestEquality wit => wit a -> EditLens (DependentEdit wit) (WholeEdit a)
dependentEditLens a = tupleEditLens $ MkDependentSelector a
