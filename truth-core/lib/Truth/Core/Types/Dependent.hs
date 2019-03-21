module Truth.Core.Types.Dependent where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Tuple
import Truth.Core.Types.Whole

data DependentSelector (wit :: Type -> Type) (edit :: Type) where
    MkDependentSelector :: wit a -> DependentSelector wit (WholeEdit a)

instance TestEquality wit => TestEquality (DependentSelector wit) where
    testEquality (MkDependentSelector a1) (MkDependentSelector a2) = do
        Refl <- testEquality a1 a2
        return Refl

instance TestEquality wit => SubjectTupleSelector (DependentSelector wit) where
    type TupleSubject (DependentSelector wit) = AllValue wit
    tupleReadFromSubject (MkDependentSelector wa) av = getAllValue av wa
    tupleWriteToSubject (MkDependentSelector wa) a = setAllValue wa a

instance (TestEquality wit, FiniteWitness wit) => FiniteTupleSelector (DependentSelector wit) where
    tupleConstruct f = assembleWitness $ \wt -> f $ MkDependentSelector wt

instance TupleReaderWitness SubjectReader (DependentSelector wit) where
    tupleReaderWitness (MkDependentSelector _) = Dict

instance TupleReaderWitness FullSubjectReader (DependentSelector wit) where
    tupleReaderWitness (MkDependentSelector _) = Dict

instance TupleWitness ApplicableEdit (DependentSelector wit) where
    tupleWitness (MkDependentSelector _) = Dict

instance TupleWitness InvertibleEdit (DependentSelector wit) where
    tupleWitness (MkDependentSelector _) = Dict

instance TupleWitness SubjectMapEdit (DependentSelector wit) where
    tupleWitness (MkDependentSelector _) = Dict

type DependentEdit wit = TupleEdit (DependentSelector wit)

dependentEditLens :: TestEquality wit => wit a -> EditLens (DependentEdit wit) (WholeEdit a)
dependentEditLens a = tupleEditLens $ MkDependentSelector a
