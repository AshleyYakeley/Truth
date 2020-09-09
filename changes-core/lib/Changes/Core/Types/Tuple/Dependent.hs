module Changes.Core.Types.Tuple.Dependent
    ( DependentSelector(..)
    , DependentUpdate
    , dependentChangeLens
    ) where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.Tuple.Tuple
import Changes.Core.Types.Whole

data DependentSelector (wit :: Type -> Type) (update :: Type) where
    MkDependentSelector :: wit a -> DependentSelector wit (WholeUpdate a)

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

instance TupleEditWitness ApplicableEdit (DependentSelector wit) where
    tupleEditWitness (MkDependentSelector _) = Dict

instance TupleEditWitness InvertibleEdit (DependentSelector wit) where
    tupleEditWitness (MkDependentSelector _) = Dict

instance TupleEditWitness SubjectMapEdit (DependentSelector wit) where
    tupleEditWitness (MkDependentSelector _) = Dict

instance TupleUpdateWitness IsUpdate (DependentSelector wit) where
    tupleUpdateWitness (MkDependentSelector _) = Dict

instance TupleUpdateWitness IsEditUpdate (DependentSelector wit) where
    tupleUpdateWitness (MkDependentSelector _) = Dict

type DependentUpdate wit = TupleUpdate (DependentSelector wit)

dependentChangeLens :: TestEquality wit => wit a -> ChangeLens (DependentUpdate wit) (WholeUpdate a)
dependentChangeLens a = tupleChangeLens $ MkDependentSelector a
