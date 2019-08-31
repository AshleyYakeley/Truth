{-# OPTIONS -fno-warn-orphans #-}

module Truth.Core.Types.EitherTuple where

import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Tuple

instance (TupleUpdateWitness c p, TupleUpdateWitness c q) => TupleUpdateWitness c (EitherType p q) where
    tupleUpdateWitness (LeftType sel) = tupleUpdateWitness sel
    tupleUpdateWitness (RightType sel) = tupleUpdateWitness sel

instance (TestEquality p, TupleReaderWitness SubjectReader p, TestEquality q, TupleReaderWitness SubjectReader q) =>
             SubjectTupleSelector (EitherType p q)

instance (TupleReaderWitness c p, TupleReaderWitness c q) => TupleReaderWitness c (EitherType p q) where
    tupleReaderWitness (LeftType sel) = tupleReaderWitness sel
    tupleReaderWitness (RightType sel) = tupleReaderWitness sel

instance (FiniteTupleSelector p, TupleSubject p ~ Tuple p, FiniteTupleSelector q, TupleSubject q ~ Tuple q) =>
             FiniteTupleSelector (EitherType p q) where
    tupleConstruct getsel =
        (\(MkTuple p) (MkTuple q) ->
             MkTuple $ \sel ->
                 case sel of
                     LeftType rt -> p rt
                     RightType rt -> q rt) <$>
        tupleConstruct (getsel . LeftType) <*>
        tupleConstruct (getsel . RightType)

eitherTuple :: Tuple sel1 -> Tuple sel2 -> Tuple (EitherType sel1 sel2)
eitherTuple (MkTuple tup1) (MkTuple tup2) =
    MkTuple $ \esel ->
        case esel of
            LeftType sel -> tup1 sel
            RightType sel -> tup2 sel
