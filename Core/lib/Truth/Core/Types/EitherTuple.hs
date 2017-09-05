{-# OPTIONS -fno-warn-orphans #-}
module Truth.Core.Types.EitherTuple where
{
    import Truth.Core.Import;
    import Truth.Core.Edit;
    import Truth.Core.Types.Tuple;


    instance (TupleWitness c p,TupleWitness c q) => TupleWitness c (EitherWitness p q) where
    {
        tupleWitness pc (LeftWitness sel) = tupleWitness pc sel;
        tupleWitness pc (RightWitness sel) = tupleWitness pc sel;
    };

    instance (TestEquality p,TupleWitness Edit p,TestEquality q,TupleWitness Edit q) => TupleSelector (EitherWitness p q);

    instance (TupleReaderWitness c p,TupleReaderWitness c q) => TupleReaderWitness c (EitherWitness p q) where
    {
        tupleReaderWitness pc (LeftWitness sel) = tupleReaderWitness pc sel;
        tupleReaderWitness pc (RightWitness sel) = tupleReaderWitness pc sel;
    };

    instance (FiniteTupleSelector p,TupleSubject p ~ Tuple p,FiniteTupleSelector q,TupleSubject q ~ Tuple q) => FiniteTupleSelector (EitherWitness p q) where
    {
        tupleConstruct getsel = (\(MkTuple p) (MkTuple q) -> MkTuple $ \sel -> case sel of
        {
            LeftWitness rt -> p rt;
            RightWitness rt -> q rt;
        }) <$> tupleConstruct (getsel . LeftWitness) <*> tupleConstruct (getsel . RightWitness);
    };

    eitherTuple :: Tuple sel1 -> Tuple sel2 -> Tuple (EitherWitness sel1 sel2);
    eitherTuple (MkTuple tup1) (MkTuple tup2) = MkTuple $ \esel -> case esel of
    {
        LeftWitness sel -> tup1 sel;
        RightWitness sel -> tup2 sel;
    };
}
