{-# OPTIONS -fno-warn-orphans #-}
module Truth.Core.Types.ConsTuple where
{
    import Truth.Core.Import;
    import Truth.Core.Edit;
    import Truth.Core.Types.Tuple;


    instance TupleWitness c EmptyWitness where
    {
        tupleWitness _ = never;
    };

    instance TupleSelector EmptyWitness;

    instance TupleReaderWitness c EmptyWitness where
    {
        tupleReaderWitness _ = never;
    };

    instance FiniteTupleSelector EmptyWitness where
    {
        tupleConstruct _ = pure $ MkTuple never;
    };

    instance TupleHasInfo EmptyWitness where
    {
        tupleHasInfo = never;
    };

    emptyTuple :: Tuple EmptyWitness;
    emptyTuple = MkTuple never;


    instance (c a,TupleWitness c r) => TupleWitness c (ConsWitness a r) where
    {
        tupleWitness _ FirstWitness = MkConstraintWitness;
        tupleWitness pc (RestWitness r) = tupleWitness pc r;
    };

    instance (Edit a,TestEquality r,TupleWitness Edit r) => TupleSelector (ConsWitness a r);

    instance (c (EditReader a),TupleReaderWitness c r) => TupleReaderWitness c (ConsWitness a r) where
    {
        tupleReaderWitness _ FirstWitness = MkConstraintWitness;
        tupleReaderWitness pc (RestWitness r) = tupleReaderWitness pc r;
    };

    instance (Edit a,FiniteTupleSelector r,TupleSubject r ~ Tuple r) => FiniteTupleSelector (ConsWitness a r) where
    {
        tupleConstruct getsel = (\f (MkTuple r) -> MkTuple $ \sel -> case sel of
        {
            FirstWitness -> f;
            RestWitness rt -> r rt;
        }) <$> getsel FirstWitness <*> tupleConstruct (getsel . RestWitness);
    };

    instance (Edit a,HasTypeInfo a,TupleHasInfo r) => TupleHasInfo (ConsWitness a r) where
    {
        tupleHasInfo FirstWitness = typeInfo;
        tupleHasInfo (RestWitness r) = tupleHasInfo r;
    };

    consTuple :: EditSubject a -> Tuple r -> Tuple (ConsWitness a r);
    consTuple a (MkTuple tup) = MkTuple $ \esel -> case esel of
    {
        FirstWitness -> a;
        RestWitness sel -> tup sel;
    };
}
