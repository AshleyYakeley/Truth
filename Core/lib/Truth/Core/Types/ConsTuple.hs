module Truth.Core.Types.ConsTuple where
{
    import Truth.Core.Import;
    import Truth.Core.Edit;
    import Truth.Core.Types.Tuple;


    newtype EmptySel t = MkEmptySel None deriving (Eq,Countable,Searchable,Empty);

    instance Finite (EmptySel t) where
    {
        allValues = [];
        assemble _ = pure never;
    };

    instance TupleWitness c EmptySel where
    {
        tupleWitness _ = never;
    };

    instance TestEquality EmptySel where
    {
        testEquality = never;
    };

    instance TupleSelector EmptySel;

    instance TupleReaderWitness c EmptySel where
    {
        tupleReaderWitness _ = never;
    };

    instance FiniteTupleSelector EmptySel where
    {
        tupleConstruct _ = pure $ MkTuple never;
    };

    instance TupleHasInfo EmptySel where
    {
        tupleHasInfo = never;
    };

    emptyTuple :: Tuple EmptySel;
    emptyTuple = MkTuple never;


    data ConsSel a r t where
    {
        FirstSel :: ConsSel t r t;
        RestSel :: r t -> ConsSel a r t;
    };

    instance TestEquality r => TestEquality (ConsSel a r) where
    {
        testEquality FirstSel FirstSel = return Refl;
        testEquality (RestSel r1) (RestSel r2) = do
        {
            Refl <- testEquality r1 r2;
            return Refl;
        };
        testEquality _ _ = Nothing;
    };

    instance (c a,TupleWitness c r) => TupleWitness c (ConsSel a r) where
    {
        tupleWitness _ FirstSel = MkConstraintWitness;
        tupleWitness pc (RestSel r) = tupleWitness pc r;
    };

    instance (Edit a,TestEquality r,TupleWitness Edit r) => TupleSelector (ConsSel a r);

    instance (c (EditReader a),TupleReaderWitness c r) => TupleReaderWitness c (ConsSel a r) where
    {
        tupleReaderWitness _ FirstSel = MkConstraintWitness;
        tupleReaderWitness pc (RestSel r) = tupleReaderWitness pc r;
    };

    instance (Edit a,FiniteTupleSelector r,TupleSubject r ~ Tuple r) => FiniteTupleSelector (ConsSel a r) where
    {
        tupleConstruct getsel = (\f (MkTuple r) -> MkTuple $ \sel -> case sel of
        {
            FirstSel -> f;
            RestSel rt -> r rt;
        }) <$> getsel FirstSel <*> tupleConstruct (getsel . RestSel);
    };

    instance (Edit a,HasTypeInfo a,TupleHasInfo r) => TupleHasInfo (ConsSel a r) where
    {
        tupleHasInfo FirstSel = typeInfo;
        tupleHasInfo (RestSel r) = tupleHasInfo r;
    };

    consTuple :: EditSubject a -> Tuple r -> Tuple (ConsSel a r);
    consTuple a (MkTuple tup) = MkTuple $ \esel -> case esel of
    {
        FirstSel -> a;
        RestSel sel -> tup sel;
    };
}
