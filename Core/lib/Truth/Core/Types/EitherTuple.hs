module Truth.Core.Types.EitherTuple where
{
    import Truth.Core.Import;
    import Truth.Core.Edit;
    import Truth.Core.Types.Tuple;


    data EitherSel (colsel1 :: k -> *) (colsel2 :: k -> *) (t :: k) = LeftSel (colsel1 t) | RightSel (colsel2 t);

    eitherAll :: All sel1 -> All sel2 -> All (EitherSel sel1 sel2);
    eitherAll (MkAll tup1) (MkAll tup2) = MkAll $ \esel -> case esel of
    {
        LeftSel sel -> tup1 sel;
        RightSel sel -> tup2 sel;
    };

    eitherAllF :: AllF sel1 f -> AllF sel2 f -> AllF (EitherSel sel1 sel2) f;
    eitherAllF (MkAllF tup1) (MkAllF tup2) = MkAllF $ \esel -> case esel of
    {
        LeftSel sel -> tup1 sel;
        RightSel sel -> tup2 sel;
    };

    instance (TestEquality colsel1,TestEquality colsel2) => TestEquality (EitherSel colsel1 colsel2) where
    {
        testEquality (LeftSel s1) (LeftSel s2) = do
        {
            Refl <- testEquality s1 s2;
            return Refl;
        };
        testEquality (RightSel s1) (RightSel s2) = do
        {
            Refl <- testEquality s1 s2;
            return Refl;
        };
        testEquality _ _ = Nothing;
    };

    instance (TupleWitness c p,TupleWitness c q) => TupleWitness c (EitherSel p q) where
    {
        tupleWitness pc (LeftSel sel) = tupleWitness pc sel;
        tupleWitness pc (RightSel sel) = tupleWitness pc sel;
    };

    instance (TestEquality p,TupleWitness Edit p,TestEquality q,TupleWitness Edit q) => TupleSelector (EitherSel p q);

    instance (TupleReaderWitness c p,TupleReaderWitness c q) => TupleReaderWitness c (EitherSel p q) where
    {
        tupleReaderWitness pc (LeftSel sel) = tupleReaderWitness pc sel;
        tupleReaderWitness pc (RightSel sel) = tupleReaderWitness pc sel;
    };

    instance (FiniteTupleSelector p,TupleSubject p ~ Tuple p,FiniteTupleSelector q,TupleSubject q ~ Tuple q) => FiniteTupleSelector (EitherSel p q) where
    {
        tupleConstruct getsel = (\(MkTuple p) (MkTuple q) -> MkTuple $ \sel -> case sel of
        {
            LeftSel rt -> p rt;
            RightSel rt -> q rt;
        }) <$> tupleConstruct (getsel . LeftSel) <*> tupleConstruct (getsel . RightSel);
    };

    instance (TupleHasInfo p,TupleHasInfo q) => TupleHasInfo (EitherSel p q) where
    {
        tupleHasInfo (LeftSel sel) = tupleHasInfo sel;
        tupleHasInfo (RightSel sel) = tupleHasInfo sel;
    };

    eitherTuple :: Tuple sel1 -> Tuple sel2 -> Tuple (EitherSel sel1 sel2);
    eitherTuple (MkTuple tup1) (MkTuple tup2) = MkTuple $ \esel -> case esel of
    {
        LeftSel sel -> tup1 sel;
        RightSel sel -> tup2 sel;
    };
}
