module Truth.Core.Types.Pair where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Tuple;


    data PairSelector ea eb (et :: *) where
    {
        EditFirst :: PairSelector ea eb ea;
        EditSecond :: PairSelector ea eb eb;
    };

    type PairEditReader ea eb = TupleEditReader (PairSelector ea eb);
    type PairEdit ea eb = TupleEdit (PairSelector ea eb);

    firstReadFunction :: ReadFunction (PairEditReader ea eb) (EditReader ea);
    firstReadFunction = tupleReadFunction EditFirst;

    secondReadFunction :: ReadFunction (PairEditReader ea eb) (EditReader eb);
    secondReadFunction = tupleReadFunction EditSecond;

    instance TestEquality (PairSelector ea eb) where
    {
        testEquality EditFirst EditFirst = Just Refl;
        testEquality EditSecond EditSecond = Just Refl;
        testEquality _ _ = Nothing;
    };

    instance (Edit ea,Edit eb) => TupleSelector (PairSelector ea eb) where
    {
        type TupleSubject (PairSelector ea eb) = (EditSubject ea,EditSubject eb);
        tupleReadFrom EditFirst (a,_b) = a;
        tupleReadFrom EditSecond (_a,b) = b;
    };

    instance (Edit ea,Edit eb) => FiniteTupleSelector (PairSelector ea eb) where
    {
        tupleConstruct f = (,) <$> f EditFirst <*> f EditSecond;
    };

    instance (c (EditReader ea),c (EditReader eb)) => TupleReaderWitness c (PairSelector ea eb) where
    {
        tupleReaderWitness _ EditFirst = MkConstraintWitness;
        tupleReaderWitness _ EditSecond = MkConstraintWitness;
    };

    instance (c ea,c eb) => TupleWitness c (PairSelector ea eb) where
    {
        tupleWitness _ EditFirst = MkConstraintWitness;
        tupleWitness _ EditSecond = MkConstraintWitness;
    };

    instance (Edit ea,HasInfo ea,Edit eb,HasInfo eb) =>
        TupleHasInfo (PairSelector ea eb) where
    {
        tupleHasInfo EditFirst = info;
        tupleHasInfo EditSecond = info;
    };

    $(return []);
    instance HasInfo PairSelector where
    {
        info = mkSimpleInfo $(ionamedwitness[t|PairSelector|]) [$(declInfo [d|
            --instance TestEquality (PairSelector ea eb);
            instance (Edit ea,Edit eb) =>
                TupleSelector (PairSelector ea eb) where
            {
                type TupleSubject (PairSelector ea eb) = (EditSubject ea,EditSubject eb);
            };
            instance (Edit ea,Edit eb) =>
                FiniteTupleSelector (PairSelector ea eb);
            instance (c (EditReader ea),c (EditReader eb)) => TupleReaderWitness c (PairSelector ea eb);
            instance (c ea,c eb) => TupleWitness c (PairSelector ea eb);
            --instance (Edit ea,HasInfo ea,Edit eb,HasInfo eb) =>
            --    TupleHasInfo (PairSelector ea eb);
        |])];
    };

    partitionPairEdits :: forall ea eb. [PairEdit ea eb] -> ([ea], [eb]);
    partitionPairEdits pes = let
    {
        toEither :: PairEdit ea eb -> Either ea eb;
        toEither (MkTupleEdit EditFirst ea) = Left ea;
        toEither (MkTupleEdit EditSecond eb) = Right eb;
    } in partitionEithers $ fmap toEither pes;

    pairMutableRead :: MutableRead m (EditReader ea) -> MutableRead m (EditReader eb) -> MutableRead m (PairEditReader ea eb);
    pairMutableRead mra _mrb (MkTupleEditReader EditFirst ra) = mra ra;
    pairMutableRead _mra mrb (MkTupleEditReader EditSecond rb) = mrb rb;
}
