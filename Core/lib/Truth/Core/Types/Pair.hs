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

    instance TestEquality (PairSelector ea eb) where
    {
        testEquality EditFirst EditFirst = Just Refl;
        testEquality EditSecond EditSecond = Just Refl;
        testEquality _ _ = Nothing;
    };

    instance (Edit ea,Edit eb) =>
        TupleSelector (PairSelector ea eb) where
    {
        type TupleSubject (PairSelector ea eb) = (EditSubject ea,EditSubject eb);
        tupleIsEdit EditFirst = MkConstraintWitness;
        tupleIsEdit EditSecond = MkConstraintWitness;
        tupleReadFrom EditFirst (a,_b) = a;
        tupleReadFrom EditSecond (_a,b) = b;
    };

    instance (Edit ea,FullReader (EditReader ea),Edit eb,FullReader (EditReader eb)) =>
        FiniteTupleSelector (PairSelector ea eb) where
    {
        tupleIsFullReader EditFirst = MkConstraintWitness;
        tupleIsFullReader EditSecond = MkConstraintWitness;
        tupleConstruct f = (,) <$> f EditFirst <*> f EditSecond;
    };

    instance (FullEdit ea,FullEdit eb) =>
        FullTupleSelector (PairSelector ea eb) where
    {
        tupleIsFullEdit EditFirst = MkConstraintWitness;
        tupleIsFullEdit EditSecond = MkConstraintWitness;
    };
}
