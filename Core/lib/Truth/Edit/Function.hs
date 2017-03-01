module Truth.Edit.Function where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;
    import Truth.Edit.Edit;
    import Truth.Edit.Tuple;


    data FunctionAggregate a eb et where
    {
        MkFunctionAggregate :: a -> FunctionAggregate a edit edit;
    };

    instance (Eq a) => TestEquality (FunctionAggregate a editb) where
    {
        testEquality (MkFunctionAggregate a1) (MkFunctionAggregate a2) | a1 == a2 = Just Refl;
        testEquality _ _ = Nothing;
    };

    instance (Eq a,Finite a,Edit editb,FullReader (EditReader editb)) =>
        IsAggregate (FunctionAggregate a editb) where
    {
        type AggregateSubject (FunctionAggregate a editb) = a -> EditSubject editb;
        aggregateIsFullReaderEdit (MkFunctionAggregate _) = MkIsFullReaderEdit;
        aggregateReadFrom (MkFunctionAggregate a) ab = ab a;
        aggregateConstruct f = assemble (\a -> f (MkFunctionAggregate a));
    };
}
