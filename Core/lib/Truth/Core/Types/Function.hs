module Truth.Core.Types.Function where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Tuple;


    data FunctionSelector a eb et where
    {
        MkFunctionSelector :: a -> FunctionSelector a edit edit;
    };

    instance (Eq a) => TestEquality (FunctionSelector a editb) where
    {
        testEquality (MkFunctionSelector a1) (MkFunctionSelector a2) | a1 == a2 = Just Refl;
        testEquality _ _ = Nothing;
    };

    instance (Eq a,Finite a,Edit edit,FullReader (EditReader edit)) =>
        TupleSelector (FunctionSelector a edit) where
    {
        type TupleSubject (FunctionSelector a edit) = a -> EditSubject edit;
        tupleIsFullReaderEdit (MkFunctionSelector _) = MkConstraintWitness;
        tupleReadFrom (MkFunctionSelector a) ab = ab a;
        tupleConstruct f = assemble (\a -> f (MkFunctionSelector a));
    };

    instance (Finite a,FullEdit edit) => FiniteTupleSelector (FunctionSelector a edit) where
    {
        tupleAllSelectors = fmap (MkAnyWitness . MkFunctionSelector) allValues;
        tupleIsFullEdit (MkFunctionSelector _) = MkConstraintWitness;
    };
}
