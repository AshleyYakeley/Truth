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

    instance (Finite a,Edit edit) =>
        TupleSelector (FunctionSelector a edit) where
    {
        type TupleSubject (FunctionSelector a edit) = a -> EditSubject edit;
        tupleIsEdit (MkFunctionSelector _) = MkConstraintWitness;
        tupleReadFrom (MkFunctionSelector a) ab = ab a;
    };

    instance (Finite a,Edit edit,FullReader (EditReader edit)) =>
        FiniteTupleSelector (FunctionSelector a edit) where
    {
        tupleIsFullReader (MkFunctionSelector _) = MkConstraintWitness;
        tupleConstruct f = assemble (\a -> f (MkFunctionSelector a));
    };

    instance (Finite a,FullEdit edit) => FullTupleSelector (FunctionSelector a edit) where
    {
        tupleIsFullEdit (MkFunctionSelector _) = MkConstraintWitness;
    };
}
