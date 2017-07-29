module Truth.Core.Types.Function where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Tuple;


    data FunctionSelector a (eb :: *) (et :: *) where
    {
        MkFunctionSelector :: a -> FunctionSelector a edit edit;
    };

    instance (Eq a) => TestEquality (FunctionSelector a editb) where
    {
        testEquality (MkFunctionSelector a1) (MkFunctionSelector a2) | a1 == a2 = Just Refl;
        testEquality _ _ = Nothing;
    };

    instance (Finite a,Edit edit) => TupleSelector (FunctionSelector a edit) where
    {
        type TupleSubject (FunctionSelector a edit) = a -> EditSubject edit;
        tupleReadFrom (MkFunctionSelector a) ab = ab a;
    };

    instance (Finite a,Edit edit) => FiniteTupleSelector (FunctionSelector a edit) where
    {
        tupleConstruct f = assemble (\a -> f (MkFunctionSelector a));
    };

    instance (c (EditReader edit)) => TupleReaderWitness c (FunctionSelector a edit) where
    {
        tupleReaderWitness _ (MkFunctionSelector _) = MkConstraintWitness;
    };

    instance (c edit) => TupleWitness c (FunctionSelector a edit) where
    {
        tupleWitness _ (MkFunctionSelector _) = MkConstraintWitness;
    };

    $(return []);
    instance HasTypeInfo FunctionSelector where
    {
        typeWitness = $(generateWitness [t|FunctionSelector|]);
        typeName _ = "FunctionSelector";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance (Eq a) => (TestEquality :: (* -> *) -> Constraint) (FunctionSelector a editb);
            instance (Finite a,Edit edit) =>
                TupleSelector (FunctionSelector a edit) where
            {
                type TupleSubject (FunctionSelector a edit) = a -> EditSubject edit;
            };
            instance (Finite a,Edit edit) => FiniteTupleSelector (FunctionSelector a edit) where
            instance (c (EditReader edit)) => TupleReaderWitness c (FunctionSelector a edit);
            instance (c edit) => TupleWitness c (FunctionSelector a edit);
        |]);
    };
}
