module Truth.Edit.Anything where
{
    import Truth.Edit.WholeEdit;
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    data Anything where
    {
        MkAnything :: forall a. Info (Type_T a) -> a -> Anything;
    };

    instance HasInfo (Type_T Anything) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_T Anything |])
        [
        ];
    };

    data AnyEdit where
    {
        MkAnyEdit :: forall edit. (Edit edit) => Info (Type_T edit) -> Info (Type_T (Subject edit)) -> edit -> AnyEdit;
    };

    instance Edit AnyEdit where
    {
        type Subject AnyEdit = Anything;

        applyEdit (MkAnyEdit _te tsubj edit) = FunctionConstFunction (\anya@(MkAnything ta a) -> case matchWitness tsubj ta of
        {
            Just MkEqualType -> MkAnything ta (applyConstFunction (applyEdit edit) a);
            _ -> anya;
        });

        invertEdit (MkAnyEdit te tsubj edit) (MkAnything ta a) = do
        {
            MkEqualType <- matchWitness tsubj ta;
            newa <- invertEdit edit a;
            return (MkAnyEdit te tsubj newa);
        };
    };

    instance HasInfo (Type_T AnyEdit) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_T AnyEdit |])
        [
            mkFacts (MkFactZ (do
            {
                return (Edit_Inst info);
            })
            :: FactZ Edit_Inst (Type_T AnyEdit)
            )
        ];
    };

    type AnyWholeEdit = Either (WholeEdit Anything) AnyEdit;
}
