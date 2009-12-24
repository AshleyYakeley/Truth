module Data.Changes.Anything where
{
    import Data.Changes.WholeEdit;
    import Data.Changes.Edit;
    import Data.Witness;
    import Data.TypeKT;
    import Data.ConstFunction;
    import Data.OpenWitness;

    data Anything where
    {
        MkAnything :: forall a. InfoT a -> a -> Anything;
    };

    instance HasInfoT Anything where
    {
        infoT = MkInfoT
            (WitT (unsafeIOWitnessFromString "Data.Changes.Anything.Anything"))
            mempty;
    };

    data AnyEdit where
    {
        MkAnyEdit :: forall edit. (Edit edit) => InfoT edit -> InfoT (Subject edit) -> edit -> AnyEdit;
    };

    instance Edit AnyEdit where
    {
        type Subject AnyEdit = Anything;

        applyEdit (MkAnyEdit _te tsubj edit) = FunctionConstFunction (\anya@(MkAnything ta a) -> case matchWitnessT tsubj ta of
        {
            Just MkEqualType -> MkAnything ta (applyConstFunction (applyEdit edit) a);
            _ -> anya;
        });

        invertEdit (MkAnyEdit te tsubj edit) (MkAnything ta a) = do
        {
            MkEqualType <- matchWitnessT tsubj ta;
            newa <- invertEdit edit a;
            return (MkAnyEdit te tsubj newa);
        };
    };

    instance HasInfoT AnyEdit where
    {
        infoT = MkInfoT
            (WitT (unsafeIOWitnessFromString "Data.Changes.Anything.AnyEdit"))
            (mkTFactsT (return (MkEditInst infoT)));
    };

    type AnyWholeEdit = Either (WholeEdit Anything) AnyEdit;
}
