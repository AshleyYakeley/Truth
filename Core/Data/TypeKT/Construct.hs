module Data.TypeKT.Construct where
{
    import Data.TypeKT.Type;

    class (PropertyT f) => ConstructT f where
    {
        constructT :: f t -> InfoT t;
    };


    data TMatchT t where
    {
        MkTMatchT :: forall f a. InfoKTT f -> InfoT a -> TMatchT (f a);
    };

    instance PropertyT TMatchT where
    {
        matchPropertyT (MkInfoT (TWitT tf ta) _) = return (MkTMatchT tf ta);
        matchPropertyT _ = Nothing;
    };

    applyTInfoT :: InfoKTT f -> InfoT a -> InfoT (f a);
    applyTInfoT tf@(MkInfoKTT _ inf) ta = MkInfoT (TWitT tf ta) (deriveTFactsT inf ta);

    instance ConstructT TMatchT where
    {
        constructT (MkTMatchT tf ta) = applyTInfoT tf ta;
    };


    class (PropertyKTT f) => ConstructKTT f where
    {
        constructKTT :: f t -> InfoKTT t;
    };



    data TMatchKTT t where
    {
        MkTMatchKTT :: forall f a. InfoKTKTT f -> InfoT a -> TMatchKTT (f a);
    };

    instance PropertyKTT TMatchKTT where
    {
        matchPropertyKTT (MkInfoKTT (TWitKTT tf ta) _) = return (MkTMatchKTT tf ta);
        matchPropertyKTT _ = Nothing;
    };

    applyTInfoKTT :: InfoKTKTT f -> InfoT a -> InfoKTT (f a);
    applyTInfoKTT tf@(MkInfoKTKTT _ inf) ta = MkInfoKTT (TWitKTT tf ta) (deriveTFactsKTT inf ta);

    instance ConstructKTT TMatchKTT where
    {
        constructKTT (MkTMatchKTT tf ta) = applyTInfoKTT tf ta;
    };

    data KTTMatchKTT t where
    {
        MkKTTMatchKTT :: forall f a. InfoKKTTKTT f -> InfoKTT a -> KTTMatchKTT (f a);
    };

    instance PropertyKTT KTTMatchKTT where
    {
        matchPropertyKTT (MkInfoKTT (KTTWitKTT tf ta) _) = return (MkKTTMatchKTT tf ta);
        matchPropertyKTT _ = Nothing;
    };

    applyKTTInfoKTT :: InfoKKTTKTT f -> InfoKTT a -> InfoKTT (f a);
    applyKTTInfoKTT tf@(MkInfoKKTTKTT _ inf) ta = MkInfoKTT (KTTWitKTT tf ta) (deriveKTTFactsKTT inf ta);

    instance ConstructKTT KTTMatchKTT where
    {
        constructKTT (MkKTTMatchKTT tf ta) = applyKTTInfoKTT tf ta;
    };

    applyTInfoKTKTT :: InfoKTKTKTT f -> InfoT a -> InfoKTKTT (f a);
    applyTInfoKTKTT tf@(MkInfoKTKTKTT _ inf) ta = MkInfoKTKTT (TWitKTKTT tf ta) (deriveTFactsKTKTT inf ta);

}
