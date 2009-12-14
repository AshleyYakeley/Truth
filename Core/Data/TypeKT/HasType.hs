module Data.TypeKT.HasType where
{
    import Data.Changes.HasNewValue;
    import Data.TypeKT.Construct;
    import Data.TypeKT.Type;
    import Data.FunctorOne;
    import Data.OpenWitness;
    import Data.Result;
    import Data.ByteString;
    import Data.Word;
    import Data.Monoid;


    data FunctorOneInst f where
    {
        MkFunctorOneInst :: forall f. (FunctorOne f) => FunctorOneInst f;
    };

    instance PropertyKTT FunctorOneInst where
    {
        matchPropertyKTT = matchPropertyKTT_Fact;
    };

    instance FactKTT FunctorOneInst where
    {
        witFactKTT = unsafeIOWitnessFromString "Data.FunctorOne.FunctorOneInst";
    };

    data HasNewValueInst f where
    {
        MkHasNewValueInst :: forall f. (HasNewValue f) => HasNewValueInst f;
    };

    instance PropertyT HasNewValueInst where
    {
        matchPropertyT = matchPropertyT_Fact;
    };

    instance FactT HasNewValueInst where
    {
        witFactT = unsafeIOWitnessFromString "Data.Changes.HasNewValue.HasNewValueInst";
    };


    -- T

    class HasInfoT a where
    {
        infoT :: InfoT a;
    };

    instance HasInfoT () where
    {
        infoT = MkInfoT
            (WitT (unsafeIOWitnessFromString "Data.Changes.HasInfoRep.()"))
            (mkTFactsT (return MkHasNewValueInst));
    };

    instance HasInfoT Bool where
    {
        infoT = MkInfoT
            (WitT (unsafeIOWitnessFromString "Data.Changes.HasInfoRep.Bool"))
            (mkTFactsT (return MkHasNewValueInst));
    };

    instance HasInfoT Word8 where
    {
        infoT = MkInfoT
            (WitT (unsafeIOWitnessFromString "Data.Changes.HasInfoRep.Word8"))
            (mkTFactsT (return MkHasNewValueInst));
    };

    instance HasInfoT Char where
    {
        infoT = MkInfoT
            (WitT (unsafeIOWitnessFromString "Data.Changes.HasInfoRep.Char"))
            (mkTFactsT (return MkHasNewValueInst));
    };

    instance HasInfoT Int where
    {
        infoT = MkInfoT
            (WitT (unsafeIOWitnessFromString "Data.Changes.HasInfoRep.Int"))
            (mkTFactsT (return MkHasNewValueInst));
    };

    instance HasInfoT ByteString where
    {
        infoT = MkInfoT
            (WitT (unsafeIOWitnessFromString "Data.Changes.HasInfoRep.ByteString"))
            (mkTFactsT (return MkHasNewValueInst));
    };


    -- KTT

    class HasInfoKTT a where
    {
        infoKTT :: InfoKTT a;
    };

    instance (HasInfoKTT f,HasInfoT a) => HasInfoT (f a) where
    {
        infoT = applyTInfoT infoKTT infoT;
    };

    instance HasInfoKTT Maybe where
    {
        infoKTT = MkInfoKTT
            (WitKTT (unsafeIOWitnessFromString "Data.Changes.HasInfoRepT.Maybe"))
            (mconcat
            [
                mkTFactsKTT (\_ -> return MkHasNewValueInst),
                mkKTTFactsKTT (return MkFunctorOneInst)
            ]);
    };

    instance HasInfoKTT [] where
    {
        infoKTT = MkInfoKTT
            (WitKTT (unsafeIOWitnessFromString "Data.Changes.HasInfoRepT.[]"))
            (mkTFactsKTT (\_ -> return MkHasNewValueInst));
    };


    -- KTKTT

    class HasInfoKTKTT a where
    {
        infoKTKTT :: InfoKTKTT a;
    };

    instance (HasInfoKTKTT f,HasInfoT a) => HasInfoKTT (f a) where
    {
        infoKTT = applyTInfoKTT infoKTKTT infoT;
    };

    instance HasInfoKTKTT (->) where
    {
        infoKTKTT = MkInfoKTKTT
            (WitKTKTT (unsafeIOWitnessFromString "Data.Changes.HasInfoRepT.->"))
            mempty;
    };

    instance HasInfoKTKTT Result where
    {
        infoKTKTT = MkInfoKTKTT
            (WitKTKTT (unsafeIOWitnessFromString "Data.Changes.HasInfoRepT.Result"))
            (mconcat [
                mkTFactsKTKTT (\_ ta -> do
                {
                    MkHasNewValueInst <- matchPropertyT ta;
                    return MkHasNewValueInst;
                }),
                mkKTTFactsKTKTT (\_ -> return MkFunctorOneInst)
            ]);
    };


    -- KKTTKTT

    class HasInfoKKTTKTT a where
    {
        infoKKTTKTT :: InfoKKTTKTT a;
    };

    instance (HasInfoKKTTKTT f,HasInfoKTT a) => HasInfoKTT (f a) where
    {
        infoKTT = applyKTTInfoKTT infoKKTTKTT infoKTT;
    };


    -- KTKTKTT

    class HasInfoKTKTKTT a where
    {
        infoKTKTKTT :: InfoKTKTKTT a;
    };

    instance (HasInfoKTKTKTT f,HasInfoT a) => HasInfoKTKTT (f a) where
    {
        infoKTKTT = applyTInfoKTKTT infoKTKTKTT infoT;
    };
}
