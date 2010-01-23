module Truth.TypeKT.HasType where
{
    import Truth.TypeKT.Construct;
    import Truth.TypeKT.Type;
    import Control.Monad;
    import Data.HasNewValue;
    import Data.FunctorOne;
    import Data.OpenWitness;
    import Data.Witness;
    import Data.Result;
    import Data.ByteString;
    import Data.Maybe;
    import Data.Word;
    import Data.Bool;
    import Data.Char;
    import Data.Int;


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
        witFactKTT = unsafeIOWitnessFromString "Truth.TypeKT.HasType.FunctorOneInst";
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
        witFactT = unsafeIOWitnessFromString "Truth.TypeKT.HasType.HasNewValueInst";
    };


    -- T

    class HasInfoT a where
    {
        infoT :: InfoT a;
    };

    instance HasInfoT () where
    {
        infoT = MkInfoT
            (WitT (unsafeIOWitnessFromString "Truth.TypeKT.HasType.()"))
            (mkTFactsT (return MkHasNewValueInst));
    };

    instance HasInfoT Bool where
    {
        infoT = MkInfoT
            (WitT (unsafeIOWitnessFromString "Truth.TypeKT.HasType.Bool"))
            (mkTFactsT (return MkHasNewValueInst));
    };

    instance HasInfoT Word8 where
    {
        infoT = MkInfoT
            (WitT (unsafeIOWitnessFromString "Truth.TypeKT.HasType.Word8"))
            (mkTFactsT (return MkHasNewValueInst));
    };

    instance HasInfoT Char where
    {
        infoT = MkInfoT
            (WitT (unsafeIOWitnessFromString "Truth.TypeKT.HasType.Char"))
            (mkTFactsT (return MkHasNewValueInst));
    };

    instance HasInfoT Int where
    {
        infoT = MkInfoT
            (WitT (unsafeIOWitnessFromString "Truth.TypeKT.HasType.Int"))
            (mkTFactsT (return MkHasNewValueInst));
    };

    instance HasInfoT ByteString where
    {
        infoT = MkInfoT
            (WitT (unsafeIOWitnessFromString "Truth.TypeKT.HasType.ByteString"))
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
            (WitKTT (unsafeIOWitnessFromString "Truth.TypeKT.HasType.Maybe"))
            (mconcat
            [
                mkTFactsKTT (\_ -> return MkHasNewValueInst),
                mkKTTFactsKTT (return MkFunctorOneInst)
            ]);
    };

    instance HasInfoKTT [] where
    {
        infoKTT = MkInfoKTT
            (WitKTT (unsafeIOWitnessFromString "Truth.TypeKT.HasType.[]"))
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
            (WitKTKTT (unsafeIOWitnessFromString "Truth.TypeKT.HasType.->"))
            mempty;
    };

    instance HasInfoKTKTT Result where
    {
        infoKTKTT = MkInfoKTKTT
            (WitKTKTT (unsafeIOWitnessFromString "Truth.TypeKT.HasType.Result"))
            (mconcat [
                mkTFactsKTKTT (\_ ta -> do
                {
                    MkHasNewValueInst <- matchPropertyT ta;
                    return MkHasNewValueInst;
                }),
                mkKTTFactsKTKTT (\_ -> return MkFunctorOneInst)
            ]);
    };


    -- KKTTT

    class HasInfoKKTTT a where
    {
        infoKKTTT :: InfoKKTTT a;
    };

    instance (HasInfoKKTTT f,HasInfoKTT a) => HasInfoT (f a) where
    {
        infoT = applyKTTInfoT infoKKTTT infoKTT;
    };

    instance HasInfoKKTTT Any where
    {
        infoKKTTT = MkInfoKKTTT
            (WitKKTTT (unsafeIOWitnessFromString "Truth.TypeKT.HasType.Any"))
            mempty;
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
