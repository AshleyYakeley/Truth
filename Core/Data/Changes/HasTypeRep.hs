module Data.Changes.HasTypeRep where
{
    import Data.Changes.HasNewValue;
    import Data.Changes.EditRep;
    import Data.FunctorOne;
    import Data.OpenWitness;
    import Data.Result;
    import Data.ByteString;
    import Data.Word;
    import Data.Monoid;



    -- T

    class HasTypeT a where
    {
        typeT :: TypeT a;
    };

    instance HasTypeT () where
    {
        typeT = MkTypeT
            (WitT (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.()"))
            (mkTInfoT (return MkHasNewValueInst));
    };

    instance HasTypeT Bool where
    {
        typeT = MkTypeT
            (WitT (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.Bool"))
            (mkTInfoT (return MkHasNewValueInst));
    };

    instance HasTypeT Word8 where
    {
        typeT = MkTypeT
            (WitT (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.Word8"))
            (mkTInfoT (return MkHasNewValueInst));
    };

    instance HasTypeT Char where
    {
        typeT = MkTypeT
            (WitT (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.Char"))
            (mkTInfoT (return MkHasNewValueInst));
    };

    instance HasTypeT Int where
    {
        typeT = MkTypeT
            (WitT (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.Int"))
            (mkTInfoT (return MkHasNewValueInst));
    };

    instance HasTypeT ByteString where
    {
        typeT = MkTypeT
            (WitT (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.ByteString"))
            (mkTInfoT (return MkHasNewValueInst));
    };


    -- KTT

    class HasTypeKTT a where
    {
        typeKTT :: TypeKTT a;
    };

    instance (HasTypeKTT f,HasTypeT a) => HasTypeT (f a) where
    {
        typeT = applyTTypeT typeKTT typeT;
    };

    instance HasTypeKTT Maybe where
    {
        typeKTT = MkTypeKTT
            (WitKTT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.Maybe"))
            (mconcat
            [
                mkTInfoKTT (\_ -> return MkHasNewValueInst),
                mkKTTInfoKTT (return MkFunctorOneInst)
            ]);
    };

    instance HasTypeKTT [] where
    {
        typeKTT = MkTypeKTT
            (WitKTT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.[]"))
            (mkTInfoKTT (\_ -> return MkHasNewValueInst));
    };


    -- KTKTT

    class HasTypeKTKTT a where
    {
        typeKTKTT :: TypeKTKTT a;
    };

    instance (HasTypeKTKTT f,HasTypeT a) => HasTypeKTT (f a) where
    {
        typeKTT = applyTTypeKTT typeKTKTT typeT;
    };

    instance HasTypeKTKTT (->) where
    {
        typeKTKTT = MkTypeKTKTT
            (WitKTKTT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.->"))
            mempty;
    };

    instance HasTypeKTKTT Result where
    {
        typeKTKTT = MkTypeKTKTT
            (WitKTKTT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.Result"))
            (mconcat [
                mkTInfoKTKTT (\_ ta -> do
                {
                    MkHasNewValueInst <- typeFactT ta;
                    return MkHasNewValueInst;
                }),
                mkKTTInfoKTKTT (\_ -> return MkFunctorOneInst)
            ]);
    };


    -- KKTTKTT

    class HasTypeKKTTKTT a where
    {
        typeKKTTKTT :: TypeKKTTKTT a;
    };

    instance (HasTypeKKTTKTT f,HasTypeKTT a) => HasTypeKTT (f a) where
    {
        typeKTT = applyKTTTypeKTT typeKKTTKTT typeKTT;
    };


    -- KTKTKTT

    class HasTypeKTKTKTT a where
    {
        typeKTKTKTT :: TypeKTKTKTT a;
    };

    instance (HasTypeKTKTKTT f,HasTypeT a) => HasTypeKTKTT (f a) where
    {
        typeKTKTT = applyTTypeKTKTT typeKTKTKTT typeT;
    };
}
