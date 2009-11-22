module Data.Changes.HasTypeRep where
{
    import Data.Changes.EditRep;
    import Data.OpenWitness;
    import Data.Result;
    import Data.ByteString;
    import Data.Word;
    import Data.Monoid;


    -- T

    class HasTypeT a where
    {
        witT :: WitT a;
        infoT :: InfoT a;
        infoT = mempty;
    };

    typeT :: (HasTypeT a) => TypeT a;
    typeT = MkTypeT infoT witT;
{-
    data HasTypeTInst a where
    {
        MkHasTypeTInst :: forall a. (HasTypeT a) => HasTypeTInst a;
    };

    typeRepTWitness :: HasTypeTInst a -> TypeT a;
    typeRepTWitness MkHasTypeTInst = typeT;

    instance SimpleWitness HasTypeTInst where
    {
        matchWitness a b = matchWitnessT (typeRepTWitness a) (typeRepTWitness b);
    };
-}
    instance HasTypeT () where
    {
        witT = WitT (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.()");
    };

    instance HasTypeT Bool where
    {
        witT = WitT (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.Bool");
    };

    instance HasTypeT Word8 where
    {
        witT = WitT (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.Word8");
    };

    instance HasTypeT Char where
    {
        witT = WitT (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.Char");
    };

    instance HasTypeT Int where
    {
        witT = WitT (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.Int");
    };

    instance HasTypeT ByteString where
    {
        witT = WitT (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.ByteString");
    };


    -- KTT

    class HasTypeKTT a where
    {
        witKTT :: WitKTT a;
        infoKTT :: InfoKTT a;
        infoKTT = mempty;
    };

    typeKTT :: (HasTypeKTT a) => TypeKTT a;
    typeKTT = MkTypeKTT infoKTT witKTT;
{-
    data TypeRepKTT a where
    {
        MkTypeRepKTT :: forall a. (HasTypeRepKTT a) => TypeRepKTT a;
    };

    typeRepKTTWitness :: TypeRepKTT a -> EditRepKTT a;
    typeRepKTTWitness MkTypeRepKTT = typeRepKTT;

    instance WitnessKTT TypeRepKTT where
    {
        matchWitnessKTT a b = matchWitnessKTT (typeRepKTTWitness a) (typeRepKTTWitness b);
    };
-}
    instance (HasTypeKTT f,HasTypeT a) => HasTypeT (f a) where
    {
        witT = TWitT typeKTT typeT;
        infoT = deriveTInfoT (infoKTT :: InfoKTT f) (typeT :: TypeT a);
    };

    instance HasTypeKTT Maybe where
    {
        witKTT = WitKTT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.Maybe");
    };

    instance HasTypeKTT [] where
    {
        witKTT = WitKTT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.[]");
    };


    -- KTKTT

    class HasTypeKTKTT a where
    {
        witKTKTT :: WitKTKTT a;
        infoKTKTT :: InfoKTKTT a;
        infoKTKTT = mempty;
    };

    typeKTKTT :: (HasTypeKTKTT a) => TypeKTKTT a;
    typeKTKTT = MkTypeKTKTT infoKTKTT witKTKTT;
{-
    data TypeRepKTKTT a where
    {
        MkTypeRepKTKTT :: forall a. (HasTypeRepKTKTT a) => TypeRepKTKTT a;
    };

    typeRepKTKTTWitness :: TypeRepKTKTT a -> EditRepKTKTT a;
    typeRepKTKTTWitness MkTypeRepKTKTT = typeRepKTKTT;

    instance WitnessKTKTT TypeRepKTKTT where
    {
        matchWitnessKTKTT a b = matchWitnessKTKTT (typeRepKTKTTWitness a) (typeRepKTKTTWitness b);
    };
-}
    instance (HasTypeKTKTT f,HasTypeT a) => HasTypeKTT (f a) where
    {
        witKTT = TWitKTT typeKTKTT typeT;
        infoKTT = deriveTInfoKTT (infoKTKTT :: InfoKTKTT f) (typeT :: TypeT a);
    };

    instance HasTypeKTKTT (->) where
    {
        witKTKTT = WitKTKTT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.->");
    };

    instance HasTypeKTKTT Result where
    {
        witKTKTT = WitKTKTT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.Result");
    };


    -- KKTTKTT

    class HasTypeKKTTKTT a where
    {
        witKKTTKTT :: WitKKTTKTT a;
        infoKKTTKTT :: InfoKKTTKTT a;
        infoKKTTKTT = mempty;
    };

    typeKKTTKTT :: (HasTypeKKTTKTT a) => TypeKKTTKTT a;
    typeKKTTKTT = MkTypeKKTTKTT infoKKTTKTT witKKTTKTT;
{-
    data TypeRepKKTTKTT a where
    {
        MkTypeRepKKTTKTT :: forall a. (HasTypeRepKKTTKTT a) => TypeRepKKTTKTT a;
    };

    typeRepKKTTKTTWitness :: TypeRepKKTTKTT a -> EditRepKKTTKTT a;
    typeRepKKTTKTTWitness MkTypeRepKKTTKTT = typeRepKKTTKTT;

    instance WitnessKKTTKTT TypeRepKKTTKTT where
    {
        matchWitnessKKTTKTT a b = matchWitnessKKTTKTT (typeRepKKTTKTTWitness a) (typeRepKKTTKTTWitness b);
    };
-}
    instance (HasTypeKKTTKTT f,HasTypeKTT a) => HasTypeKTT (f a) where
    {
        witKTT = KTTWitKTT typeKKTTKTT typeKTT;
        infoKTT = deriveKTTInfoKTT (infoKKTTKTT :: InfoKKTTKTT f) (typeKTT :: TypeKTT a);
    };


    -- KTKTKTT

    class HasTypeKTKTKTT a where
    {
        witKTKTKTT :: WitKTKTKTT a;
        infoKTKTKTT :: InfoKTKTKTT a;
        infoKTKTKTT = mempty;
    };

    typeKTKTKTT :: (HasTypeKTKTKTT a) => TypeKTKTKTT a;
    typeKTKTKTT = MkTypeKTKTKTT infoKTKTKTT witKTKTKTT;
{-
    data TypeRepKTKTKTT a where
    {
        MkTypeRepKTKTKTT :: forall a. (HasTypeRepKTKTKTT a) => TypeRepKTKTKTT a;
    };

    typeRepKTKTKTTWitness :: TypeRepKTKTKTT a -> EditRepKTKTKTT a;
    typeRepKTKTKTTWitness MkTypeRepKTKTKTT = typeRepKTKTKTT;

    instance WitnessKTKTKTT TypeRepKTKTKTT where
    {
        matchWitnessKTKTKTT a b = matchWitnessKTKTKTT (typeRepKTKTKTTWitness a) (typeRepKTKTKTTWitness b);
    };
-}
    instance (HasTypeKTKTKTT f,HasTypeT a) => HasTypeKTKTT (f a) where
    {
        witKTKTT = TWitKTKTT typeKTKTKTT typeT;
        infoKTKTT = deriveTInfoKTKTT (infoKTKTKTT :: InfoKTKTKTT f) (typeT :: TypeT a);
    };
}
