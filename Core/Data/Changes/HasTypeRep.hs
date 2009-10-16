module Data.Changes.HasTypeRep where
{
    import Data.Changes.EditRep;
    import Data.TypeKT.WitnessKT;
    import Data.OpenWitness;
    import Data.Witness;
    import Data.Result;
    import Data.ByteString;
    import Data.Word;


    -- T

    class HasTypeRepT a where
    {
        typeRepT :: EditRepT a;

        --type TypeRepTEvidence a = ();
        type TypeRepTEvidence a;
        typeRepTEvidence :: forall r. r a -> TypeRepTEvidence a;
        --typeRepTEvidence _ = ();
    };

    data TypeRepT a where
    {
        MkTypeRepT :: forall a. (HasTypeRepT a) => TypeRepT a;
    };

    typeRepTWitness :: TypeRepT a -> EditRepT a;
    typeRepTWitness MkTypeRepT = typeRepT;

    instance SimpleWitness TypeRepT where
    {
        matchWitness a b = matchWitnessT (typeRepTWitness a) (typeRepTWitness b);
    };

    instance HasTypeRepT () where
    {
        typeRepT = EditRepT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.()");

        type TypeRepTEvidence () = ();
        typeRepTEvidence _ = ();
    };

    instance HasTypeRepT Bool where
    {
        typeRepT = EditRepT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.Bool");

        type TypeRepTEvidence Bool = ();
        typeRepTEvidence _ = ();
    };

    instance HasTypeRepT Word8 where
    {
        typeRepT = EditRepT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.Word8");

        type TypeRepTEvidence Word8 = ();
        typeRepTEvidence _ = ();
    };

    instance HasTypeRepT Char where
    {
        typeRepT = EditRepT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.Char");

        type TypeRepTEvidence Char = ();
        typeRepTEvidence _ = ();
    };

    instance HasTypeRepT Int where
    {
        typeRepT = EditRepT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.Int");

        type TypeRepTEvidence Int = ();
        typeRepTEvidence _ = ();
    };

    instance HasTypeRepT ByteString where
    {
        typeRepT = EditRepT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.ByteString");

        type TypeRepTEvidence ByteString = ();
        typeRepTEvidence _ = ();
    };


    -- KTT

    class HasTypeRepKTT a where
    {
        typeRepKTT :: EditRepKTT a;
    };

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

    instance (HasTypeRepKTT f,HasTypeRepT a) => HasTypeRepT (f a) where
    {
        typeRepT = TEditRepT typeRepKTT typeRepT;

        type TypeRepTEvidence (f a) = (TypeRepKTT f,TypeRepT a);
        typeRepTEvidence _ = (MkTypeRepKTT,MkTypeRepT);
    };

    instance HasTypeRepKTT Maybe where
    {
        typeRepKTT = EditRepKTT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.Maybe");
    };

    instance HasTypeRepKTT [] where
    {
        typeRepKTT = EditRepKTT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.[]");
    };


    -- KTKTT

    class HasTypeRepKTKTT a where
    {
        typeRepKTKTT :: EditRepKTKTT a;
    };

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

    instance (HasTypeRepKTKTT f,HasTypeRepT a) => HasTypeRepKTT (f a) where
    {
        typeRepKTT = TEditRepKTT typeRepKTKTT typeRepT;
    };

    instance HasTypeRepKTKTT (->) where
    {
        typeRepKTKTT = EditRepKTKTT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.->");
    };

    instance HasTypeRepKTKTT Either where
    {
        typeRepKTKTT = EditRepKTKTT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.Either");
    };

    instance HasTypeRepKTKTT Result where
    {
        typeRepKTKTT = EditRepKTKTT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.Result");
    };


    -- KKTTKTT

    class HasTypeRepKKTTKTT a where
    {
        typeRepKKTTKTT :: EditRepKKTTKTT a;
    };

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

    instance (HasTypeRepKKTTKTT f,HasTypeRepKTT a) => HasTypeRepKTT (f a) where
    {
        typeRepKTT = KTTEditRepKTT typeRepKKTTKTT typeRepKTT;
    };


    -- KTKTKTT

    class HasTypeRepKTKTKTT a where
    {
        typeRepKTKTKTT :: EditRepKTKTKTT a;
    };

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

    instance (HasTypeRepKTKTKTT f,HasTypeRepT a) => HasTypeRepKTKTT (f a) where
    {
        typeRepKTKTT = TEditRepKTKTT typeRepKTKTKTT typeRepT;
    };
}
