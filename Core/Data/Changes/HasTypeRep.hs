module Data.Changes.HasTypeRep where
{
    import Data.Changes.EditRep;
    import Data.OpenWitness;
    import Data.Result;
    import Data.ByteString;
    import Data.Word;


    -- T

    class HasTypeRepT a where
    {
        typeRepT :: EditRepT a;
    };

    instance HasTypeRepT () where
    {
        typeRepT = EditRepT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.()");
    };

    instance HasTypeRepT Bool where
    {
        typeRepT = EditRepT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.Bool");
    };

    instance HasTypeRepT Word8 where
    {
        typeRepT = EditRepT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.Word8");
    };

    instance HasTypeRepT Char where
    {
        typeRepT = EditRepT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.Char");
    };

    instance HasTypeRepT Int where
    {
        typeRepT = EditRepT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.Int");
    };

    instance HasTypeRepT ByteString where
    {
        typeRepT = EditRepT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.ByteString");
    };


    -- KTT

    class HasTypeRepKTT a where
    {
        typeRepKTT :: EditRepKTT a;
    };

    instance (HasTypeRepKTT f,HasTypeRepT a) => HasTypeRepT (f a) where
    {
        typeRepT = TEditRepT typeRepKTT typeRepT;
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

    instance (HasTypeRepKKTTKTT f,HasTypeRepKTT a) => HasTypeRepKTT (f a) where
    {
        typeRepKTT = KTTEditRepKTT typeRepKKTTKTT typeRepKTT;
    };


    -- KTKTKTT

    class HasTypeRepKTKTKTT a where
    {
        typeRepKTKTKTT :: EditRepKTKTKTT a;
    };

    instance (HasTypeRepKTKTKTT f,HasTypeRepT a) => HasTypeRepKTKTT (f a) where
    {
        typeRepKTKTT = TEditRepKTKTT typeRepKTKTKTT typeRepT;
    };
}
