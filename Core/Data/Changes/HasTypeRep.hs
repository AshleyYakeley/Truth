module Data.Changes.HasTypeRep where
{
    import Data.Changes.EditRep;
    import Data.OpenWitness;
    import Data.Witness;
    import Data.Word;
    import Data.Result;

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

    instance HasTypeRepKTKTT Result where
    {
        typeRepKTKTT = EditRepKTKTT (unsafeIOWitnessFromString "Data.Changes.HasTypeRepT.Result");
    };

    class HasTypeRepKKTTKTT a where
    {
        typeRepKKTTKTT :: EditRepKKTTKTT a;
    };

    instance (HasTypeRepKKTTKTT f,HasTypeRepKTT a) => HasTypeRepKTT (f a) where
    {
        typeRepKTT = KTTEditRepKTT typeRepKKTTKTT typeRepKTT;
    };

    type RepDict = WitnessFDict EditRepT;
    
    repDictLookup :: (HasTypeRepT a) => RepDict f -> Maybe (f a);
    repDictLookup = witnessFDictLookup typeRepT;
    
    repDictAdd :: (HasTypeRepT a) => f a -> RepDict f -> RepDict f;
    repDictAdd = witnessFDictAdd typeRepT;
}
