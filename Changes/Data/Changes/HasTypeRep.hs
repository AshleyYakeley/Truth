module Data.Changes.HasTypeRep where
{
	import Data.OpenWitness.OpenRep;
	import Data.OpenWitness;
	import Data.Word;
	import Data.Result;

	class HasTypeRep a where
	{
		typeRep :: OpenRep a;
	};

	instance HasTypeRep () where
	{
		typeRep = SimpleOpenRep (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.()");
	};

	instance HasTypeRep Bool where
	{
		typeRep = SimpleOpenRep (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.Bool");
	};

	instance HasTypeRep Word8 where
	{
		typeRep = SimpleOpenRep (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.Word8");
	};

	instance HasTypeRep Char where
	{
		typeRep = SimpleOpenRep (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.Char");
	};

	class HasTypeRep1 a where
	{
		typeRep1 :: OpenRep1 a;
	};

	instance (HasTypeRep1 f,HasTypeRep a) => HasTypeRep (f a) where
	{
		typeRep = ApplyOpenRep typeRep1 typeRep;
	};

	instance HasTypeRep1 Maybe where
	{
		typeRep1 = SimpleOpenRep1 (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.Maybe");
	};

	instance HasTypeRep1 [] where
	{
		typeRep1 = SimpleOpenRep1 (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.[]");
	};

	class HasTypeRep2 a where
	{
		typeRep2 :: OpenRep2 a;
	};

	instance (HasTypeRep2 f,HasTypeRep a) => HasTypeRep1 (f a) where
	{
		typeRep1 = ApplyOpenRep1 typeRep2 typeRep;
	};

	instance HasTypeRep2 (->) where
	{
		typeRep2 = SimpleOpenRep2 (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.->");
	};

	instance HasTypeRep2 Result where
	{
		typeRep2 = SimpleOpenRep2 (unsafeIOWitnessFromString "Data.Changes.HasTypeRep.Result");
	};
}
