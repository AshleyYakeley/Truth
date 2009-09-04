module Data.Changes.EditRep where
{
	import Data.Witness;
	import Data.OpenWitness;
	import Data.Maybe;

	data EditRepKTKTT p where
	{
		EditRepKTKTT :: IOWitness (p () ()) -> EditRepKTKTT p;
	};

	matchEditRepKTKTT :: EditRepKTKTT a -> EditRepKTKTT b -> Maybe (EqualType (a () ()) (b () ()));
	matchEditRepKTKTT (EditRepKTKTT wa) (EditRepKTKTT wb) = matchWitness wa wb;

	data EditRepKKTTKTT p where
	{
		EditRepKKTTKTT :: IOWitness (p EditRepT ()) -> EditRepKKTTKTT p;
	};

	matchEditRepKKTTKTT :: EditRepKKTTKTT a -> EditRepKKTTKTT b -> Maybe (EqualType (a EditRepT ()) (b EditRepT ()));
	matchEditRepKKTTKTT (EditRepKKTTKTT wa) (EditRepKKTTKTT wb) = matchWitness wa wb;

	data EditRepKTT p where
	{
		EditRepKTT :: IOWitness (p ()) -> EditRepKTT p;
		TEditRepKTT :: EditRepKTKTT p -> EditRepT a -> EditRepKTT (p a);
		KTTEditRepKTT :: EditRepKKTTKTT p -> EditRepKTT a -> EditRepKTT (p a);
	};

	matchEditRepKTT :: EditRepKTT a -> EditRepKTT b -> Maybe (EqualType (a ()) (b ()));
	matchEditRepKTT (EditRepKTT wa) (EditRepKTT wb) = matchWitness wa wb;
	matchEditRepKTT (TEditRepKTT tfa ta) (TEditRepKTT tfb tb) = do
	{
		MkEqualType <- matchEditRepKTKTT tfa tfb;
		MkEqualType <- matchWitness ta tb;
		return MkEqualType;
	};
	matchEditRepKTT (KTTEditRepKTT tfa ta) (KTTEditRepKTT tfb tb) = do
	{
		MkEqualType <- matchEditRepKKTTKTT tfa tfb;
		MkEqualType <- matchEditRepKTT ta tb;
		return MkEqualType;
	};
	matchEditRepKTT _ _ = Nothing;

	data EditRepT a where
	{
		EditRepT :: IOWitness a -> EditRepT a;
		TEditRepT :: EditRepKTT p -> EditRepT a -> EditRepT (p a);
	};

	instance SimpleWitness EditRepT where
	{
		matchWitness (EditRepT wa) (EditRepT wb) = matchWitness wa wb;
		matchWitness (TEditRepT tfa ta) (TEditRepT tfb tb) = do
		{
			MkEqualType <- matchEditRepKTT tfa tfb;
			MkEqualType <- matchWitness ta tb;
			return MkEqualType;
		};
		matchWitness _ _ = Nothing;
	};

	instance Eq1 EditRepT where
	{
		equals1 r1 r2 = isJust (matchWitness r1 r2);
	};
}

