module Data.Changes.EditRep where
{
	import Data.Witness;
	import Data.OpenWitness;
	import Data.Maybe;

    -- K and T represent kinds. T represents *, and Kxy represents x -> y.

    class IsApplyEditRep f x where
    {
        type ApplyEditRep f x;

        applyEditRep :: f -> x -> ApplyEditRep f x;
        matchApplyEditRep :: ApplyEditRep f x -> Maybe (f,x);
    };

    class IsWitnessEditRep rep where
    {
        type RepT rep;

        witnessEditRep :: IOWitness (RepT rep) -> rep;
        matchWitnessEditRep :: rep -> Maybe (IOWitness (RepT rep));
    };

	data EditRepKTKTT p where
	{
		EditRepKTKTT :: IOWitness (p () ()) -> EditRepKTKTT p;
	};

	matchEditRepKTKTT :: EditRepKTKTT a -> EditRepKTKTT b -> Maybe (EqualType (a () ()) (b () ()));
	matchEditRepKTKTT (EditRepKTKTT wa) (EditRepKTKTT wb) = matchWitness wa wb;

    instance IsWitnessEditRep (EditRepKTKTT p) where
    {
        type RepT (EditRepKTKTT p) = p () ();
        witnessEditRep = EditRepKTKTT;
        matchWitnessEditRep (EditRepKTKTT wit) = Just wit;
    };

	data EditRepKKTTKTT p where
	{
		EditRepKKTTKTT :: IOWitness (p EditRepT ()) -> EditRepKKTTKTT p;
	};

	matchEditRepKKTTKTT :: EditRepKKTTKTT a -> EditRepKKTTKTT b -> Maybe (EqualType (a EditRepT ()) (b EditRepT ()));
	matchEditRepKKTTKTT (EditRepKKTTKTT wa) (EditRepKKTTKTT wb) = matchWitness wa wb;

    instance IsWitnessEditRep (EditRepKKTTKTT p) where
    {
        type RepT (EditRepKKTTKTT p) = p EditRepT ();
        witnessEditRep = EditRepKKTTKTT;
        matchWitnessEditRep (EditRepKKTTKTT wit) = Just wit;
    };

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

    instance IsWitnessEditRep (EditRepKTT p) where
    {
        type RepT (EditRepKTT p) = p ();
        witnessEditRep = EditRepKTT;
        matchWitnessEditRep (EditRepKTT wit) = Just wit;
        matchWitnessEditRep _ = Nothing;
    };

    instance IsApplyEditRep (EditRepKTKTT p) (EditRepT a) where
    {
        type ApplyEditRep (EditRepKTKTT p) (EditRepT a) = EditRepKTT (p a);
        applyEditRep = TEditRepKTT;
        matchApplyEditRep (TEditRepKTT p a) = Just (p,a);
        matchApplyEditRep _ = Nothing;
    };

    instance IsApplyEditRep (EditRepKKTTKTT p) (EditRepKTT a) where
    {
        type ApplyEditRep (EditRepKKTTKTT p) (EditRepKTT a) = EditRepKTT (p a);
        applyEditRep = KTTEditRepKTT;
        matchApplyEditRep (KTTEditRepKTT p a) = Just (p,a);
        matchApplyEditRep _ = Nothing;
    };

	data EditRepT a where
	{
		EditRepT :: IOWitness a -> EditRepT a;
		TEditRepT :: EditRepKTT p -> EditRepT a -> EditRepT (p a);
	};

    instance IsWitnessEditRep (EditRepT p) where
    {
        type RepT (EditRepT p) = p;
        witnessEditRep = EditRepT;
        matchWitnessEditRep (EditRepT wit) = Just wit;
        matchWitnessEditRep _ = Nothing;
    };

    instance IsApplyEditRep (EditRepKTT p) (EditRepT a) where
    {
        type ApplyEditRep (EditRepKTT p) (EditRepT a) = EditRepT (p a);
        applyEditRep = TEditRepT;
        matchApplyEditRep (TEditRepT p a) = Just (p,a);
        matchApplyEditRep _ = Nothing;
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

