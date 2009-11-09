module Data.Changes.EditRep where
{
    import Data.TypeKT.IOWitnessKT;
    import Data.TypeKT.WitnessKT;
	import Data.Witness;
	import Data.OpenWitness;
	import Data.Maybe;

    -- K and T represent kinds. T represents *, and Kxy represents x -> y.

    class IsApplyWit f x where
    {
        type ApplyWit f x;

        applyWit :: f -> x -> ApplyWit f x;
        matchApplyWit :: ApplyWit f x -> Maybe (f,x);
    };

    class IsAtomWit rep where
    {
        type RepT rep;

        witnessWit :: IOWitness (RepT rep) -> rep;
        matchWitnessWit :: rep -> Maybe (IOWitness (RepT rep));
    };


    -- KTKTKTT

    data FactKTKTKTT a where
    {
        MkFactKTKTKTT :: forall f a. IOWitnessKTKTKTKTT f -> f a -> FactKTKTKTT a;
    };

    data TypeKTKTKTT a = MkTypeKTKTKTT [FactKTKTKTT a] (WitKTKTKTT a);

    instance WitnessKTKTKTT TypeKTKTKTT where
    {
        matchWitnessKTKTKTT (MkTypeKTKTKTT wa) (MkTypeKTKTKTT wb) = matchWitnessKTKTKTT wa wb;
    };

	data WitKTKTKTT p where
	{
		WitKTKTKTT :: IOWitnessKTKTKTT p -> WitKTKTKTT p;
	};

    instance WitnessKTKTKTT WitKTKTKTT where
    {
	    matchWitnessKTKTKTT (WitKTKTKTT wa) (WitKTKTKTT wb) = matchWitnessT wa wb;
	};

    instance IsAtomWit (WitKTKTKTT p) where
    {
        type RepT (WitKTKTKTT p) = p () () ();
        witnessWit = WitKTKTKTT;
        matchWitnessWit (WitKTKTKTT wit) = Just wit;
    };


    -- KTKTT

    data FactKTKTT a where
    {
        forall f a. IOWitnessKTKTKTT f -> f a -> FactKTKTT a;
    };

    data TypeKTKTT a = MkRepKTKTT [FactKTKTT a] (WitKTKTT a);

	data WitKTKTT p where
	{
		WitKTKTT :: IOWitnessKTKTT p -> WitKTKTT p;
		TWitKTKTT :: TypeKTKTKTT p -> TypeT a -> WitKTKTT (p a);
	};

    instance WitnessKTKTT WitKTKTT where
    {
	    matchWitnessKTKTT (WitKTKTT wa) (WitKTKTT wb) = matchWitnessT wa wb;
	    matchWitnessKTKTT (TWitKTKTT tfa ta) (TWitKTKTT tfb tb) = do
	    {
		    MkEqualType <- matchWitnessKTKTKTT tfa tfb;
		    MkEqualType <- matchWitnessT ta tb;
		    return MkEqualType;
	    };
	    matchWitnessKTKTT _ _ = Nothing;
	};

    instance IsAtomWit (WitKTKTT p) where
    {
        type RepT (WitKTKTT p) = p () ();
        witnessWit = WitKTKTT;
        matchWitnessWit (WitKTKTT wit) = Just wit;
        matchWitnessWit _ = Nothing;
    };

    instance IsApplyWit (WitKTKTKTT p) (WitT a) where
    {
        type ApplyWit (WitKTKTKTT p) (WitT a) = WitKTKTT (p a);
        applyWit = TWitKTKTT;
        matchApplyWit (TWitKTKTT p a) = Just (p,a);
        matchApplyWit _ = Nothing;
    };


    -- KKTTKTT

    data FactKKTTKTT a where
    {
        forall f a. IOWitnessKTKKTTKTT f -> f a -> FactKKTTKTT a;
    };

    data TypeKKTTKTT a = MkRepKKTTKTT [FactKKTTKTT a] (WitKKTTKTT a);

	data WitKKTTKTT p where
	{
		WitKKTTKTT :: IOWitness (p Maybe ()) -> WitKKTTKTT p;
	};

    instance WitnessKKTTKTT WitKKTTKTT where
    {
    	matchWitnessKKTTKTT (WitKKTTKTT wa) (WitKKTTKTT wb) = matchWitnessT wa wb;
	};

    instance IsAtomWit (WitKKTTKTT p) where
    {
        type RepT (WitKKTTKTT p) = p Maybe ();
        witnessWit = WitKKTTKTT;
        matchWitnessWit (WitKKTTKTT wit) = Just wit;
    };


    -- KTT

    data FactKTT a where
    {
        forall f a. IOWitnessKTKTT f -> f a -> FactKTT a;
    };

    data TypeKTT a = MkRepKTT [FactKTT a] (WitKTT a);

	data WitKTT a where
	{
		WitKTT :: IOWitnessKTT a -> WitKTT a;
		TWitKTT :: TypeKTKTT f -> TypeT a -> WitKTT (f a);
		KTTWitKTT :: TypeKKTTKTT f -> TypeKTT a -> WitKTT (f a);
	};

    instance WitnessKTT WitKTT where
    {
	    matchWitnessKTT (WitKTT wa) (WitKTT wb) = matchWitness wa wb;
	    matchWitnessKTT (TWitKTT tfa ta) (TWitKTT tfb tb) = do
	    {
		    MkEqualType <- matchWitnessKTKTT tfa tfb;
		    MkEqualType <- matchWitnessT ta tb;
		    return MkEqualType;
	    };
	    matchWitnessKTT (KTTWitKTT tfa ta) (KTTWitKTT tfb tb) = do
	    {
		    MkEqualType <- matchWitnessKKTTKTT tfa tfb;
		    MkEqualType <- matchWitnessKTT ta tb;
		    return MkEqualType;
	    };
	    matchWitnessKTT _ _ = Nothing;
    };

    instance IsAtomWit (WitKTT p) where
    {
        type RepT (WitKTT p) = p ();
        witnessWit = WitKTT;
        matchWitnessWit (WitKTT wit) = Just wit;
        matchWitnessWit _ = Nothing;
    };

    instance IsApplyWit (WitKTKTT p) (WitT a) where
    {
        type ApplyWit (WitKTKTT p) (WitT a) = WitKTT (p a);
        applyWit = TWitKTT;
        matchApplyWit (TWitKTT p a) = Just (p,a);
        matchApplyWit _ = Nothing;
    };

    instance IsApplyWit (WitKKTTKTT p) (WitKTT a) where
    {
        type ApplyWit (WitKKTTKTT p) (WitKTT a) = WitKTT (p a);
        applyWit = KTTWitKTT;
        matchApplyWit (KTTWitKTT p a) = Just (p,a);
        matchApplyWit _ = Nothing;
    };


    -- T

    data FactT a where
    {
        forall f a. IOWitnessKTT f -> f a -> FactT a;
    };

    data TypeT a = MkRepT [FactT a] (WitT a);

	data WitT a where
	{
		WitT :: IOWitnessT a -> WitT a;
		TWitT :: TypeKTT p -> TypeT a -> WitT (p a);
	};

    instance IsAtomWit (WitT p) where
    {
        type RepT (WitT p) = p;
        witnessWit = WitT;
        matchWitnessWit (WitT wit) = Just wit;
        matchWitnessWit _ = Nothing;
    };

    instance IsApplyWit (WitKTT p) (WitT a) where
    {
        type ApplyWit (WitKTT p) (WitT a) = WitT (p a);
        applyWit = TWitT;
        matchApplyWit (TWitT p a) = Just (p,a);
        matchApplyWit _ = Nothing;
    };

	instance SimpleWitness WitT where
	{
		matchWitness (WitT wa) (WitT wb) = matchWitness wa wb;
		matchWitness (TWitT tfa ta) (TWitT tfb tb) = do
		{
			MkEqualType <- matchWitnessKTT tfa tfb;
			MkEqualType <- matchWitnessT ta tb;
			return MkEqualType;
		};
		matchWitness _ _ = Nothing;
	};

	instance Eq1 WitT where
	{
		equals1 r1 r2 = isJust (matchWitness r1 r2);
	};
}

