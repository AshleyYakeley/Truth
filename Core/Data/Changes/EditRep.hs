module Data.Changes.EditRep
(
    module Data.Changes.EditRep,
    Monoid(..)
) where
{
    import Data.TypeKT.IOWitnessKT;
    import Data.TypeKT.WitnessKT;
	import Data.Witness;
	import Data.OpenWitness;
	import Data.Maybe;
    import Control.Monad;
	import Data.Monoid;

    -- K and T represent kinds. T represents *, and Kxy represents x -> y.


    -- T

    data TypeT a = MkTypeT (InfoT a) (WitT a);

    instance SimpleWitness TypeT where
    {
        matchWitness (MkTypeT _ wa) (MkTypeT _ wb) = matchWitness wa wb;
    };

	data WitT a where
	{
		WitT :: IOWitness (SatT a) -> WitT a;
		TWitT :: TypeKTT p -> TypeT a -> WitT (p a);
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

    data InfoT a = MkInfoT
    {
        infoFactT :: forall f. IOWitness (SatKTT f) -> Maybe (f a)
    };

    instance Monoid (InfoT a) where
    {
        mempty = MkInfoT (\_ -> Nothing);
        mappend (MkInfoT f1) (MkInfoT f2) = MkInfoT (\w -> mplus (f1 w) (f2 w));
    };

    typeFactT :: IOWitness (SatKTT f) -> TypeT a -> Maybe (f a);
    typeFactT fact (MkTypeT info _) = infoFactT info fact;


    -- KTT

    data TypeKTT a = MkTypeKTT (InfoKTT a) (WitKTT a);

    instance WitnessKTT TypeKTT where
    {
        matchWitnessKTT (MkTypeKTT _ wa) (MkTypeKTT _ wb) = matchWitnessKTT wa wb;
    };

    applyTTypeT :: TypeKTT f -> TypeT a -> TypeT (f a);
    applyTTypeT tf@(MkTypeKTT inf _) ta = MkTypeT (deriveTInfoT inf ta) (TWitT tf ta);

	data WitKTT a where
	{
		WitKTT :: IOWitness (SatKTT a) -> WitKTT a;
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

    data InfoKTT a = MkInfoKTT
    {
        infoFactKTT :: forall f. IOWitness (SatKKTTT f) -> Maybe (f a),
        deriveTInfoT :: forall i. TypeT i -> InfoT (a i)
    };

    instance Monoid (InfoKTT a) where
    {
        mempty = MkInfoKTT (\_ -> Nothing) (\_ -> mempty);
        mappend (MkInfoKTT f1 d1) (MkInfoKTT f2 d2) = MkInfoKTT
         (\w -> mplus (f1 w) (f2 w))
         (\info -> mappend (d1 info) (d2 info));
    };

    typeFactKTT :: IOWitness (SatKKTTT f) -> TypeKTT a -> Maybe (f a);
    typeFactKTT fact (MkTypeKTT info _) = infoFactKTT info fact;

    mkInfoKTT :: IOWitness (SatKTT f) -> (forall i. TypeT i -> Maybe (f (a i))) -> InfoKTT a;
    mkInfoKTT wit f = MkInfoKTT (\_ -> Nothing) (\ta -> MkInfoT (\wit' -> do
    {
        MkEqualType <- matchWitnessT wit' wit;
        f ta;
    }));


    -- KTKTT

    data TypeKTKTT a = MkTypeKTKTT (InfoKTKTT a) (WitKTKTT a);

    instance WitnessKTKTT TypeKTKTT where
    {
        matchWitnessKTKTT (MkTypeKTKTT _ wa) (MkTypeKTKTT _ wb) = matchWitnessKTKTT wa wb;
    };

	data WitKTKTT p where
	{
		WitKTKTT :: IOWitness (SatKTKTT p) -> WitKTKTT p;
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

    data InfoKTKTT a = MkInfoKTKTT
    {
        infoFactKTKTT :: forall f. IOWitness (SatKKTKTTT f) -> Maybe (f a),
        deriveTInfoKTT :: forall i. TypeT i -> InfoKTT (a i)
    };

    instance Monoid (InfoKTKTT a) where
    {
        mempty = MkInfoKTKTT (\_ -> Nothing) (\_ -> mempty);
        mappend (MkInfoKTKTT f1 d1) (MkInfoKTKTT f2 d2) = MkInfoKTKTT
         (\w -> mplus (f1 w) (f2 w))
         (\info -> mappend (d1 info) (d2 info));
    };

    typeFactKTKTT :: IOWitness (SatKKTKTTT f) -> TypeKTKTT a -> Maybe (f a);
    typeFactKTKTT fact (MkTypeKTKTT info _) = infoFactKTKTT info fact;

    mkInfoKTKTT :: IOWitness (SatKTT f) -> (forall i1 i2. TypeT i1 -> TypeT i2 -> Maybe (f (a i1 i2))) -> InfoKTKTT a;
    mkInfoKTKTT wit f = MkInfoKTKTT (\_ -> Nothing) (\ta -> MkInfoKTT  (\_ -> Nothing) (\tb -> MkInfoT (\wit' -> do
    {
        MkEqualType <- matchWitnessT wit' wit;
        f ta tb;
    })));


    -- KKTTT

    data InfoKKTTT a = MkInfoKKTTT
    {
        infoFactKKTTT :: forall f. IOWitness (SatKKKTTTT f) -> Maybe (f a),
        deriveKTTInfoT :: forall i. TypeKTT i -> InfoT (a i)
    };

    instance Monoid (InfoKKTTT a) where
    {
        mempty = MkInfoKKTTT (\_ -> Nothing) (\_ -> mempty);
        mappend (MkInfoKKTTT f1 d1) (MkInfoKKTTT f2 d2) = MkInfoKKTTT
         (\w -> mplus (f1 w) (f2 w))
         (\info -> mappend (d1 info) (d2 info));
    };


    -- KKTTKTT

    data TypeKKTTKTT a = MkTypeKKTTKTT (InfoKKTTKTT a) (WitKKTTKTT a);

    instance WitnessKKTTKTT TypeKKTTKTT where
    {
        matchWitnessKKTTKTT (MkTypeKKTTKTT _ wa) (MkTypeKKTTKTT _ wb) = matchWitnessKKTTKTT wa wb;
    };

	data WitKKTTKTT p where
	{
		WitKKTTKTT :: IOWitness (SatKKTTKTT p) -> WitKKTTKTT p;
	};

    instance WitnessKKTTKTT WitKKTTKTT where
    {
    	matchWitnessKKTTKTT (WitKKTTKTT wa) (WitKKTTKTT wb) = matchWitnessT wa wb;
	};

    data InfoKKTTKTT a = MkInfoKKTTKTT
    {
        infoFactKKTTKTT :: forall f. IOWitness (SatKKKTTKTTT f) -> Maybe (f a),
        deriveKTTInfoKTT :: forall i. TypeKTT i -> InfoKTT (a i)
    };

    instance Monoid (InfoKKTTKTT a) where
    {
        mempty = MkInfoKKTTKTT (\_ -> Nothing) (\_ -> mempty);
        mappend (MkInfoKKTTKTT f1 d1) (MkInfoKKTTKTT f2 d2) = MkInfoKKTTKTT
         (\w -> mplus (f1 w) (f2 w))
         (\info -> mappend (d1 info) (d2 info));
    };

    typeFactKKTTKTT :: IOWitness (SatKKKTTKTTT f) -> TypeKKTTKTT a -> Maybe (f a);
    typeFactKKTTKTT fact (MkTypeKKTTKTT info _) = infoFactKKTTKTT info fact;

    mkInfoKKTTKTT :: IOWitness (SatKTT f) -> (forall i1 i2. TypeKTT i1 -> TypeT i2 -> Maybe (f (a i1 i2))) -> InfoKKTTKTT a;
    mkInfoKKTTKTT wit f = MkInfoKKTTKTT (\_ -> Nothing) (\ta -> MkInfoKTT  (\_ -> Nothing) (\tb -> MkInfoT (\wit' -> do
    {
        MkEqualType <- matchWitnessT wit' wit;
        f ta tb;
    })));


    -- KTKTKTT

    data TypeKTKTKTT a = MkTypeKTKTKTT (InfoKTKTKTT a) (WitKTKTKTT a);

    instance WitnessKTKTKTT TypeKTKTKTT where
    {
        matchWitnessKTKTKTT (MkTypeKTKTKTT _ wa) (MkTypeKTKTKTT _ wb) = matchWitnessKTKTKTT wa wb;
    };

	data WitKTKTKTT p where
	{
		WitKTKTKTT :: IOWitness (SatKTKTKTT p) -> WitKTKTKTT p;
	};

    instance WitnessKTKTKTT WitKTKTKTT where
    {
	    matchWitnessKTKTKTT (WitKTKTKTT wa) (WitKTKTKTT wb) = matchWitnessT wa wb;
	};

    data InfoKTKTKTT a = MkInfoKTKTKTT
    {
        infoFactKTKTKTT :: forall f. IOWitness (SatKKTKTKTTT f) -> Maybe (f a),
        deriveTInfoKTKTT :: forall i. TypeT i -> InfoKTKTT (a i)
    };

    instance Monoid (InfoKTKTKTT a) where
    {
        mempty = MkInfoKTKTKTT (\_ -> Nothing) (\_ -> mempty);
        mappend (MkInfoKTKTKTT f1 d1) (MkInfoKTKTKTT f2 d2) = MkInfoKTKTKTT
         (\w -> mplus (f1 w) (f2 w))
         (\info -> mappend (d1 info) (d2 info));
    };

    typeFactKTKTKTT :: IOWitness (SatKKTKTKTTT f) -> TypeKTKTKTT a -> Maybe (f a);
    typeFactKTKTKTT fact (MkTypeKTKTKTT info _) = infoFactKTKTKTT info fact;
}

