module Data.TypeKT.Type
(
    module Data.TypeKT.Type,
    Monoid(..)
) where
{
    import Data.TypeKT.Basic;
    import Data.TypeKT.Witness;
	import Data.Witness;
	import Data.OpenWitness;
	import Data.Maybe;
    import Control.Monad;
	import Data.Monoid;

    -- K and T represent kinds. T represents *, and Kxy represents x -> y.


    -- T

    data TypeT a = MkTypeT (WitT a) (InfoT a);

    instance SimpleWitness TypeT where
    {
        matchWitness (MkTypeT wa _) (MkTypeT wb _) = matchWitness wa wb;
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

    class TypeFactT f where
    {
        witFactT :: IOWitness (SatKTT f);
    };

    typeFactT :: (TypeFactT f) => TypeT a -> Maybe (f a);
    typeFactT (MkTypeT _ info) = infoFactT info witFactT;

    mkTInfoT_ :: IOWitness (SatKTT f) -> Maybe (f a) -> InfoT a;
    mkTInfoT_ witF f = MkInfoT (\wit -> do
    {
        MkEqualType <- matchWitnessT wit witF;
        f;
    });

    mkTInfoT :: forall f a. (TypeFactT f) => Maybe (f a) -> InfoT a;
    mkTInfoT = mkTInfoT_ witFactT;


    -- KTT

    data TypeKTT a = MkTypeKTT (WitKTT a) (InfoKTT a);

    instance WitnessKTT TypeKTT where
    {
        matchWitnessKTT (MkTypeKTT wa _) (MkTypeKTT wb _) = matchWitnessKTT wa wb;
    };

    applyTTypeT :: TypeKTT f -> TypeT a -> TypeT (f a);
    applyTTypeT tf@(MkTypeKTT _ inf) ta = MkTypeT (TWitT tf ta) (deriveTInfoT inf ta);

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

    class TypeFactKTT f where
    {
        witFactKTT :: IOWitness (SatKKTTT f);
    };

    typeFactKTT :: (TypeFactKTT f) => TypeKTT a -> Maybe (f a);
    typeFactKTT (MkTypeKTT _ info) = infoFactKTT info witFactKTT;

    mkTInfoKTT_ :: IOWitness (SatKTT f) -> (forall i. TypeT i -> Maybe (f (a i))) -> InfoKTT a;
    mkTInfoKTT_ witF f = MkInfoKTT (\_ -> Nothing) (mkTInfoT_ witF . f);

    mkTInfoKTT :: forall f a. (TypeFactT f) => (forall i. TypeT i -> Maybe (f (a i))) -> InfoKTT a;
    mkTInfoKTT f = MkInfoKTT (\_ -> Nothing) (mkTInfoT . f);

    mkKTTInfoKTT_ :: IOWitness (SatKKTTT f) -> Maybe (f a) -> InfoKTT a;
    mkKTTInfoKTT_ witF f = MkInfoKTT (\wit -> do
    {
        MkEqualType <- matchWitnessT wit witF;
        f;
    }) (\_ -> mempty);

    mkKTTInfoKTT :: forall f a. (TypeFactKTT f) => Maybe (f a) -> InfoKTT a;
    mkKTTInfoKTT = mkKTTInfoKTT_ witFactKTT;


    -- KTKTT

    data TypeKTKTT a = MkTypeKTKTT (WitKTKTT a) (InfoKTKTT a);

    instance WitnessKTKTT TypeKTKTT where
    {
        matchWitnessKTKTT (MkTypeKTKTT wa _) (MkTypeKTKTT wb _) = matchWitnessKTKTT wa wb;
    };

    applyTTypeKTT :: TypeKTKTT f -> TypeT a -> TypeKTT (f a);
    applyTTypeKTT tf@(MkTypeKTKTT _ inf) ta = MkTypeKTT (TWitKTT tf ta) (deriveTInfoKTT inf ta);

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

    class TypeFactKTKTT f where
    {
        witFactKTKTT :: IOWitness (SatKKTKTTT f);
    };

    typeFactKTKTT :: (TypeFactKTKTT f) => TypeKTKTT a -> Maybe (f a);
    typeFactKTKTT (MkTypeKTKTT _ info) = infoFactKTKTT info witFactKTKTT;

    mkTInfoKTKTT_ :: IOWitness (SatKTT f) -> (forall i1 i2. TypeT i1 -> TypeT i2 -> Maybe (f (a i1 i2))) -> InfoKTKTT a;
    mkTInfoKTKTT_ witF f = MkInfoKTKTT (\_ -> Nothing) (\ta -> mkTInfoKTT_ witF (f ta));

    mkTInfoKTKTT :: forall f a. (TypeFactT f) => (forall i1 i2. TypeT i1 -> TypeT i2 -> Maybe (f (a i1 i2))) -> InfoKTKTT a;
    mkTInfoKTKTT = mkTInfoKTKTT_ witFactT;

    mkKTTInfoKTKTT_ :: IOWitness (SatKKTTT f) -> (forall i. TypeT i -> Maybe (f (a i))) -> InfoKTKTT a;
    mkKTTInfoKTKTT_ witF f = MkInfoKTKTT (\_ -> Nothing) (mkKTTInfoKTT_ witF . f);

    mkKTTInfoKTKTT :: forall f a. (TypeFactKTT f) => (forall i. TypeT i -> Maybe (f (a i))) -> InfoKTKTT a;
    mkKTTInfoKTKTT = mkKTTInfoKTKTT_ witFactKTT;


    -- KKTTT

    -- add it if you need it

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

    data TypeKKTTKTT a = MkTypeKKTTKTT (WitKKTTKTT a) (InfoKKTTKTT a);

    instance WitnessKKTTKTT TypeKKTTKTT where
    {
        matchWitnessKKTTKTT (MkTypeKKTTKTT wa _) (MkTypeKKTTKTT wb _) = matchWitnessKKTTKTT wa wb;
    };

    applyKTTTypeKTT :: TypeKKTTKTT f -> TypeKTT a -> TypeKTT (f a);
    applyKTTTypeKTT tf@(MkTypeKKTTKTT _ inf) ta = MkTypeKTT (KTTWitKTT tf ta) (deriveKTTInfoKTT inf ta);

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

    class TypeFactKKTTKTT f where
    {
        witFactKKTTKTT :: IOWitness (SatKKKTTKTTT f);
    };

    typeFactKKTTKTT :: (TypeFactKKTTKTT f) => TypeKKTTKTT a -> Maybe (f a);
    typeFactKKTTKTT (MkTypeKKTTKTT _ info) = infoFactKKTTKTT info witFactKKTTKTT;

    mkTInfoKKTTKTT_ :: IOWitness (SatKTT f) -> (forall i1 i2. TypeKTT i1 -> TypeT i2 -> Maybe (f (a i1 i2))) -> InfoKKTTKTT a;
    mkTInfoKKTTKTT_ witF f = MkInfoKKTTKTT (\_ -> Nothing) (\ta -> mkTInfoKTT_ witF (f ta));

    mkTInfoKKTTKTT :: forall f a. (TypeFactT f) => (forall i1 i2. TypeKTT i1 -> TypeT i2 -> Maybe (f (a i1 i2))) -> InfoKKTTKTT a;
    mkTInfoKKTTKTT = mkTInfoKKTTKTT_ witFactT;


    -- KTKTKTT

    data TypeKTKTKTT a = MkTypeKTKTKTT (WitKTKTKTT a) (InfoKTKTKTT a);

    instance WitnessKTKTKTT TypeKTKTKTT where
    {
        matchWitnessKTKTKTT (MkTypeKTKTKTT wa _) (MkTypeKTKTKTT wb _) = matchWitnessKTKTKTT wa wb;
    };

    applyTTypeKTKTT :: TypeKTKTKTT f -> TypeT a -> TypeKTKTT (f a);
    applyTTypeKTKTT tf@(MkTypeKTKTKTT _ inf) ta = MkTypeKTKTT (TWitKTKTT tf ta) (deriveTInfoKTKTT inf ta);

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

    class TypeFactKTKTKTT f where
    {
        witFactKTKTKTT :: IOWitness (SatKKTKTKTTT f);
    };

    typeFactKTKTKTT :: (TypeFactKTKTKTT f) => TypeKTKTKTT a -> Maybe (f a);
    typeFactKTKTKTT (MkTypeKTKTKTT _ info) = infoFactKTKTKTT info witFactKTKTKTT;

    mkTInfoKTKTKTT_ :: IOWitness (SatKTT f) -> (forall i1 i2 i3. TypeT i1 -> TypeT i2 -> TypeT i3 -> Maybe (f (a i1 i2 i3))) -> InfoKTKTKTT a;
    mkTInfoKTKTKTT_ witF f = MkInfoKTKTKTT (\_ -> Nothing) (\t1 -> mkTInfoKTKTT_ witF (f t1));

    mkTInfoKTKTKTT :: forall f a. (TypeFactT f) => (forall i1 i2 i3. TypeT i1 -> TypeT i2 -> TypeT i3 -> Maybe (f (a i1 i2 i3))) -> InfoKTKTKTT a;
    mkTInfoKTKTKTT = mkTInfoKTKTKTT_ witFactT;
}

