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

    data InfoT a = MkInfoT (WitT a) (FactsT a);

    instance SimpleWitness InfoT where
    {
        matchWitness (MkInfoT wa _) (MkInfoT wb _) = matchWitness wa wb;
    };

	data WitT a where
	{
		WitT :: IOWitness (SatT a) -> WitT a;
		TWitT :: InfoKTT p -> InfoT a -> WitT (p a);
		KTTWitT :: InfoKKTTT p -> InfoKTT a -> WitT (p a);
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
		matchWitness (KTTWitT tfa ta) (KTTWitT tfb tb) = do
		{
			MkEqualType <- matchWitnessKKTTT tfa tfb;
			MkEqualType <- matchWitnessKTT ta tb;
			return MkEqualType;
		};
		matchWitness _ _ = Nothing;
	};

	instance Eq1 WitT where
	{
		equals1 r1 r2 = isJust (matchWitness r1 r2);
	};

    class PropertyT f where
    {
        matchPropertyT :: forall t. InfoT t -> Maybe (f t);
    };

    matchPropertyT_ :: (PropertyT f) => Type (f FT) -> InfoT t -> Maybe (f t);
    matchPropertyT_ _ = matchPropertyT;

    data FactsT a = MkFactsT
    {
        infoFactT :: forall f. IOWitness (SatKTT f) -> Maybe (f a)
    };

    instance Monoid (FactsT a) where
    {
        mempty = MkFactsT (\_ -> Nothing);
        mappend (MkFactsT f1) (MkFactsT f2) = MkFactsT (\w -> mplus (f1 w) (f2 w));
    };

    class (PropertyT f) => FactT f where
    {
        witFactT :: IOWitness (SatKTT f);
    };

    matchPropertyT_Fact :: (FactT f) => InfoT a -> Maybe (f a);
    matchPropertyT_Fact (MkInfoT _ info) = infoFactT info witFactT;

    mkTFactsT_ :: IOWitness (SatKTT f) -> Maybe (f a) -> FactsT a;
    mkTFactsT_ witF f = MkFactsT (\wit -> do
    {
        MkEqualType <- matchWitnessT wit witF;
        f;
    });

    mkTFactsT :: forall f a. (FactT f) => Maybe (f a) -> FactsT a;
    mkTFactsT = mkTFactsT_ witFactT;


    -- KTT

    data InfoKTT a = MkInfoKTT (WitKTT a) (FactsKTT a);

    instance WitnessKTT InfoKTT where
    {
        matchWitnessKTT (MkInfoKTT wa _) (MkInfoKTT wb _) = matchWitnessKTT wa wb;
    };

	data WitKTT a where
	{
		WitKTT :: IOWitness (SatKTT a) -> WitKTT a;
		TWitKTT :: InfoKTKTT f -> InfoT a -> WitKTT (f a);
		KTTWitKTT :: InfoKKTTKTT f -> InfoKTT a -> WitKTT (f a);
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

    class PropertyKTT f where
    {
        matchPropertyKTT :: forall t. InfoKTT t -> Maybe (f t);
    };

    matchPropertyKTT_ :: (PropertyKTT f) => Type (f FKTT) -> InfoKTT t -> Maybe (f t);
    matchPropertyKTT_ _ = matchPropertyKTT;

    data FactsKTT a = MkFactsKTT
    {
        infoFactKTT :: forall f. IOWitness (SatKKTTT f) -> Maybe (f a),
        deriveTFactsT :: forall i. InfoT i -> FactsT (a i)
    };

    instance Monoid (FactsKTT a) where
    {
        mempty = MkFactsKTT (\_ -> Nothing) (\_ -> mempty);
        mappend (MkFactsKTT f1 d1) (MkFactsKTT f2 d2) = MkFactsKTT
         (\w -> mplus (f1 w) (f2 w))
         (\info -> mappend (d1 info) (d2 info));
    };

    class (PropertyKTT f) => FactKTT f where
    {
        witFactKTT :: IOWitness (SatKKTTT f);
    };

    matchPropertyKTT_Fact :: (FactKTT f) => InfoKTT a -> Maybe (f a);
    matchPropertyKTT_Fact (MkInfoKTT _ info) = infoFactKTT info witFactKTT;

    mkTFactsKTT_ :: IOWitness (SatKTT f) -> (forall i. InfoT i -> Maybe (f (a i))) -> FactsKTT a;
    mkTFactsKTT_ witF f = MkFactsKTT (\_ -> Nothing) (mkTFactsT_ witF . f);

    mkTFactsKTT :: forall f a. (FactT f) => (forall i. InfoT i -> Maybe (f (a i))) -> FactsKTT a;
    mkTFactsKTT f = MkFactsKTT (\_ -> Nothing) (mkTFactsT . f);

    mkKTTFactsKTT_ :: IOWitness (SatKKTTT f) -> Maybe (f a) -> FactsKTT a;
    mkKTTFactsKTT_ witF f = MkFactsKTT (\wit -> do
    {
        MkEqualType <- matchWitnessT wit witF;
        f;
    }) (\_ -> mempty);

    mkKTTFactsKTT :: forall f a. (FactKTT f) => Maybe (f a) -> FactsKTT a;
    mkKTTFactsKTT = mkKTTFactsKTT_ witFactKTT;


    -- KTKTT

    data InfoKTKTT a = MkInfoKTKTT (WitKTKTT a) (FactsKTKTT a);

    instance WitnessKTKTT InfoKTKTT where
    {
        matchWitnessKTKTT (MkInfoKTKTT wa _) (MkInfoKTKTT wb _) = matchWitnessKTKTT wa wb;
    };

	data WitKTKTT p where
	{
		WitKTKTT :: IOWitness (SatKTKTT p) -> WitKTKTT p;
		TWitKTKTT :: InfoKTKTKTT p -> InfoT a -> WitKTKTT (p a);
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

    class PropertyKTKTT f where
    {
        matchPropertyKTKTT :: forall t. InfoKTKTT t -> Maybe (f t);
    };

    matchPropertyKTKTT_ :: (PropertyKTKTT f) => Type (f FKTKTT) -> InfoKTKTT t -> Maybe (f t);
    matchPropertyKTKTT_ _ = matchPropertyKTKTT;

    data FactsKTKTT a = MkFactsKTKTT
    {
        infoFactKTKTT :: forall f. IOWitness (SatKKTKTTT f) -> Maybe (f a),
        deriveTFactsKTT :: forall i. InfoT i -> FactsKTT (a i)
    };

    instance Monoid (FactsKTKTT a) where
    {
        mempty = MkFactsKTKTT (\_ -> Nothing) (\_ -> mempty);
        mappend (MkFactsKTKTT f1 d1) (MkFactsKTKTT f2 d2) = MkFactsKTKTT
         (\w -> mplus (f1 w) (f2 w))
         (\info -> mappend (d1 info) (d2 info));
    };

    class (PropertyKTKTT f) => FactKTKTT f where
    {
        witFactKTKTT :: IOWitness (SatKKTKTTT f);
    };

    matchPropertyKTKTT_Fact :: (FactKTKTT f) => InfoKTKTT a -> Maybe (f a);
    matchPropertyKTKTT_Fact (MkInfoKTKTT _ info) = infoFactKTKTT info witFactKTKTT;

    mkTFactsKTKTT_ :: IOWitness (SatKTT f) -> (forall i1 i2. InfoT i1 -> InfoT i2 -> Maybe (f (a i1 i2))) -> FactsKTKTT a;
    mkTFactsKTKTT_ witF f = MkFactsKTKTT (\_ -> Nothing) (\ta -> mkTFactsKTT_ witF (f ta));

    mkTFactsKTKTT :: forall f a. (FactT f) => (forall i1 i2. InfoT i1 -> InfoT i2 -> Maybe (f (a i1 i2))) -> FactsKTKTT a;
    mkTFactsKTKTT = mkTFactsKTKTT_ witFactT;

    mkKTTFactsKTKTT_ :: IOWitness (SatKKTTT f) -> (forall i. InfoT i -> Maybe (f (a i))) -> FactsKTKTT a;
    mkKTTFactsKTKTT_ witF f = MkFactsKTKTT (\_ -> Nothing) (mkKTTFactsKTT_ witF . f);

    mkKTTFactsKTKTT :: forall f a. (FactKTT f) => (forall i. InfoT i -> Maybe (f (a i))) -> FactsKTKTT a;
    mkKTTFactsKTKTT = mkKTTFactsKTKTT_ witFactKTT;


    -- KKTTT

    data InfoKKTTT a = MkInfoKKTTT (WitKKTTT a) (FactsKKTTT a);

    instance WitnessKKTTT InfoKKTTT where
    {
        matchWitnessKKTTT (MkInfoKKTTT wa _) (MkInfoKKTTT wb _) = matchWitnessKKTTT wa wb;
    };

	data WitKKTTT p where
	{
		WitKKTTT :: IOWitness (SatKKTTT p) -> WitKKTTT p;
	};

    instance WitnessKKTTT WitKKTTT where
    {
    	matchWitnessKKTTT (WitKKTTT wa) (WitKKTTT wb) = matchWitnessT wa wb;
	};

    class PropertyKKTTT f where
    {
        matchPropertyKKTTT :: forall t. InfoKKTTT t -> Maybe (f t);
    };

    matchPropertyKKTTT_ :: (PropertyKKTTT f) => Type (f FKKTTT) -> InfoKKTTT t -> Maybe (f t);
    matchPropertyKKTTT_ _ = matchPropertyKKTTT;

    data FactsKKTTT a = MkFactsKKTTT
    {
        infoFactKKTTT :: forall f. IOWitness (SatKKKTTTT f) -> Maybe (f a),
        deriveKTTFactsT :: forall i. InfoKTT i -> FactsT (a i)
    };

    instance Monoid (FactsKKTTT a) where
    {
        mempty = MkFactsKKTTT (\_ -> Nothing) (\_ -> mempty);
        mappend (MkFactsKKTTT f1 d1) (MkFactsKKTTT f2 d2) = MkFactsKKTTT
         (\w -> mplus (f1 w) (f2 w))
         (\info -> mappend (d1 info) (d2 info));
    };

    class (PropertyKKTTT f) => FactKKTTT f where
    {
        witFactKKTTT :: IOWitness (SatKKKTTTT f);
    };

    matchPropertyKKTTT_Fact :: (FactKKTTT f) => InfoKKTTT a -> Maybe (f a);
    matchPropertyKKTTT_Fact (MkInfoKKTTT _ info) = infoFactKKTTT info witFactKKTTT;

    mkTFactsKKTTT_ :: IOWitness (SatKTT f) -> (forall i1. InfoKTT i1 -> Maybe (f (a i1))) -> FactsKKTTT a;
    mkTFactsKKTTT_ witF f = MkFactsKKTTT (\_ -> Nothing) (\ta -> mkTFactsT_ witF (f ta));

    mkTFactsKKTTT :: forall f a. (FactT f) => (forall i1. InfoKTT i1 -> Maybe (f (a i1))) -> FactsKKTTT a;
    mkTFactsKKTTT = mkTFactsKKTTT_ witFactT;


    -- KKTTKTT

    data InfoKKTTKTT a = MkInfoKKTTKTT (WitKKTTKTT a) (FactsKKTTKTT a);

    instance WitnessKKTTKTT InfoKKTTKTT where
    {
        matchWitnessKKTTKTT (MkInfoKKTTKTT wa _) (MkInfoKKTTKTT wb _) = matchWitnessKKTTKTT wa wb;
    };

	data WitKKTTKTT p where
	{
		WitKKTTKTT :: IOWitness (SatKKTTKTT p) -> WitKKTTKTT p;
	};

    instance WitnessKKTTKTT WitKKTTKTT where
    {
    	matchWitnessKKTTKTT (WitKKTTKTT wa) (WitKKTTKTT wb) = matchWitnessT wa wb;
	};

    data FactsKKTTKTT a = MkFactsKKTTKTT
    {
        infoFactKKTTKTT :: forall f. IOWitness (SatKKKTTKTTT f) -> Maybe (f a),
        deriveKTTFactsKTT :: forall i. InfoKTT i -> FactsKTT (a i)
    };

    instance Monoid (FactsKKTTKTT a) where
    {
        mempty = MkFactsKKTTKTT (\_ -> Nothing) (\_ -> mempty);
        mappend (MkFactsKKTTKTT f1 d1) (MkFactsKKTTKTT f2 d2) = MkFactsKKTTKTT
         (\w -> mplus (f1 w) (f2 w))
         (\info -> mappend (d1 info) (d2 info));
    };

    class FactKKTTKTT f where
    {
        witFactKKTTKTT :: IOWitness (SatKKKTTKTTT f);
    };

    matchPropertyKKTTKTT :: (FactKKTTKTT f) => InfoKKTTKTT a -> Maybe (f a);
    matchPropertyKKTTKTT (MkInfoKKTTKTT _ info) = infoFactKKTTKTT info witFactKKTTKTT;

    mkTFactsKKTTKTT_ :: IOWitness (SatKTT f) -> (forall i1 i2. InfoKTT i1 -> InfoT i2 -> Maybe (f (a i1 i2))) -> FactsKKTTKTT a;
    mkTFactsKKTTKTT_ witF f = MkFactsKKTTKTT (\_ -> Nothing) (\ta -> mkTFactsKTT_ witF (f ta));

    mkTFactsKKTTKTT :: forall f a. (FactT f) => (forall i1 i2. InfoKTT i1 -> InfoT i2 -> Maybe (f (a i1 i2))) -> FactsKKTTKTT a;
    mkTFactsKKTTKTT = mkTFactsKKTTKTT_ witFactT;


    -- KTKTKTT

    data InfoKTKTKTT a = MkInfoKTKTKTT (WitKTKTKTT a) (FactsKTKTKTT a);

    instance WitnessKTKTKTT InfoKTKTKTT where
    {
        matchWitnessKTKTKTT (MkInfoKTKTKTT wa _) (MkInfoKTKTKTT wb _) = matchWitnessKTKTKTT wa wb;
    };

	data WitKTKTKTT p where
	{
		WitKTKTKTT :: IOWitness (SatKTKTKTT p) -> WitKTKTKTT p;
	};

    instance WitnessKTKTKTT WitKTKTKTT where
    {
	    matchWitnessKTKTKTT (WitKTKTKTT wa) (WitKTKTKTT wb) = matchWitnessT wa wb;
	};

    data FactsKTKTKTT a = MkFactsKTKTKTT
    {
        infoFactKTKTKTT :: forall f. IOWitness (SatKKTKTKTTT f) -> Maybe (f a),
        deriveTFactsKTKTT :: forall i. InfoT i -> FactsKTKTT (a i)
    };

    instance Monoid (FactsKTKTKTT a) where
    {
        mempty = MkFactsKTKTKTT (\_ -> Nothing) (\_ -> mempty);
        mappend (MkFactsKTKTKTT f1 d1) (MkFactsKTKTKTT f2 d2) = MkFactsKTKTKTT
         (\w -> mplus (f1 w) (f2 w))
         (\info -> mappend (d1 info) (d2 info));
    };

    class FactKTKTKTT f where
    {
        witFactKTKTKTT :: IOWitness (SatKKTKTKTTT f);
    };

    matchPropertyKTKTKTT :: (FactKTKTKTT f) => InfoKTKTKTT a -> Maybe (f a);
    matchPropertyKTKTKTT (MkInfoKTKTKTT _ info) = infoFactKTKTKTT info witFactKTKTKTT;

    mkTFactsKTKTKTT_ :: IOWitness (SatKTT f) -> (forall i1 i2 i3. InfoT i1 -> InfoT i2 -> InfoT i3 -> Maybe (f (a i1 i2 i3))) -> FactsKTKTKTT a;
    mkTFactsKTKTKTT_ witF f = MkFactsKTKTKTT (\_ -> Nothing) (\t1 -> mkTFactsKTKTT_ witF (f t1));

    mkTFactsKTKTKTT :: forall f a. (FactT f) => (forall i1 i2 i3. InfoT i1 -> InfoT i2 -> InfoT i3 -> Maybe (f (a i1 i2 i3))) -> FactsKTKTKTT a;
    mkTFactsKTKTKTT = mkTFactsKTKTKTT_ witFactT;
}

