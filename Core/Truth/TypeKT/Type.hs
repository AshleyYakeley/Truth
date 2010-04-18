{-# LANGUAGE CPP #-}
module Truth.TypeKT.Type
(
    module Truth.TypeKT.Type,
    Monoid(..)
) where
{
    import Truth.TypeKT.Basic;
    import Truth.TypeKT.Witness;
	import Data.Witness;
	import Data.OpenWitness;
	import Data.Maybe;
    import Control.Monad;
	import Data.Monoid;
	import Control.Category;

    -- K and T represent kinds. T represents *, and Kxy represents x -> y.

#define DECL_Info(P)\
    data Info##P a = MkInfo##P (Wit##P a) (Facts##P a);\
\
    instance Witness##P Info##P where\
    {\
        matchWitness##P (MkInfo##P wa _) (MkInfo##P wb _) = matchWitness##P wa wb;\
    };

#define CONS_Wit(P,Q) Q##Wit##P :: InfoK##Q##P p -> Info##Q a -> Wit##P (p a);

#define DECL_Wit(P,CONS)\
	data Wit##P p where\
	{\
		Wit##P :: IOWitness (Sat##P p) -> Wit##P p;\
		CONS\
	};

#define EQN_matchWitness(P,Q)\
	    matchWitness##P (Q##Wit##P tfa ta) (Q##Wit##P tfb tb) = do\
	    {\
		    MkEqualType <- matchWitnessK##Q##P tfa tfb;\
		    MkEqualType <- matchWitness##Q ta tb;\
		    return MkEqualType;\
	    };

#define EQN_matchWitnessDefault(P) matchWitness##P _ _ = Nothing;

#define DECL_instance_Witness_Wit(P,EQNS)\
    instance Witness##P Wit##P where\
    {\
	    matchWitness##P (Wit##P wa) (Wit##P wb) = matchWitnessT wa wb;\
EQNS\
	};

#define DECL_Property(P)\
    class Property##P f where\
    {\
        matchProperty##P :: forall t. Info##P t -> Maybe (f t);\
    };\
\
    matchProperty##P##_ :: (Property##P f) => Type (f F##P) -> Info##P t -> Maybe (f t);\
    matchProperty##P##_ _ = matchProperty##P;


#define MEMBER_deriveFacts(P,Q) ,derive##P##Facts##Q :: forall i. Info##P i -> Facts##Q (a i)

#define DECL_Facts(P,MEMBERS)\
    data Facts##P a = MkFacts##P\
    {\
        infoFact##P :: forall f. IOWitness (SatK##P##T f) -> Maybe (f a)\
        MEMBERS\
    };\
\
    instance Monoid (Facts##P a) where\
    {\
        mempty = MkFacts##P (\_ -> Nothing) (\_ -> mempty);\
        mappend (MkFacts##P f1 d1) (MkFacts##P f2 d2) = MkFacts##P\
         (\w -> mplus (f1 w) (f2 w))\
         (\info -> mappend (d1 info) (d2 info));\
    };

#define DECL_Fact(P)\
    class (Property##P f) => Fact##P f where\
    {\
        witFact##P :: IOWitness (SatK##P##T f);\
    };\
\
    matchProperty##P##_Fact :: (Fact##P f) => Info##P a -> Maybe (f a);\
    matchProperty##P##_Fact (MkInfo##P _ info) = infoFact##P info witFact##P;

    -- T

DECL_Info(T)
DECL_Wit(T,CONS_Wit(T,T) CONS_Wit(T,KTT))
DECL_instance_Witness_Wit(T,EQN_matchWitness(T,T) EQN_matchWitness(T,KTT) EQN_matchWitnessDefault(T))

	instance Eq1 WitT where
	{
		equals1 r1 r2 = isJust (matchWitnessT r1 r2);
	};

DECL_Property(T)

    data FactsT a = MkFactsT
    {
        infoFactT :: forall f. IOWitness (SatKTT f) -> Maybe (f a)
    };

    instance Monoid (FactsT a) where
    {
        mempty = MkFactsT (\_ -> Nothing);
        mappend (MkFactsT f1) (MkFactsT f2) = MkFactsT (\w -> mplus (f1 w) (f2 w));
    };

DECL_Fact(T)

    mkTFactsT_ :: IOWitness (SatKTT f) -> Maybe (f a) -> FactsT a;
    mkTFactsT_ witF f = MkFactsT (\wit -> do
    {
        MkEqualType <- matchWitnessT wit witF;
        f;
    });

    mkTFactsT :: forall f a. (FactT f) => Maybe (f a) -> FactsT a;
    mkTFactsT = mkTFactsT_ witFactT;


    -- KTT
DECL_Info(KTT)
DECL_Wit(KTT,CONS_Wit(KTT,T) CONS_Wit(KTT,KTT))
DECL_instance_Witness_Wit(KTT,EQN_matchWitness(KTT,T) EQN_matchWitness(KTT,KTT) EQN_matchWitnessDefault(KTT))
DECL_Property(KTT)
DECL_Facts(KTT,MEMBER_deriveFacts(T,T))
DECL_Fact(KTT)

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
DECL_Info(KTKTT)
DECL_Wit(KTKTT,CONS_Wit(KTKTT,T))
DECL_instance_Witness_Wit(KTKTT,EQN_matchWitness(KTKTT,T) EQN_matchWitnessDefault(KTKTT))
DECL_Property(KTKTT)
DECL_Facts(KTKTT,MEMBER_deriveFacts(T,KTT))
DECL_Fact(KTKTT)

    mkTFactsKTKTT_ :: IOWitness (SatKTT f) -> (forall i1 i2. InfoT i1 -> InfoT i2 -> Maybe (f (a i1 i2))) -> FactsKTKTT a;
    mkTFactsKTKTT_ witF f = MkFactsKTKTT (\_ -> Nothing) (\ta -> mkTFactsKTT_ witF (f ta));

    mkTFactsKTKTT :: forall f a. (FactT f) => (forall i1 i2. InfoT i1 -> InfoT i2 -> Maybe (f (a i1 i2))) -> FactsKTKTT a;
    mkTFactsKTKTT = mkTFactsKTKTT_ witFactT;

    mkKTTFactsKTKTT_ :: IOWitness (SatKKTTT f) -> (forall i. InfoT i -> Maybe (f (a i))) -> FactsKTKTT a;
    mkKTTFactsKTKTT_ witF f = MkFactsKTKTT (\_ -> Nothing) (mkKTTFactsKTT_ witF . f);

    mkKTTFactsKTKTT :: forall f a. (FactKTT f) => (forall i. InfoT i -> Maybe (f (a i))) -> FactsKTKTT a;
    mkKTTFactsKTKTT = mkKTTFactsKTKTT_ witFactKTT;


    -- KKTTT
DECL_Info(KKTTT)
DECL_Wit(KKTTT,)
DECL_instance_Witness_Wit(KKTTT,)
DECL_Property(KKTTT)
DECL_Facts(KKTTT,MEMBER_deriveFacts(KTT,T))
DECL_Fact(KKTTT)

    mkTFactsKKTTT_ :: IOWitness (SatKTT f) -> (forall i1. InfoKTT i1 -> Maybe (f (a i1))) -> FactsKKTTT a;
    mkTFactsKKTTT_ witF f = MkFactsKKTTT (\_ -> Nothing) (\ta -> mkTFactsT_ witF (f ta));

    mkTFactsKKTTT :: forall f a. (FactT f) => (forall i1. InfoKTT i1 -> Maybe (f (a i1))) -> FactsKKTTT a;
    mkTFactsKKTTT = mkTFactsKKTTT_ witFactT;


    -- KKTTKTT
DECL_Info(KKTTKTT)
DECL_Wit(KKTTKTT,)
DECL_instance_Witness_Wit(KKTTKTT,)
DECL_Property(KKTTKTT)
DECL_Facts(KKTTKTT,MEMBER_deriveFacts(KTT,KTT))
DECL_Fact(KKTTKTT)

    mkTFactsKKTTKTT_ :: IOWitness (SatKTT f) -> (forall i1 i2. InfoKTT i1 -> InfoT i2 -> Maybe (f (a i1 i2))) -> FactsKKTTKTT a;
    mkTFactsKKTTKTT_ witF f = MkFactsKKTTKTT (\_ -> Nothing) (\ta -> mkTFactsKTT_ witF (f ta));

    mkTFactsKKTTKTT :: forall f a. (FactT f) => (forall i1 i2. InfoKTT i1 -> InfoT i2 -> Maybe (f (a i1 i2))) -> FactsKKTTKTT a;
    mkTFactsKKTTKTT = mkTFactsKKTTKTT_ witFactT;


    -- KTKTKTT
DECL_Info(KTKTKTT)
DECL_Wit(KTKTKTT,)
DECL_instance_Witness_Wit(KTKTKTT,)
DECL_Property(KTKTKTT)
DECL_Facts(KTKTKTT,MEMBER_deriveFacts(T,KTKTT))
DECL_Fact(KTKTKTT)

    mkTFactsKTKTKTT_ :: IOWitness (SatKTT f) -> (forall i1 i2 i3. InfoT i1 -> InfoT i2 -> InfoT i3 -> Maybe (f (a i1 i2 i3))) -> FactsKTKTKTT a;
    mkTFactsKTKTKTT_ witF f = MkFactsKTKTKTT (\_ -> Nothing) (\t1 -> mkTFactsKTKTT_ witF (f t1));

    mkTFactsKTKTKTT :: forall f a. (FactT f) => (forall i1 i2 i3. InfoT i1 -> InfoT i2 -> InfoT i3 -> Maybe (f (a i1 i2 i3))) -> FactsKTKTKTT a;
    mkTFactsKTKTKTT = mkTFactsKTKTKTT_ witFactT;
}

