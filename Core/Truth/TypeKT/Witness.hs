{-# LANGUAGE CPP #-}
module Truth.TypeKT.Witness where
{
    import Truth.TypeKT.Basic;
    import Data.Witness;
    import Data.Maybe;

#define DECL_Witness(P)\
    class Witness##P w where\
    {\
        matchWitness##P :: forall a b. w a -> w b -> Maybe (EqualType##P a b);\
    };

#define DECL(P,Q)\
    type EqualTypeK##P##Q a b = EqualType##Q (a F##P) (b F##P);\
\
DECL_Witness(K##P##Q)

    -- T = *
    type EqualTypeT = EqualType;

    class (SimpleWitness w) => WitnessT w where
    {
        matchWitnessT :: forall a b. w a -> w b -> Maybe (EqualTypeT a b);
    };

    instance (SimpleWitness w) => WitnessT w where
    {
        matchWitnessT = matchWitness;
    };

DECL(T,T)
DECL(KTT,T)
DECL(T,KTT)
DECL(KTT,KTT)
DECL(T,KTKTT)
}
