{-# LANGUAGE CPP #-}
module Truth.TypeKT.Witness where
{
    import Truth.TypeKT.Basic;
    import Data.OpenWitness;
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
DECL_Witness(T)

DECL(T,T)
DECL(KTT,T)
DECL(T,KTT)
DECL(KTT,KTT)
DECL(T,KTKTT)

    instance WitnessT (OpenWitness w) where
    {
        matchWitnessT = matchWitness;
    };

}
