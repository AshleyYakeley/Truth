{-# LANGUAGE CPP #-}
module Truth.TypeKT.Construct where
{
    import Truth.TypeKT.Kind;
    import Truth.TypeKT.Type;
    import Data.Maybe;
    import Control.Monad;

    class (Property f) => Construct f where
    {
        construct :: f t -> Info t;
    };

    data Match t where
    {
        MkMatch :: forall f a.
         (
            ConstructType f a
{-
            ,
            DeconstructType (TypeConstructed f a),
            f ~ TypeConstructor (TypeConstructed f a),
            a ~ TypeArgument (TypeConstructed f a)
-}
         ) => Info f -> Info a -> Match (TypeConstructed f a);
    };

    instance Property Match where
    {
        matchProperty (MkInfo (ConsWit tf ta) _) = return (MkMatch tf ta);
        matchProperty _ = Nothing;
    };

    applyInfo :: (
            ConstructType f a
{-
            ,
            DeconstructType (TypeConstructed f a),
            f ~ TypeConstructor (TypeConstructed f a),
            a ~ TypeArgument (TypeConstructed f a)
-}
         ) => Info f -> Info a -> Info (TypeConstructed f a);
    applyInfo tf@(MkInfo _ inf) ta@(MkInfo _ _) = MkInfo (ConsWit tf ta) (deriveFacts inf ta);

    instance Construct Match where
    {
        construct (MkMatch tf ta) = applyInfo tf ta;
    };



{-




#define DECL_Construct(P)\
    class (Property##P f) => Construct##P f where\
    {\
        construct##P :: f t -> Info##P t;\
    };

#define DECL_Match(P,Q)\
    data Q##Match##P t where\
    {\
        Mk##Q##Match##P :: forall f a. InfoK##Q##P f -> Info##Q a -> Q##Match##P (f a);\
    };\
\
    instance Property##P Q##Match##P where\
    {\
        matchProperty##P (MkInfo##P (Q##Wit##P tf ta) _) = return (Mk##Q##Match##P tf ta);\
        matchProperty##P _ = Nothing;\
    };\
\
    apply##Q##Info##P :: InfoK##Q##P f -> Info##Q a -> Info##P (f a);\
    apply##Q##Info##P tf@(MkInfoK##Q##P _ inf) ta = MkInfo##P (Q##Wit##P tf ta) (derive##Q##Facts##P inf ta);\
\
    instance Construct##P Q##Match##P where\
    {\
        construct##P (Mk##Q##Match##P tf ta) = apply##Q##Info##P tf ta;\
    };

DECL_Construct(T)
DECL_Match(T,T)
DECL_Match(T,KTT)

DECL_Construct(KTT)
DECL_Match(KTT,T)
DECL_Match(KTT,KTT)

DECL_Construct(KTKTT)
DECL_Match(KTKTT,T)
-}
}
