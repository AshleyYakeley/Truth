{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-unused-binds -w #-}
module Truth.TypeKT.Type where
{
    import Truth.TypeKT.TH;
    import qualified Language.Haskell.TH as TH;
    import Language.Haskell.TH hiding (Info,Type,Kind);
    import Data.List;
    import Data.Char;
    import Control.Monad;
	import Data.OpenWitness;

    $(forM supportedKinds (\k ->
        -- [d|data $(return (kindTypeName k)) (a :: $(return k))|];
        do
        {
            ndecla <- newName "a";
            return (DataD [] (kindTypeName k) [KindedTV ndecla k] [] []);
        }
    ));

    class (HasKind f, HasKind a, HasKind (TypeConstructed f a)) =>
         ConstructType f a where
    {
        type TypeConstructed f a :: *;
    };

#define DECL_IsKind(p) data Kind_##p t where { Kind_##p :: forall a. Kind_##p (Type_##p a); };

    DECL_IsKind(T)
    DECL_IsKind(KTT)
    DECL_IsKind(KTKTT)
    DECL_IsKind(KTKTKTT)
    DECL_IsKind(KKTTKTT)
    DECL_IsKind(KKTTT)

    class IsKind k where
    {
        witKind :: IOWitness (k ());
    };

    class (IsKind (TypeKind t)) => HasKind t where
    {
        type TypeKind t :: * -> *;
        typeKind :: TypeKind t t;
    };

$(fmap concat (forM supportedKinds (\k -> let
    {
        tkType = conT (mkName ("Kind_" ++ (kindCode k)));
    } in
    [d|
        instance IsKind $(tkType) where
        {
            witKind = $(iowitness [t|$(tkType) ()|]);
        };
    |]))
);


$(fmap concat (forM supportedKinds (\k -> let
    {
        tt = kindTypeQ k;
        tkType = conT (mkName ("Kind_" ++ (kindCode k)));
        tkCons = conE (mkName ("Kind_" ++ (kindCode k)));
    } in
    {-
    [d|
        instance HasKind ($(tt) a) where
        {
            type TypeKind ($(tt) a) = $(tkType);
            typeKind = $(tkCons);
        };
    |]
    -}
    do
    {
        an <- newName "a";
        a <- return (varT an);
        sequence
        [
            instanceD (return [])
              (appT (conT ''HasKind) (appT tt a))
            [
                (tySynInstD ''TypeKind [appT tt a] tkType),
                valD (varP 'typeKind)
                 (normalB tkCons)
                 []
            ]
        ]
    }
    ))
);

$(
    let
    {
        makeTypeDec :: TH.Kind -> Q Dec;
        makeTypeDec k =
        -- [d|data $(return (kindTypeName k)) (a :: $(return k))|];
        do
        {
            ndecla <- newName "a";
            return (DataD [] (kindTypeName k) [KindedTV ndecla k] [] []);
        };

        makeInstanceDecs :: TH.Kind -> Q [Dec];
        makeInstanceDecs StarK = return [];
        makeInstanceDecs kpq@(ArrowK kp kq) = let
        {
            tp = kindTypeQ kp;
            tq = kindTypeQ kq;
            tpq = kindTypeQ kpq;
        } in
        {-
        [d|
            instance ConstructType ($(tpq) f) ($(tp) a) where
            {
                type TypeConstructed ($(tpq) f) ($(tp) a) = $(tq) (f a);

                constructKind $() $() = $();
            };
        |];
        -}
        let
        {
            typef = sigT (varT (mkName "f")) kpq;
            typea = sigT (varT (mkName "a")) kp;
            tf = appT tpq typef;
            -- [t|$(tpq) $(typef)|];
            ta = appT tp typea;
            -- [t|$(tp) $(typea)|];
            tfa = appT tq (appT typef typea);
            -- [t|$(tq) ($(typef) $(typea))|];
        } in do
        {
        consdec <- instanceD (return [])
        (appT (appT (conT ''ConstructType) tf) ta)
         [
         (tySynInstD ''TypeConstructed [tf,ta] tfa)
         ];

        return [consdec];
        };

        makeCompleteDec :: TH.Kind -> Q [Dec];
        makeCompleteDec k = do
        {
            tyd <- makeTypeDec k;
            inds <- makeInstanceDecs k;
            return (tyd:inds);
        };
    }
    in do
    {
        declist <- forM supportedKinds makeInstanceDecs;
        return (concat declist);
    }
);
}
