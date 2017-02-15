{-# OPTIONS -fno-warn-unused-binds -w #-}
module Truth.TypeKT.Type where
{
    import Truth.TypeKT.TH;
    import Data.KindCategory;
    import qualified Language.Haskell.TH as TH;
    import Language.Haskell.TH hiding (Info,Type,Kind);
    import Data.List;
    import Data.Char;
    import Control.Monad;
    import Data.OpenWitness;
    import Data.Witness;
    import Data.Maybe;
    import qualified Control.Category;
    import GHC.Exts hiding (Any);




    data WrappedType = forall a. WrapType a;

{-
$(
    forM supportedKinds (\k ->
        -- [d|data $(return (kTypeTypeName k)) (a :: $(return k))|];
        do
        {
            ndecla <- newName "a";
            return (DataD [] (kTypeTypeName k) [KindedTV ndecla k] [] []);
        }
    )
);
-}

    data Kind_W :: WrappedType -> * where
    {
        KindStar :: Kind_W (WrapType (WrapKind :: * -> WrappedKind));
        KindConstraint :: Kind_W (WrapType (WrapKind :: Constraint -> WrappedKind));
        KindArrow ::
         Kind_W (WrapType (WrapKind :: kp -> WrappedKind)) ->
         Kind_W (WrapType (WrapKind :: kq -> WrappedKind)) ->
         Kind_W (WrapType (WrapKind :: (kp -> kq) -> WrappedKind));
    };

    instance SimpleWitness Kind_W where
    {
        matchWitness KindStar KindStar = Just MkEqualType;
        matchWitness (KindArrow kp1 kq1) (KindArrow kp2 kq2) = do
        {
            MkEqualType <- matchWitness kp1 kp2;
            MkEqualType <- matchWitness kq1 kq2;
            return MkEqualType;
        };
        matchWitness _ _ = Nothing;
    };

    instance Eq1 Kind_W where
    {
        equals1 w1 w2 = isJust (matchWitness w1 w2);
    };

    instance Representative Kind_W where
    {
        getRepWitness KindStar = MkRepWitness;
        getRepWitness KindConstraint = MkRepWitness;
        getRepWitness (KindArrow kp kq) = case (getRepWitness kp,getRepWitness kq)
        of
        {
            (MkRepWitness,MkRepWitness) -> MkRepWitness;
        };
    };

    instance Is Kind_W (WrapType (WrapKind :: * -> WrappedKind)) where
    {
        representative = KindStar;
    };

    instance Is Kind_W (WrapType (WrapKind :: Constraint -> WrappedKind)) where
    {
        representative = KindConstraint;
    };

    instance
    (
        Is Kind_W (WrapType (WrapKind :: kp -> WrappedKind)),
        Is Kind_W (WrapType (WrapKind :: kq -> WrappedKind))
    ) => Is Kind_W (WrapType (WrapKind :: (kp -> kq) -> WrappedKind)) where
    {
        representative = KindArrow representative representative;
    };

    type IsKind (wk :: k -> WrappedKind) = (KindCategory wk,Is Kind_W (WrapType wk));

    type Kind_X wk = Kind_W (WrapType wk);

    witKind :: (IsKind wk) => Kind_X wk;
    witKind = representative;

    typeKind :: (IsKind (WrapKind :: k -> WrappedKind)) =>
     Type (t :: k) -> Kind_X (WrapKind :: k -> WrappedKind);
    typeKind _ = representative;

{-
    class IsKind (k :: * -> *) where
    {
        witKind :: IOWitness k;
    };

    class (IsKind (TypeKind t)) => HasKind t where
    {
        type TypeKind t :: * -> *;
        typeKind :: TypeKind t t;
    };

    class (HasKind f, HasKind a, HasKind (TypeConstructed f a)) =>
         ConstructType f a where
    {
        type TypeConstructed f a :: *;
    };
-}

    type family WrapApply (f :: WrappedType) (x :: WrappedType) :: WrappedType;
    type instance WrapApply (WrapType (f :: ka -> kfa)) (WrapType (a :: ka)) = WrapType (f a);

--    constructKind :: WrapKind (WrapType :: k -> WrappedType)
--    constructKind kf ka

{-
    class () =>
        ConstructType (f :: WrappedType) (a :: WrappedType) where
    {
        type WrapApply f a :: WrappedType;
        constructKind :: WrapKind (WrapType :: k -> WrappedType)
    };


    instance


-}


--    data Kind_X k (t :: *) where { Kind_X :: forall (a :: k). Kind_X (Type_X a); };

{-
$(
    forM supportedKinds (\k ->
    -- data Kind_##p (t :: *) where { Kind_##p :: forall a. Kind_##p (Type_##p a); };
    -- data Kind_##p (t :: *) where { Kind_##p :: forall a. (t ~ Type_##p a) => Kind_##p t; };
    do
    {
        t <- newName "t";
        a <- newName "a";
        return (
            DataD [] (kKindTypeName k) [KindedTV t StarK]
             [ForallC [KindedTV a k] [EqualP (VarT t) (AppT (kTypeType k) (VarT a))] (NormalC (kKindConsName k) [])]
             []
            );
    })
);
$(
    fmap concat (forM supportedKinds (\k -> let
    {
        tkType = return (kKindType k);
    } in
    [d|
        instance IsKind $(tkType) where
        {
            witKind = $(iowitness [t|Type_X $(tkType)|]);
        };
    |]))
);
$(
    fmap concat (forM supportedKinds (\k -> let
    {
        tt = return (kTypeType k);
        tkType = return (kKindType k);
        tkCons = conE (kKindConsName k);
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
        -- [d|data $(return (kTypeTypeName k)) (a :: $(return k))|];
        do
        {
            ndecla <- newName "a";
            return (DataD [] (kTypeTypeName k) [KindedTV ndecla k] [] []);
        };

        makeInstanceDecs :: TH.Kind -> Q [Dec];
        makeInstanceDecs StarK = return [];
        makeInstanceDecs kpq@(ArrowK kp kq) = let
        {
            tp = kTypeTypeQ kp;
            tq = kTypeTypeQ kq;
            tpq = kTypeTypeQ kpq;
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
-}
}
