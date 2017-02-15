{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-unused-binds -w #-}
module Truth.TypeKT.Info
(
    module Truth.TypeKT.Info,
    Monoid(..)
) where
{
    import Truth.TypeKT.Type;
    import Data.KindCategory;
    import Data.Witness;
    import Data.OpenWitness;
    import Data.Maybe;
    import Control.Monad;
    import Data.Monoid;
    import Control.Applicative.Free;
    import Data.Category;

    import Truth.TypeKT.TH;
    import Data.List;
    import Data.Char;
    import Data.Bool;
    import qualified Language.Haskell.TH as TH;
    import Language.Haskell.TH hiding (Info,Type,Kind);




    data Info_W :: WrappedType -> * where
    {
        MkInfo :: forall (t :: k). Kind_X (WrapKind :: k -> WrappedKind) -> Wit_W (WrapType t) -> Facts_W (WrapType t) -> Info_W (WrapType t);
    };

    type Info_X (a :: k) = Info_W (WrapType a);
    type Info_X' (a :: k) = Info_W (WrapType a);
    type Info_X'' (a :: k) = Info_W (WrapType a);
    type Info_X''' (a :: k) = Info_W (WrapType a);

    type IOWitness_X (a :: k) = IOWitness (WrapType a);

    instance SimpleWitness Info_W where
    {
        matchWitness (MkInfo _ wa _) (MkInfo _ wb _) = matchWitness wa wb;
    };

    data Wit_W :: WrappedType -> * where
    {
        SimpleWit :: forall (wt :: WrappedType). IOWitness wt -> Wit_W wt;
        ConsWit :: forall f a. Info_W (WrapType f) -> Info_W (WrapType a) -> Wit_W (WrapType (f a));
    };

    splitInfo :: forall (r :: *) (fa :: kfa). Info_X'' fa ->
     (forall (f :: ka -> kfa) (a :: ka). (f a ~ fa) => (Info_X'' f,Info_X'' a) -> r) ->
     Maybe r;
    splitInfo (MkInfo kfa (ConsWit infoF infoA) _) ff = Just (ff (infoF,infoA));
    splitInfo _ _ = Nothing;

    newtype Knowledge = MkKnowledge (forall (a :: *). Knowledge -> Info_X'' a -> Maybe a);

    instance Monoid Knowledge where
    {
        mempty = MkKnowledge (\_ _ -> mzero);
        mappend (MkKnowledge ka) (MkKnowledge kb) = MkKnowledge (\k' w -> mplus (ka k' w) (kb k' w));
    };

    ask :: Knowledge -> Info_X'' a -> Maybe a;
    ask k@(MkKnowledge f) w = f k w;

    know :: forall a. Info_X'' a -> a -> Knowledge;
    know w a = MkKnowledge (\_ w' -> do
    {
        MkEqualType <- matchWitness w w';
        return a;
    });

    knowDependent :: forall b. (forall a. Info_X'' a -> Maybe (Info_X'' b,b -> a)) -> Knowledge;
    knowDependent wamwbba = MkKnowledge (\k wa -> do
    {
        (wb,ba) <- wamwbba wa;
        b <- ask k wb;
        return (ba b);
    });


    instance SimpleWitness Wit_W where
    {
        matchWitness (SimpleWit iowa) (SimpleWit iowb) = matchWitness iowa iowb;

        matchWitness (ConsWit ica iaa) (ConsWit icb iab) = do
        {
            MkEqualType <- matchWitness ica icb;
            MkEqualType <- matchWitness iaa iab;
            return MkEqualType;
        };
        matchWitness _ _ = Nothing;
    };

    data Facts_W (wt :: WrappedType) = MkFacts
    {
        infoFact :: forall (fact :: WrappedType -> *). (Fact fact) => Maybe (fact wt),
        deriveFacts :: forall (wx :: WrappedType). Info_W wx -> Facts_W (WrapApply wt wx)
    };

    type Facts_X (a :: k) = Facts_W (WrapType a);

    instance Monoid (Facts_W wt) where
    {
        mempty = MkFacts Nothing (\_ -> mempty);
        mappend (MkFacts f1 d1) (MkFacts f2 d2) = MkFacts
         (mplus f1 f2)
         (\info -> mappend (d1 info) (d2 info));
    };

    mkSimpleInfo :: forall (t :: k). (IsKind (WrapKind :: k -> WrappedKind)) => IOWitness_X t -> [Facts_X t] -> Info_W (WrapType t);
    mkSimpleInfo wit facts = MkInfo witKind (SimpleWit wit) (mconcat facts);


    class Property (prop :: WrappedType -> *) where
    {
        matchProperty :: Info_W wt -> Maybe (prop wt);
    };

    matchProp :: (Property prop) => Type prop -> Info_W wt -> Maybe (prop wt);
    matchProp _ = matchProperty;

{-
$(fmap concat (forM supportedKinds (\k -> do
    {
        [d|
            instance Property Kind_T where
            {
                matchProperty (MkInfo Kind_T _ _) = Just Kind_T;
                matchProperty _ = Nothing;
            };
        |]
    }))
);
-}

    data WrapArgType :: (k -> *) -> WrappedType -> * where
    {
        MkWrapArgType :: forall (f :: k -> *) (a :: k). (f a) -> WrapArgType f (WrapType a);
    };

    data InfoXW :: (WrappedType -> *) -> * where
    {
        MkInfoXW :: forall (f :: k -> *). Info_X f -> InfoXW (WrapArgType f);
    };

    instance SimpleWitness InfoXW where
    {
        matchWitness (MkInfoXW w1) (MkInfoXW w2) = do
        {
            MkEqualType <- matchWitness w1 w2;
            return MkEqualType;
        };
    };










    class (Property fact) => Fact (fact :: WrappedType -> *) where
    {
        factInfoXW :: InfoXW fact;
    };

    matchProperty_Fact :: forall (wt :: WrappedType) (prop :: WrappedType -> *). (Fact prop) => Info_W wt -> Maybe (prop wt);
    matchProperty_Fact (MkInfo _ _ facts) = infoFact facts;


    data Fact_U (prop :: WrappedType -> *) (a :: WrappedType) =
        MkFactZ_U (Maybe (prop a)) |
        MkFactS_U (forall (i :: WrappedType). Info_W i -> Fact_U prop (WrapApply a i));

    instance FactChecker Fact_U where
    {
        mkFacts (MkFactZ_U f) = MkFacts (infoFactF f) (\_ -> mempty);
        mkFacts (MkFactS_U f) = MkFacts Nothing (\i -> mkFacts (f i));
    };


    newtype FactZ (prop :: WrappedType -> *) (a :: WrappedType) = MkFactZ (Maybe (prop a));

    newtype FactS (ff :: (WrappedType -> *) -> WrappedType -> *) (prop :: WrappedType -> *) (a :: WrappedType) =
     MkFactS (forall (i :: WrappedType). Info_W i -> ff prop (WrapApply a i));

    class FactChecker (ff :: (WrappedType -> *) -> WrappedType -> *) where
    {
        mkFacts :: forall (fact :: WrappedType -> *) (a :: WrappedType). (Fact fact) => ff fact a -> Facts_W a;
    };

    mkFacts0 :: forall (fact :: WrappedType -> *) (a :: WrappedType). (Fact fact) =>
     Maybe (fact a) -> Facts_W a;
    mkFacts0 f = mkFacts (MkFactZ_U f);

    mkFacts1 :: forall (fact :: WrappedType -> *) (a :: WrappedType). (Fact fact) =>
     (forall (t0 :: WrappedType). Info_W t0 -> Maybe (fact (WrapApply a t0))) -> Facts_W a;
    mkFacts1 f = mkFacts (MkFactS_U (\a0 -> MkFactZ_U (f a0)));

    mkFacts2 :: forall (fact :: WrappedType -> *) (a :: WrappedType). (Fact fact) =>
     (forall t0 t1. Info_W t0 -> Info_W t1 -> Maybe (fact (WrapApply (WrapApply a t0) t1))) -> Facts_W a;
    mkFacts2 f = mkFacts (MkFactS_U (\a0 -> MkFactS_U (\a1 -> MkFactZ_U (f a0 a1))));

    infoFactF :: forall fact1 fact2 a. (Fact fact1,Fact fact2) => Maybe (fact1 a) -> Maybe (fact2 a);
    infoFactF f = do
    {
        MkEqualType <- matchWitness (factInfoXW :: InfoXW fact1) (factInfoXW :: InfoXW fact2);
        f;
    };

    instance FactChecker FactZ where
    {
        mkFacts (MkFactZ f) = MkFacts (infoFactF f) (\_ -> mempty);
    };

    instance (FactChecker ff) => FactChecker (FactS ff) where
    {
        mkFacts (MkFactS f) = MkFacts Nothing (\i -> mkFacts (f i));
    };


    data KindProperty :: (k -> WrappedKind) -> WrappedType -> * where
    {
        MkWrappedProperty :: forall (t :: k). KindProperty (WrapKind :: k -> WrappedKind) (WrapType t);
    };

    instance (IsKind wk,wk ~ WrapKind) => Property (KindProperty wk) where
    {
        matchProperty (MkInfo kind _ _) = do
        {
            MkEqualType <- matchWitness kind (witKind :: Kind_X wk);
            return MkWrappedProperty;
        };
    };


{-
$(fmap concat (forM supportedKinds (\k -> let
    {
        tkType = conT (mkName ("Kind_" ++ (kindCode k)));
    } in
    [d|
        instance Property $(tkType) where
        {
            matchProperty = matchPropertyKind $(type1 tkType);
        };
    |]))
);
-}

{-
    $(fmap concat (forM supportedKinds (\k -> let
        {
            t = kindTypeQ k;
        } in let
        {
            funName = mkName ("matchKind_" ++ (kindCode k));
            funcType = [t| forall t r. Info t -> (forall a. (t ~ $(t) a) => Info ($(t) a) -> Maybe r) -> Maybe r |];
            sigDecl = TH.sigD funName funcType;
            sigDefn = do
            {
                tvar <- newName "t";
                fvar <- newName "f";
                TH.funD funName
                [
                TH.clause
                  [asP tvar (conP 'MkInfo [conP (kindConsName k) [],wildP,wildP]),varP fvar]
                  (normalB (appE (varE fvar) (varE tvar)))
                  []
                ,
                TH.clause [wildP,wildP] (normalB [|Nothing|]) []
                ];
            };
        } in sequence [sigDecl,sigDefn]
        {-
        [d|
        matchKind_T :: forall t r. Info t -> (forall a. (t ~ $(t) a) => Info ($(t) a) -> Maybe r) -> Maybe r;
        matchKind_T t@(MkInfo Kind_T _ _) f = f t;
        matchKind_T _ _ = Nothing;
        |]
        -}
    )));
-}

}

