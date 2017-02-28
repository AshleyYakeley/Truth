{-# LANGUAGE TypeOperators, TypeInType, UndecidableSuperClasses #-}
{-# OPTIONS -fno-warn-unused-binds -w #-}
module Data.Reity.Info
(
    module Data.Reity.Info,
    -- Monoid(..)
) where
{
    import Data.Maybe;
    import Control.Monad;
    import Data.Type.Equality;
    import Data.Type.Heterogeneous;
    import Data.OpenWitness;
    import Data.Kind;
--    import Data.Reity.Type;



{-
    class Known (a :: k) where
    {
        known :: forall (proxy :: k -> *). proxy a -> k;
    };

    instance Known True where
    {
        known _ = True
    };

    type TypeW = TypeRep;

    type HasTypeW = Typeable;


    areEqual :: forall (t :: *). HasTypeW t => t -> t -> Bool;
    areEqual a b = case findInstance instances (typeW :: TypeW (Eq t)) of
    {
        Just MkDict -> a == b;
        Nothing -> False;
    };
-}

    data Info (t :: k) where
    {
        MkInfo :: forall (k :: *) (t :: k). Info k -> Wit t -> Facts t -> Info t;
    };

    instance TestHetEquality Info where
    {
        testHetEquality (MkInfo _ wa _) (MkInfo _ wb _) = testHetEquality wa wb;
    };

    data Wit (t :: k) where
    {
        SimpleWit :: forall (k :: *) (t :: k). IOWitness t -> Wit t;
        ConsWit :: forall (ka :: *) (kb :: *) (f :: ka -> kb) (a :: ka). Info f -> Info a -> Wit (f a);
    };

    instance TestHetEquality Wit where
    {
        testHetEquality (SimpleWit iowa) (SimpleWit iowb) = testHetEquality iowa iowb;
        testHetEquality (ConsWit ica iaa) (ConsWit icb iab) = do
        {
            ReflH <- testHetEquality ica icb;
            ReflH <- testHetEquality iaa iab;
            return ReflH;
        };
        testHetEquality _ _ = Nothing;
    };

    splitInfo' :: forall (ka :: *) (f :: ka -> kfa) (a :: ka). Info (f a) -> Maybe (Info f,Info a);
    splitInfo' (MkInfo kfa (ConsWit infoF infoA) _) = Just (infoF,infoA);
    splitInfo' _ = Nothing;

    applyInfo :: forall (ka :: *) (kb :: *) (f :: ka -> kb) (a :: ka). Info f -> Info a -> Info (f a);
    applyInfo tf@(MkInfo ika _ inf) ta = MkInfo (snd $ fromMaybe (error "unexpected kind witness") $ splitInfo' ika) (ConsWit tf ta) (deriveFacts inf ta);

    splitInfo :: forall (r :: *) (kfa :: *) (fa :: kfa). Info fa ->
     (forall (ka :: *) (f :: ka -> kfa) (a :: ka). (f a ~ fa) => (Info f,Info a) -> r) ->
     Maybe r;
    splitInfo (MkInfo kfa (ConsWit infoF infoA) _) ff = Just (ff (infoF,infoA));
    splitInfo _ _ = Nothing;

    splitInfoMaybe :: forall (r :: *) (kfa :: *) (fa :: kfa). Info fa ->
     (forall (ka :: *) (f :: ka -> kfa) (a :: ka). (f a ~ fa) => (Info f,Info a) -> Maybe r) ->
     Maybe r;
    splitInfoMaybe ifa ff = do
    {
        mr <- splitInfo ifa ff;
        mr;
    };

    data Facts (t :: k) = MkFacts
    {
        infoFact :: forall (k :: *) (t :: k) (fact :: k -> *). (Fact fact) => Maybe (fact t),
        deriveFacts :: forall (ka :: *) (kb :: *) (x :: ka) (t :: ka -> kb). Info x -> Facts (t x)
    };

    instance Monoid (Facts wt) where
    {
        mempty = MkFacts Nothing (\_ -> mempty);
        mappend (MkFacts f1 d1) (MkFacts f2 d2) = MkFacts
         (mplus f1 f2)
         (\info -> mappend (d1 info) (d2 info));
    };

    --mkSimpleInfo :: forall (k :: *) (t :: k). (IsKind k) => IOWitness t -> [Facts t] -> Info t;
    --mkSimpleInfo wit facts = MkInfo witKind (SimpleWit wit) (mconcat facts);

    class Property (prop :: k -> *) where
    {
        matchProperty :: forall (t :: k). Info t -> Maybe (prop t);
    };

    matchProp :: (Property prop) => proxy prop -> Info t -> Maybe (prop t);
    matchProp _ = matchProperty;

    class (Property fact) => Fact (fact :: k -> *) where
    {
        factInfo :: Info fact;
    };

    matchProperty_Fact :: forall (k :: *) (t :: k) (prop :: k -> *). (Fact prop) => Info t -> Maybe (prop t);
    matchProperty_Fact (MkInfo _ _ facts) = infoFact facts;
}

