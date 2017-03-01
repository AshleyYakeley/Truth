{-# LANGUAGE TypeOperators, TypeInType, UndecidableSuperClasses #-}
{-# OPTIONS -fno-warn-unused-binds -w #-}
module Data.Reity.Info where
{
    import Data.Maybe;
    import Control.Monad;
    import Data.Type.Equality;
    import Data.Type.Heterogeneous;
    import Data.OpenWitness;
    import Data.Kind;
    import Data.Maybe;
    import Data.KindCategory;
    import Data.Knowledge;


    type ConstraintKnowledge = Knowledge Info ConstraintWitness;

    data Info (t :: k) where
    {
        MkInfo :: forall (k :: *) (t :: k). Info k -> Wit t -> Info t;
    };

    instance TestHetEquality Info where
    {
        testHetEquality (MkInfo _ wa) (MkInfo _ wb) = testHetEquality wa wb;
    };

    instance TestEquality Info where
    {
        testEquality wa wb = do
        {
            ReflH <- testHetEquality wa wb;
            return Refl;
        }
    };

    data Wit (t :: k) where
    {
        SimpleWit :: forall (k :: *) (t :: k). IOWitness t -> ConstraintKnowledge -> Wit t;
        ConsWit :: forall (ka :: *) (kb :: *) (f :: ka -> kb) (a :: ka). Info f -> Info a -> Wit (f a);
    };

    instance TestHetEquality Wit where
    {
        testHetEquality (SimpleWit iowa _) (SimpleWit iowb _) = testHetEquality iowa iowb;
        testHetEquality (ConsWit ica iaa) (ConsWit icb iab) = do
        {
            ReflH <- testHetEquality ica icb;
            ReflH <- testHetEquality iaa iab;
            return ReflH;
        };
        testHetEquality _ _ = Nothing;
    };

    instance TestEquality Wit where
    {
        testEquality wa wb = do
        {
            ReflH <- testHetEquality wa wb;
            return Refl;
        }
    };

    infoKnowledge :: Info t -> ConstraintKnowledge;
    infoKnowledge (MkInfo _ w) = witKnowledge w;

    witKnowledge :: Wit t -> ConstraintKnowledge;
    witKnowledge (SimpleWit _ k) = k;
    witKnowledge (ConsWit i1 i2) = mappend (infoKnowledge i1) (infoKnowledge i2);

    knowConstraint :: forall (c :: Constraint). c => Info c -> ConstraintKnowledge;
    knowConstraint info = know info MkConstraintWitness;

    askConstraint :: Info (c :: Constraint) -> Maybe (ConstraintWitness c);
    askConstraint info = ask (infoKnowledge info) info;

    splitInfo' :: forall (ka :: *) (f :: ka -> kfa) (a :: ka). Info (f a) -> Maybe (Info f,Info a);
    splitInfo' (MkInfo kfa (ConsWit infoF infoA)) = Just (infoF,infoA);
    splitInfo' _ = Nothing;

    applyInfo :: forall (ka :: *) (kb :: *) (f :: ka -> kb) (a :: ka). Info f -> Info a -> Info (f a);
    applyInfo tf@(MkInfo ika _) ta = MkInfo (snd $ fromMaybe (error "unexpected kind witness") $ splitInfo' ika) (ConsWit tf ta);

    splitInfo :: forall (r :: *) (kfa :: *) (fa :: kfa). Info fa ->
     (forall (ka :: *) (f :: ka -> kfa) (a :: ka). (f a ~ fa) => (Info f,Info a) -> r) ->
     Maybe r;
    splitInfo (MkInfo kfa (ConsWit infoF infoA)) ff = Just (ff (infoF,infoA));
    splitInfo _ _ = Nothing;

    splitInfoMaybe :: forall (r :: *) (kfa :: *) (fa :: kfa). Info fa ->
     (forall (ka :: *) (f :: ka -> kfa) (a :: ka). (f a ~ fa) => (Info f,Info a) -> Maybe r) ->
     Maybe r;
    splitInfoMaybe ifa ff = do
    {
        mr <- splitInfo ifa ff;
        mr;
    };
}

