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
        FamilyWit :: forall (a :: *). Info a -> Wit (?family :: a);
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
        testHetEquality (FamilyWit ia) (FamilyWit ib) = do
        {
            ReflH <- testHetEquality ia ib;
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

    cwitFamily :: ConstraintWitness (?family :: a) -> a;
    cwitFamily MkConstraintWitness = ?family;

    pattern FamilyConstraintWitness :: a -> ConstraintWitness (?family :: a);
    pattern FamilyConstraintWitness x <- (cwitFamily -> x) where
    {
        FamilyConstraintWitness x = let {?family = x} in MkConstraintWitness;
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


    class MatchInfo (p :: forall k. k -> *) where
    {
        matchInfo :: forall (ka :: *) (a :: ka). Info a -> Maybe (p a);
    };

    instance MatchInfo Info where
    {
        matchInfo = Just;
    };

    data SplitInfo (fa :: kfa) where
    {
        MkSplitInfo :: forall (ka :: *) (f :: ka -> kfa) (a :: ka). Info f -> Info a -> SplitInfo (f a);
    };

    instance MatchInfo SplitInfo where
    {
        matchInfo (MkInfo kfa (ConsWit infoF infoA)) = Just (MkSplitInfo infoF infoA);
        matchInfo _ = Nothing;
    };

    data FamilyInfo (a :: ka) where
    {
        MkFamilyInfo :: forall (a :: *). Info a -> FamilyInfo (?family :: a);
    };

    instance MatchInfo FamilyInfo where
    {
        matchInfo (MkInfo _ (FamilyWit x)) = Just $ MkFamilyInfo x;
        matchInfo _ = Nothing;
    };

    applyInfo :: forall (ka :: *) (kb :: *) (f :: ka -> kb) (a :: ka). Info f -> Info a -> Info (f a);
    applyInfo tf@(MkInfo ika _) ta = case matchInfo ika of
    {
        Just (MkSplitInfo _ ik) -> MkInfo ik (ConsWit tf ta);
        Nothing -> error "unexpected kind witness";
    };
}
