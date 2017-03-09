module Data.Reity.Info where
{
    import Data.Type.Equality;
    import Data.Type.Heterogeneous;
    import Data.OpenWitness;
    import Data.Kind;
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
    witKnowledge (FamilyWit i) = infoKnowledge i;

    knowConstraint :: forall (c :: Constraint). c => Info c -> ConstraintKnowledge;
    knowConstraint info = know info MkConstraintWitness;

    askConstraint :: Info (c :: Constraint) -> Maybe (ConstraintWitness c);
    askConstraint info = ask (infoKnowledge info) info;
}
