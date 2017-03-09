module Data.Reity.Info where
{
    import Data.Type.Equality;
    import Data.Type.Heterogeneous;
    import Data.OpenWitness;
    import Data.Kind;
    import Data.KindCategory;
    import Data.Knowledge;


    data TypeFact (a :: k) where
    {
        ConstraintFact :: forall a. a => TypeFact a;
        ValueFact :: forall a. a -> TypeFact a;
    };

    type TypeKnowledge = Knowledge Info TypeFact;

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
        SimpleWit :: forall (k :: *) (t :: k). IOWitness t -> TypeKnowledge -> Wit t;
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

    infoKnowledge :: Info t -> TypeKnowledge;
    infoKnowledge (MkInfo _ w) = witKnowledge w;

    witKnowledge :: Wit t -> TypeKnowledge;
    witKnowledge (SimpleWit _ k) = k;
    witKnowledge (ConsWit i1 i2) = mappend (infoKnowledge i1) (infoKnowledge i2);

    knowValue :: forall (t :: *). t -> Info t -> TypeKnowledge;
    knowValue t i = know i $ ValueFact t;

    knowConstraint :: forall (c :: Constraint). c => Info c -> TypeKnowledge;
    knowConstraint info = know info ConstraintFact;

    askValue :: forall (a :: *). Info a -> Maybe a;
    askValue info = do
    {
        ValueFact a <- ask (infoKnowledge info) info;
        return a;
    };

    askConstraint :: forall (c :: Constraint). Info c -> Maybe (ConstraintWitness c);
    askConstraint info = do
    {
        ConstraintFact <- ask (infoKnowledge info) info;
        return MkConstraintWitness;
    };
}
