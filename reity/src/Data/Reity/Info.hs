module Data.Reity.Info where
{
    import Data.Kind;
    import Data.Type.Equality;
    import Data.Type.Heterogeneous;
    import Data.OpenWitness;
    import Data.Knowledge;
    import Data.Reity.KnowM;


    namedKnowledge :: forall (w :: HetWit) (f :: HetWit). String -> Knowledge KnowM w f -> Knowledge KnowM w f;
    namedKnowledge name (MkKnowledge ff) = MkKnowledge $ \k i -> kmContext name $ ff k i;

    data TypeFact (a :: k) where
    {
        ConstraintFact :: forall a. a => TypeFact a;
        ValueFact :: forall a. a -> TypeFact a;
    };

    type TypeKnowledge = Knowledge KnowM Info TypeFact;

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

    instance Show (Info a) where
    {
        show (MkInfo _ w) = show w;
    };

    infoKind :: forall (k :: *) (a :: k). Info a -> Info k;
    infoKind (MkInfo ki _) = ki;

    data Wit (t :: k) where
    {
        SimpleWit :: forall (k :: *) (t :: k). IOWitness t -> String -> TypeKnowledge -> Wit t;
        ConsWit :: forall (ka :: *) (kb :: *) (f :: ka -> kb) (a :: ka). Info f -> Info a -> Wit (f a);
    };

    instance TestHetEquality Wit where
    {
        testHetEquality (SimpleWit iowa _ _) (SimpleWit iowb _ _) = testHetEquality iowa iowb;
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

    instance Show (Wit a) where
    {
        show (SimpleWit _ name _) = name;
        show (ConsWit fi (MkInfo _ (SimpleWit _ name _))) = show fi ++ " " ++ name;
        show (ConsWit fi fa) = show fi ++ " (" ++ show fa ++ ")";
    };

    infoKnowledge :: Info t -> TypeKnowledge;
    infoKnowledge (MkInfo _ w) = witKnowledge w;

    witKnowledge :: Wit t -> TypeKnowledge;
    witKnowledge (SimpleWit _ _ k) = k;
    witKnowledge (ConsWit i1 i2) = mappend (infoKnowledge i1) (infoKnowledge i2);

    knowValue :: forall (t :: *). t -> Info t -> TypeKnowledge;
    knowValue t i = know i $ ValueFact t;

    knowConstraint :: forall (c :: Constraint). c => Info c -> TypeKnowledge;
    knowConstraint info = know info ConstraintFact;
}
