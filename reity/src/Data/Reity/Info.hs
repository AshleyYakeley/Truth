module Data.Reity.Info where
{
    import Data.Kind;
    import Data.Proxy;
    import Data.Type.Equality;
    import Data.Type.Heterogeneous;
    import Data.Knowledge;
    import Data.Reity.KnowM;
    import Data.Reity.Wit;


    namedKnowledge :: forall (w :: HetWit) (f :: HetWit). String -> Knowledge KnowM w f -> Knowledge KnowM w f;
    namedKnowledge name (MkKnowledge ff) = MkKnowledge $ \k i -> kmContext name $ ff k i;

    data TypeFact (a :: k) where
    {
        ConstraintFact :: forall a. a => TypeFact a;
        ValueFact :: forall a. a -> TypeFact a;
    };

    type TypeKnowledge = Knowledge KnowM Info TypeFact;


    data SplitInfo (fa :: kfa) where
    {
        MkSplitInfo :: forall (ka :: *) (f :: ka -> kfa) (a :: ka). Info f -> Info a -> SplitInfo (f a);
    };

    class HasInfo (a :: k) where
    {
        typeKind :: proxy a -> Info k;

        default typeKind :: HasInfo k => proxy a -> Info k;
        typeKind _ = MkTypeInfo;

        typeWitness :: Wit a;

        typeName :: proxy a -> String;

        typeNameSingle :: proxy a -> String;
        typeNameSingle pa = typeName pa;

        typeNameApply :: proxy a -> String -> String;
        typeNameApply pa sa = typeName pa ++ " " ++ sa;

        typeKnowledge :: proxy a -> TypeKnowledge;
        typeKnowledge _ = mempty;

        typeSplit :: Maybe (SplitInfo a);
        typeSplit = Nothing;
    };

    instance (HasInfo f,HasInfo a) => HasInfo (f a) where
    {
        typeKind _ = case typeInfoSplit $ typeKind (Proxy @f) of
        {
            Just (MkSplitInfo _ ik) -> ik;
            Nothing -> error "unexpected kind witness";
        };
        typeWitness = ConsWit typeWitness typeWitness;
        typeName _ = typeNameApply (Proxy @f) $ typeNameSingle (Proxy @a);
        typeNameSingle pa = "(" ++ typeName pa ++ ")";
        typeKnowledge _ = mappend (typeKnowledge (Proxy @f)) (typeKnowledge (Proxy @a));
        typeSplit = Just $ MkSplitInfo MkTypeInfo MkTypeInfo;
    };


    --type Info (a :: k) = ConstraintWitness (HasInfo a);
    data Info (a :: k) where
    {
        MkTypeInfo :: forall (k :: Type) (a :: k). HasInfo a => Info a;
    };

    info :: HasInfo a => Info a;
    info = MkTypeInfo;

    infoKind :: forall (k :: *) (a :: k). Info a -> Info k;
    infoKind i@MkTypeInfo = typeKind i;

    typeInfoWitness :: Info a -> Wit a;
    typeInfoWitness MkTypeInfo = typeWitness;

    typeInfoSplit :: Info a -> Maybe (SplitInfo a);
    typeInfoSplit MkTypeInfo = typeSplit;

    --infoKind :: forall (k :: Type) (a :: k). Info a -> Info k;
    --infoKind MkConstraintWitness = typeKind;

    --pattern MkInfo :: forall (a :: k). Info k -> Wit a -> Info a;
    --pattern MkInfo ik wit <- ((\(i@MkInfo') -> (typeKind i,typeWitness)) -> (ik,wit));





{-
    data Info (t :: k) where
    {
        MkInfo :: forall (k :: *) (t :: k). Info k -> Wit t -> Info t;
    };
-}
    instance TestHetEquality Info where
    {
        testHetEquality ia ib = testHetEquality (typeInfoWitness ia) (typeInfoWitness ib);
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
        show i@MkTypeInfo = typeName i;
    };

    infoKnowledge :: Info t -> TypeKnowledge;
    infoKnowledge i@MkTypeInfo = typeKnowledge i;

    knowValue :: forall (t :: *). t -> Info t -> TypeKnowledge;
    knowValue t i = know i $ ValueFact t;

    knowConstraint :: forall (c :: Constraint). c => Info c -> TypeKnowledge;
    knowConstraint i = know i ConstraintFact;
}
