module Data.Reity.TypeInfo where
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

    type TypeKnowledge = Knowledge KnowM TypeInfo TypeFact;


    data SplitTypeInfo (fa :: kfa) where
    {
        MkSplitTypeInfo :: forall (ka :: *) (f :: ka -> kfa) (a :: ka). TypeInfo f -> TypeInfo a -> SplitTypeInfo (f a);
    };

    class HasTypeInfo (a :: k) where
    {
        typeKind :: proxy a -> TypeInfo k;

        default typeKind :: HasTypeInfo k => proxy a -> TypeInfo k;
        typeKind _ = MkTypeInfo;

        typeWitness :: Wit a;

        typeName :: proxy a -> String;

        typeNameSingle :: proxy a -> String;
        typeNameSingle pa = typeName pa;

        typeNameApply :: proxy a -> String -> String;
        typeNameApply pa sa = typeName pa ++ " " ++ sa;

        typeKnowledge :: proxy a -> TypeKnowledge;
        typeKnowledge _ = mempty;

        typeSplit :: Maybe (SplitTypeInfo a);
        typeSplit = Nothing;
    };

    instance (HasTypeInfo f,HasTypeInfo a) => HasTypeInfo (f a) where
    {
        typeKind _ = case typeInfoSplit $ typeKind (Proxy @f) of
        {
            Just (MkSplitTypeInfo _ ik) -> ik;
            Nothing -> error "unexpected kind witness";
        };
        typeWitness = ConsWit typeWitness typeWitness;
        typeName _ = typeNameApply (Proxy @f) $ typeNameSingle (Proxy @a);
        typeNameSingle pa = "(" ++ typeName pa ++ ")";
        typeKnowledge _ = mappend (typeKnowledge (Proxy @f)) (typeKnowledge (Proxy @a));
        typeSplit = Just $ MkSplitTypeInfo MkTypeInfo MkTypeInfo;
    };

    data TypeInfo (a :: k) where
    {
        MkTypeInfo :: forall (k :: Type) (a :: k). HasTypeInfo a => TypeInfo a;
    };

    typeInfo :: HasTypeInfo a => TypeInfo a;
    typeInfo = MkTypeInfo;

    typeInfoKind :: forall (k :: *) (a :: k). TypeInfo a -> TypeInfo k;
    typeInfoKind i@MkTypeInfo = typeKind i;

    typeInfoWitness :: TypeInfo a -> Wit a;
    typeInfoWitness MkTypeInfo = typeWitness;

    typeInfoKnowledge :: TypeInfo t -> TypeKnowledge;
    typeInfoKnowledge i@MkTypeInfo = typeKnowledge i;

    typeInfoSplit :: TypeInfo a -> Maybe (SplitTypeInfo a);
    typeInfoSplit MkTypeInfo = typeSplit;

    instance TestHetEquality TypeInfo where
    {
        testHetEquality ia ib = testHetEquality (typeInfoWitness ia) (typeInfoWitness ib);
    };

    instance TestEquality TypeInfo where
    {
        testEquality wa wb = do
        {
            ReflH <- testHetEquality wa wb;
            return Refl;
        }
    };

    instance Show (TypeInfo a) where
    {
        show i@MkTypeInfo = typeName i;
    };

    askTypeInfo :: forall (k :: *) (a :: k). TypeKnowledge -> TypeInfo a -> KnowM (TypeFact a);
    askTypeInfo k i = kmContext (show i) $ ask k i;

    knowValue :: forall (t :: *). t -> TypeInfo t -> TypeKnowledge;
    knowValue t i = know i $ ValueFact t;

    knowConstraint :: forall (c :: Constraint). c => TypeInfo c -> TypeKnowledge;
    knowConstraint i = know i ConstraintFact;
}
