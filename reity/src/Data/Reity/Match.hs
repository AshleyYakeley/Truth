module Data.Reity.Match where
{
    import Data.Kind;
    import Data.Type.Heterogeneous;
    import Data.Knowledge;
    import Data.Reity.KnowM;
    import Data.Reity.TypeInfo;


    sameTypeInfo :: forall (ka :: *) (kb :: *) (a :: ka) (b :: kb). TypeInfo a -> TypeInfo b -> KnowM (HetEq a b);
    sameTypeInfo ia ib = case testHetEquality ia ib of
    {
        Just ReflH -> return ReflH;
        Nothing -> kmError $ "couldn't match " ++ show ib ++ " with " ++ show ia;
    };

    class MatchTypeInfo (p :: HetWit) where
    {
        matchTypeInfo :: forall (ka :: *) (a :: ka). TypeInfo a -> KnowM (p a);
    };

    instance MatchTypeInfo TypeInfo where
    {
        matchTypeInfo = pure;
    };

    data IgnoreTypeInfo :: HetWit where
    {
        MkIgnoreTypeInfo :: forall (ka :: *) (a :: ka). IgnoreTypeInfo a;
    };

    instance MatchTypeInfo IgnoreTypeInfo where
    {
        matchTypeInfo _ = pure MkIgnoreTypeInfo;
    };

    instance MatchTypeInfo SplitTypeInfo where
    {
        matchTypeInfo i = case typeInfoSplit i of
        {
            Just split -> pure split;
            Nothing -> kmError $ "couldn't split " ++ show i;
        }
    };

    data SplitTypeInfo' (mf :: HetWit) (ma :: HetWit) :: HetWit where
    {
        MkSplitTypeInfo' :: forall (mf :: HetWit) (ma :: HetWit) (kfa :: *) (ka :: *) (f :: ka -> kfa) (a :: ka). mf f -> ma a -> SplitTypeInfo' mf ma (f a);
    };

    instance forall (mf :: HetWit) (ma :: HetWit). (MatchTypeInfo mf,MatchTypeInfo ma) => MatchTypeInfo (SplitTypeInfo' mf ma) where
    {
        matchTypeInfo i = kmContext ("splitting " ++ show i) $ do
        {
            MkSplitTypeInfo infoF infoA <- matchTypeInfo i;
            f <- matchTypeInfo infoF;
            a <- matchTypeInfo infoA;
            return (MkSplitTypeInfo' f a);
        };
    };

    data KindTypeInfo :: HetWit where
    {
        MkKindTypeInfo :: forall (ka :: *) (a :: ka). TypeInfo ka -> KindTypeInfo a;
    };

    instance MatchTypeInfo KindTypeInfo where
    {
        matchTypeInfo i = pure $ MkKindTypeInfo $ typeInfoKind i;
    };

    applyTypeInfo :: forall (ka :: *) (kb :: *) (f :: ka -> kb) (a :: ka). TypeInfo f -> TypeInfo a -> TypeInfo (f a);
    applyTypeInfo MkTypeInfo MkTypeInfo = MkTypeInfo;
}
