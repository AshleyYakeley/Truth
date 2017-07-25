module Data.Reity.Match where
{
    import Data.Kind;
    import Data.Type.Heterogeneous;
    import Data.Knowledge;
    import Data.Reity.KnowM;
    import Data.Reity.Info;


    sameInfo :: forall (ka :: *) (kb :: *) (a :: ka) (b :: kb). Info a -> Info b -> KnowM (HetEq a b);
    sameInfo ia ib = case testHetEquality ia ib of
    {
        Just ReflH -> return ReflH;
        Nothing -> kmError $ "couldn't match " ++ show ib ++ " with " ++ show ia;
    };

    class MatchInfo (p :: HetWit) where
    {
        matchInfo :: forall (ka :: *) (a :: ka). Info a -> KnowM (p a);
    };

    instance MatchInfo Info where
    {
        matchInfo = pure;
    };

    data IgnoreInfo :: HetWit where
    {
        MkIgnoreInfo :: forall (ka :: *) (a :: ka). IgnoreInfo a;
    };

    instance MatchInfo IgnoreInfo where
    {
        matchInfo _ = pure MkIgnoreInfo;
    };

    instance MatchInfo SplitInfo where
    {
        matchInfo i = case typeInfoSplit i of
        {
            Just split -> pure split;
            Nothing -> kmError $ "couldn't split " ++ show i;
        }
    };

    data SplitInfo' (mf :: HetWit) (ma :: HetWit) :: HetWit where
    {
        MkSplitInfo' :: forall (mf :: HetWit) (ma :: HetWit) (kfa :: *) (ka :: *) (f :: ka -> kfa) (a :: ka). mf f -> ma a -> SplitInfo' mf ma (f a);
    };

    instance forall (mf :: HetWit) (ma :: HetWit). (MatchInfo mf,MatchInfo ma) => MatchInfo (SplitInfo' mf ma) where
    {
        matchInfo i = kmContext ("splitting " ++ show i) $ do
        {
            MkSplitInfo infoF infoA <- matchInfo i;
            f <- matchInfo infoF;
            a <- matchInfo infoA;
            return (MkSplitInfo' f a);
        };
    };

    data KindInfo :: HetWit where
    {
        MkKindInfo :: forall (ka :: *) (a :: ka). Info ka -> KindInfo a;
    };

    instance MatchInfo KindInfo where
    {
        matchInfo i = pure $ MkKindInfo $ infoKind i;
    };

    applyInfo :: forall (ka :: *) (kb :: *) (f :: ka -> kb) (a :: ka). Info f -> Info a -> Info (f a);
    applyInfo MkTypeInfo MkTypeInfo = MkTypeInfo;
    {-
    applyInfo tf@(MkInfo ika _) ta = let
    {
        ik = case matchInfo ika of
        {
            SuccessResult (MkSplitInfo _ ik') -> ik';
            FailureResult _ -> error "unexpected kind witness";
        };
    } in MkInfo ik (ConsWit tf ta);
    -}
}
