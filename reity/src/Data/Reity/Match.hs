module Data.Reity.Match where
{
    import Data.Kind;
    import Data.Type.Heterogeneous;
    import Data.Result;
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

    data SplitInfo' (mf :: HetWit) (ma :: HetWit) :: HetWit where
    {
        MkSplitInfo' :: forall (mf :: HetWit) (ma :: HetWit) (kfa :: *) (ka :: *) (f :: ka -> kfa) (a :: ka). mf f -> ma a -> SplitInfo' mf ma (f a);
    };

    instance forall (mf :: HetWit) (ma :: HetWit). (MatchInfo mf,MatchInfo ma) => MatchInfo (SplitInfo' mf ma) where
    {
        matchInfo i@(MkInfo _ (ConsWit infoF infoA)) = kmContext ("splitting " ++ show i) $ do
        {
            f <- matchInfo infoF;
            a <- matchInfo infoA;
            return (MkSplitInfo' f a);
        };
        matchInfo info = kmError $ "couldn't split " ++ show info;
    };

    data SplitInfo (fa :: kfa) where
    {
        MkSplitInfo :: forall (ka :: *) (f :: ka -> kfa) (a :: ka). Info f -> Info a -> SplitInfo (f a);
    };

    instance MatchInfo SplitInfo where
    {
        matchInfo (MkInfo _ (ConsWit infoF infoA)) = pure (MkSplitInfo infoF infoA);
        matchInfo info = kmError $ "couldn't split " ++ show info;
    };

    data KindInfo :: HetWit where
    {
        MkKindInfo :: forall (ka :: *) (a :: ka). Info ka -> KindInfo a;
    };

    instance MatchInfo KindInfo where
    {
        matchInfo (MkInfo ki _) = pure $ MkKindInfo ki;
    };

    applyInfo :: forall (ka :: *) (kb :: *) (f :: ka -> kb) (a :: ka). Info f -> Info a -> Info (f a);
    applyInfo tf@(MkInfo ika _) ta = let
    {
        ik = case matchInfo ika of
        {
            SuccessResult (MkSplitInfo _ ik') -> ik';
            FailureResult _ -> error "unexpected kind witness";
        };
    } in MkInfo ik (ConsWit tf ta);
}
