module Data.Reity.Match where
{
    import Data.Kind;
    import Data.Type.Heterogeneous;
    import Data.Knowledge;
    import Data.Reity.Info;


    sameInfo :: forall (ka :: *) (kb :: *) (a :: ka) (b :: kb). Info a -> Info b -> Maybe (HetEq a b);
    sameInfo = testHetEquality;

    class MatchInfo (p :: HetWit) where
    {
        matchInfo :: forall (ka :: *) (a :: ka). Info a -> Maybe (p a);
    };

    instance MatchInfo Info where
    {
        matchInfo = Just;
    };

    data IgnoreInfo :: HetWit where
    {
        MkIgnoreInfo :: forall (ka :: *) (a :: ka). IgnoreInfo a;
    };

    instance MatchInfo IgnoreInfo where
    {
        matchInfo _ = Just MkIgnoreInfo;
    };

    data SplitInfo' (mf :: HetWit) (ma :: HetWit) :: HetWit where
    {
        MkSplitInfo' :: forall (mf :: HetWit) (ma :: HetWit) (kfa :: *) (ka :: *) (f :: ka -> kfa) (a :: ka). mf f -> ma a -> SplitInfo' mf ma (f a);
    };

    instance forall (mf :: HetWit) (ma :: HetWit). (MatchInfo mf,MatchInfo ma) => MatchInfo (SplitInfo' mf ma) where
    {
        matchInfo (MkInfo _ (ConsWit infoF infoA)) = do
        {
            f <- matchInfo infoF;
            a <- matchInfo infoA;
            return (MkSplitInfo' f a);
        };
        matchInfo _ = Nothing;
    };

    data SplitInfo (fa :: kfa) where
    {
        MkSplitInfo :: forall (ka :: *) (f :: ka -> kfa) (a :: ka). Info f -> Info a -> SplitInfo (f a);
    };

    instance MatchInfo SplitInfo where
    {
        matchInfo (MkInfo _ (ConsWit infoF infoA)) = Just (MkSplitInfo infoF infoA);
        matchInfo _ = Nothing;
    };

    data KindInfo :: HetWit where
    {
        MkKindInfo :: forall (ka :: *) (a :: ka). Info ka -> KindInfo a;
    };

    instance MatchInfo KindInfo where
    {
        matchInfo (MkInfo ki _) = Just $ MkKindInfo ki;
    };

    applyInfo :: forall (ka :: *) (kb :: *) (f :: ka -> kb) (a :: ka). Info f -> Info a -> Info (f a);
    applyInfo tf@(MkInfo ika _) ta = let
    {
        ik = case matchInfo ika of
        {
            Just (MkSplitInfo _ ik') -> ik';
            Nothing -> error "unexpected kind witness";
        };
    } in MkInfo ik (ConsWit tf ta);
}
