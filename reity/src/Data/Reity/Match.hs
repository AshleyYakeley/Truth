module Data.Reity.Match where
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
    import Data.Reity.Info;


    sameInfo :: forall (ka :: *) (kb :: *) (a :: ka) (b :: kb). Info a -> Info b -> Maybe (HetEq a b);
    sameInfo = testHetEquality;

    type HWit = forall k. k -> *;

    class MatchInfo (p :: HWit) where
    {
        matchInfo :: forall (ka :: *) (a :: ka). Info a -> Maybe (p a);
    };

    instance MatchInfo Info where
    {
        matchInfo = Just;
    };

    data IgnoreInfo :: HWit where
    {
        MkIgnoreInfo :: forall (ka :: *) (a :: ka). IgnoreInfo a;
    };

    instance MatchInfo IgnoreInfo where
    {
        matchInfo _ = Just MkIgnoreInfo;
    };

    data SplitInfo' (mf :: HWit) (ma :: HWit) :: HWit where
    {
        MkSplitInfo' :: forall (mf :: HWit) (ma :: HWit) (kfa :: *) (ka :: *) (f :: ka -> kfa) (a :: ka). mf f -> ma a -> SplitInfo' mf ma (f a);
    };

    instance forall (mf :: HWit) (ma :: HWit). (MatchInfo mf,MatchInfo ma) => MatchInfo (SplitInfo' mf ma) where
    {
        matchInfo (MkInfo kfa (ConsWit infoF infoA)) = do
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
        matchInfo (MkInfo kfa (ConsWit infoF infoA)) = Just (MkSplitInfo infoF infoA);
        matchInfo _ = Nothing;
    };

    data FamilyInfo :: HWit where
    {
        MkFamilyInfo :: forall (a :: *). Info a -> FamilyInfo (?family :: a);
    };

    instance MatchInfo FamilyInfo where
    {
        matchInfo (MkInfo _ (FamilyWit x)) = Just $ MkFamilyInfo x;
        matchInfo _ = Nothing;
    };

    data KindInfo :: HWit where
    {
        MkKindInfo :: forall (ka :: *) (a :: ka). Info ka -> KindInfo a;
    };

    instance MatchInfo KindInfo where
    {
        matchInfo (MkInfo ki _) = Just $ MkKindInfo ki;
    };

    applyInfo :: forall (ka :: *) (kb :: *) (f :: ka -> kb) (a :: ka). Info f -> Info a -> Info (f a);
    applyInfo tf@(MkInfo ika _) ta = case matchInfo ika of
    {
        Just (MkSplitInfo _ ik) -> MkInfo ik (ConsWit tf ta);
        Nothing -> error "unexpected kind witness";
    };
}
