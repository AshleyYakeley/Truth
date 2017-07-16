module Truth.UI.GTK.Tuple(tupleMatchView) where
{
    import Data.Proxy;
    import Data.Foldable;
    import Graphics.UI.Gtk;
    import Data.Type.Heterogeneous;
    import Data.KindCategory;
    import Data.Reity;
    import Truth.Core;
    import Truth.UI.GTK.GView;


    arrangeWidgets :: [Widget] -> IO Widget;
    arrangeWidgets widgets = do
    {
        vbox <- vBoxNew False 0;
        for_ widgets $ \widget -> boxPackEnd vbox widget PackNatural 0;
        return $ toWidget vbox;
    };

    tupleGView :: (FiniteTupleSelector sel,Applicative m) => (forall edit. sel edit -> m (GView edit)) -> m (GView (TupleEdit sel));
    tupleGView selview = fmap (mapIOView arrangeWidgets) $ tupleView selview;

    tupleMatchView :: MatchView -> MatchView;
    tupleMatchView (MkMatchView allviews) = namedMatchView "tuple" $ \tedit -> do
    {
        MkSplitInfo ite isel <- matchInfoNamed tedit;
        ReflH <- testHetEqualityNamed (info @TupleEdit) ite;
        ConstraintFact <- askNamed (infoKnowledge isel) $ applyInfo (info @FiniteTupleSelector) isel;
        ConstraintFact <- askNamed (infoKnowledge isel) $ applyInfo (info @TupleHasInfo) isel;
        tupleGView $ \sel -> case tupleWitness (Proxy :: Proxy Edit) sel of
        {
            MkConstraintWitness -> namedResult "selector" $ allviews $ tupleHasInfo sel;
        };
    };
}
