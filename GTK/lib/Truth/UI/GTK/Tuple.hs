module Truth.UI.GTK.Tuple(tupleMatchView) where
{
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

    tupleGView :: FiniteTupleSelector sel => (forall edit. sel edit -> Maybe (GView edit)) -> Maybe (GView (TupleEdit sel));
    tupleGView selview = fmap (mapIOView arrangeWidgets) $ tupleView selview;

    tupleMatchView :: MatchView -> MatchView;
    tupleMatchView allviews tedit = do
    {
        MkSplitInfo ite isel <- matchInfo tedit;
        ReflH <- testHetEquality (info @TupleEdit) ite;
        ConstraintFact <- ask (infoKnowledge isel) $ applyInfo (info @FiniteTupleSelector) isel;
        ConstraintFact <- ask (infoKnowledge isel) $ applyInfo (info @TupleHasInfo) isel;
        tupleGView $ \sel -> case tupleIsEdit sel of
        {
            MkConstraintWitness -> allviews $ tupleHasInfo sel;
        };
    };
}
