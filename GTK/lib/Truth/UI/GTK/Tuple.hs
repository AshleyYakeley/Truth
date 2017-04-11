module Truth.UI.GTK.Tuple(tupleMatchView) where
{
    import Data.Foldable;
    import Data.Type.Equality;
    import Graphics.UI.Gtk;
    import Data.Type.Heterogeneous;
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

    tupleGView :: FiniteTupleSelector sel => (forall edit. sel edit -> GView edit) -> GView (TupleEdit sel);
    tupleGView selview = mapIOView arrangeWidgets $ tupleView selview;

    tupleMatchView :: MatchView;
    tupleMatchView tedit = do
    {
        MkSplitInfo ite _isel <- matchInfo tedit;
        ReflH <- testHetEquality (info @TupleEdit) ite;
        return $ tupleGView selview;
    };
}
