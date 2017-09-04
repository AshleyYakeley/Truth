{-# OPTIONS -fno-warn-orphans #-}
module Truth.UI.GTK.GView where
{
    import Shapes;
    import Graphics.UI.Gtk;
    import Data.Reity;
    import Truth.Core;


    makeButton :: String -> IO () -> IO Button;
    makeButton name action = do
    {
        button <- buttonNew;
        set button [buttonLabel := name];
        _ <- onClicked button action;
        return button;
    };

    type GView edit = View edit Widget;
    type GViewResult edit = ViewResult edit Widget;

    newtype GetUIView = MkGetUIView {getUIView :: forall edit. Edit edit => (forall edit'. Edit edit' => UISpec edit' -> GView edit') -> UISpec edit -> Maybe (GView edit)};

    instance Semigroup GetUIView where
    {
        (MkGetUIView p) <> (MkGetUIView q) = MkGetUIView $ \getview uispec -> case p getview uispec of
        {
            Just view -> Just view;
            Nothing -> q getview uispec;
        }
    };

    instance Monoid GetUIView where
    {
        mempty = MkGetUIView $ \_ _ -> Nothing;
        mappend = (<>);
    };

    lensUIView :: GetUIView;
    lensUIView = MkGetUIView $ \getview speca -> do
    {
        MkUILens specb lens <- isUISpec speca;
        return $ mapView lens $ getview specb;
    };

    instance HasTypeInfo Widget where
    {
        typeWitness = $(generateWitness [t|Widget|]);
        typeName _ = "Widget";
    };
}
