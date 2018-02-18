module Truth.UI.GTK.Tuple
    ( verticalLayoutGetView
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView

verticalLayoutGetView :: GetGView
verticalLayoutGetView =
    MkGetView $ \getview uispec -> do
        uilayout <- isUISpec uispec
        (aspects, orientation) <-
            return $
            case uilayout of
                MkUIHorizontal aspects -> (aspects, OrientationHorizontal)
                MkUIVertical aspects -> (aspects, OrientationVertical)
        return $ do
            widgets <-
                for aspects $ \(item, grow) -> do
                    w <- getview item
                    return (w, grow)
            liftIO $ arrangeWidgets orientation widgets

arrangeWidgets :: Orientation -> [(Widget, Bool)] -> IO Widget
arrangeWidgets orientation widgets = do
    vbox <- new Box [#orientation := orientation]
    for_ widgets $ \(widget, grow)
        --grow <- isScrollable widget
     -> do #packStart vbox widget grow grow 0
    toWidget vbox
