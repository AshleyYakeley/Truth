module Truth.UI.GTK.Tuple
    ( verticalLayoutGetView
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

verticalLayoutGetView :: GetGView
verticalLayoutGetView =
    MkGetView $ \getview uispec -> do
        MkUIVertical aspects <- isUISpec uispec
        return $ do
            widgets <- for aspects getview
            liftIO $ arrangeWidgets widgets

arrangeWidgets :: [Widget] -> IO Widget
arrangeWidgets widgets = do
    vbox <- new Box [#orientation := OrientationVertical]
    for_ widgets $ \widget -> do
        grow <- isScrollable widget
        #packStart vbox widget grow grow 0
    toWidget vbox
