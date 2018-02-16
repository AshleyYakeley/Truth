module Truth.UI.GTK.Labelled
    ( labelledGetView
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView

labelledGetView :: GetGView
labelledGetView =
    MkGetView $ \getview uispec -> do
        MkUILabelled text spec <- isUISpec uispec
        return $ do
            view <- getview spec
            labelWidget text view

labelWidget :: MonadIO m => Text -> Widget -> m Widget
labelWidget text widget = do
    box <- new Box [#orientation := OrientationHorizontal]
    label <- new Label [#label := text <> ": "]
    #packStart box label False False 0
    #packStart box widget True True 0
    toWidget box
