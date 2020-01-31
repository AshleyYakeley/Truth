module Truth.UI.GTK.Layout
    ( layoutGetView
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView

layoutGetView :: GetGView
layoutGetView =
    MkGetView $ \getview uispec -> do
        uilayout <- isUISpec uispec
        (aspects, orientation) <-
            return $
            case uilayout of
                HorizontalUISpec aspects -> (aspects, OrientationHorizontal)
                VerticalUISpec aspects -> (aspects, OrientationVertical)
        return $ do
            widgets <-
                for aspects $ \(ispec, grow) -> do
                    w <- getview ispec
                    return (w, grow)
            vbox <- new Box [#orientation := orientation]
            for_ widgets $ \(widget, grow) -> #packStart vbox widget grow grow 0
            toWidget vbox
