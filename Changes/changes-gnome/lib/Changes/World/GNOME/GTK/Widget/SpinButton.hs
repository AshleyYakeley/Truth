module Changes.World.GNOME.GTK.Widget.SpinButton where

import Changes.Core
import Changes.World.GNOME.GI
import GI.Gdk
import GI.Gtk as Gtk
import Shapes hiding (get)
import Shapes.Numeric

createSpinButton :: GView 'Locked (SpinButton, Widget)
createSpinButton = gvNewWidget SpinButton []

attachSpinButtonValue :: SpinButton -> Model (WholeUpdate Double) -> GView 'Unlocked ()
attachSpinButtonValue widget model = do
    esrc <- gvNewEditSource
    gvRunLockedThen $ do
        signal <-
            gvOnSignal widget #valueChanged $ do
                val <- get widget #value
                _ <- gvRunUnlocked $ gvSetWholeModel model esrc val
                return ()
        return $ do
            gvBindWholeModel model (Just esrc) $ \val ->
                gvRunLocked $ withSignalBlocked widget signal $ set widget [#value := val]

attachSpinButtonRange :: SpinButton -> Model (ROWUpdate (Double, Double)) -> GView 'Unlocked ()
attachSpinButtonRange widget model = do
    gvBindReadOnlyWholeModel model $ \(rlo, rhi) -> gvRunLocked $ spinButtonSetRange widget rlo rhi

attachSpinButtonIncrements :: SpinButton -> Model (ROWUpdate (Double, Double)) -> GView 'Unlocked ()
attachSpinButtonIncrements widget model =
    gvBindReadOnlyWholeModel model $ \(s, p) -> gvRunLocked $ spinButtonSetIncrements widget s p
