module Changes.World.GNOME.GTK.Widget.SpinButton where

import Shapes.Numeric

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

createSpinButton :: GView 'Locked (GI.SpinButton, GI.Widget)
createSpinButton = gvNewWidget GI.SpinButton []

attachSpinButtonValue :: GI.SpinButton -> Model (WholeUpdate Double) -> GView 'Unlocked ()
attachSpinButtonValue widget model = do
    esrc <- gvNewEditSource
    gvRunLockedThen $ do
        signal <-
            gvOnSignal widget #valueChanged $ do
                val <- GI.get widget #value
                _ <- gvRunUnlocked $ gvSetWholeModel model esrc val
                return ()
        return $ do
            gvBindWholeModel model (Just esrc) $ \val ->
                gvRunLocked $ withSignalBlocked widget signal $ GI.set widget [#value GI.:= val]

attachSpinButtonRange :: GI.SpinButton -> Model (ROWUpdate (Double, Double)) -> GView 'Unlocked ()
attachSpinButtonRange widget model = do
    gvBindReadOnlyWholeModel model $ \(rlo, rhi) -> gvRunLocked $ GI.spinButtonSetRange widget rlo rhi

attachSpinButtonIncrements :: GI.SpinButton -> Model (ROWUpdate (Double, Double)) -> GView 'Unlocked ()
attachSpinButtonIncrements widget model =
    gvBindReadOnlyWholeModel model $ \(s, p) -> gvRunLocked $ GI.spinButtonSetIncrements widget s p
