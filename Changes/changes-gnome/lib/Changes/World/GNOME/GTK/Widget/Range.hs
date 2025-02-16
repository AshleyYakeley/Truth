module Changes.World.GNOME.GTK.Widget.Range where

import Shapes.Numeric

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

attachRangeValue :: GI.Range -> Model (WholeUpdate Double) -> GView 'Unlocked ()
attachRangeValue widget model = do
    esrc <- gvNewEditSource
    gvRunLockedThen $ do
        signal <-
            gvOnSignal widget #valueChanged $ do
                val <- GI.rangeGetValue widget
                _ <- gvRunUnlocked $ gvSetWholeModel model esrc val
                return ()
        return $ do
            gvBindWholeModel model (Just esrc) $ \val ->
                gvRunLocked $ withSignalBlocked widget signal $ GI.rangeSetValue widget val

attachRangeRange :: GI.Range -> Model (ROWUpdate (Double, Double)) -> GView 'Unlocked ()
attachRangeRange widget model =
    gvBindReadOnlyWholeModel model $ \(vlo, vhi) -> gvRunLocked $ GI.rangeSetRange widget vlo vhi

attachRangeIncrements :: GI.Range -> Model (ROWUpdate (Double, Double)) -> GView 'Unlocked ()
attachRangeIncrements widget model =
    gvBindReadOnlyWholeModel model $ \(s, p) -> gvRunLocked $ GI.rangeSetIncrements widget s p
