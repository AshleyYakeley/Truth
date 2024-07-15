module Changes.World.GNOME.GTK.Widget.Range where

import Changes.Core
import Changes.World.GNOME.GI
import GI.Gtk as Gtk
import Shapes hiding (get)
import Shapes.Numeric

attachRangeValue :: Range -> Model (WholeUpdate Double) -> GView 'Unlocked ()
attachRangeValue widget model = do
    esrc <- gvNewEditSource
    gvRunLockedThen $ do
        signal <-
            gvOnSignal widget #valueChanged $ do
                val <- rangeGetValue widget
                _ <- gvRunUnlocked $ gvSetWholeModel model esrc val
                return ()
        return $ do
            gvBindWholeModel model (Just esrc) $ \val ->
                gvRunLocked $ withSignalBlocked widget signal $ rangeSetValue widget val

attachRangeRange :: Range -> Model (ROWUpdate (Double, Double)) -> GView 'Unlocked ()
attachRangeRange widget model =
    gvBindReadOnlyWholeModel model $ \(vlo, vhi) -> gvRunLocked $ rangeSetRange widget vlo vhi

attachRangeIncrements :: Range -> Model (ROWUpdate (Double, Double)) -> GView 'Unlocked ()
attachRangeIncrements widget model =
    gvBindReadOnlyWholeModel model $ \(s, p) -> gvRunLocked $ rangeSetIncrements widget s p
