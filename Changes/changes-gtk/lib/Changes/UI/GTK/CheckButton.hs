module Changes.UI.GTK.CheckButton
    ( createCheckButton
    , createMaybeCheckButton
    ) where

import Changes.Core
import Changes.GI
import GI.Gdk
import GI.Gtk as Gtk
import Shapes

createCheckButton :: Model (ROWUpdate Text) -> Model (WholeUpdate Bool) -> GView 'Locked Widget
createCheckButton label rmod = do
    esrc <- newEditSource
    initial <- gvRunResource rmod $ \asub -> aModelRead asub ReadWhole
    widget <- gvNew CheckButton [#active := initial]
    gvBindReadOnlyWholeModel label $ \val -> gvLiftIO $ set widget [#label := val]
    changedSignal <-
        gvOnSignal widget #clicked $ do
            st <- gvLiftIO $ Gtk.get widget #active
            gvRunResource rmod $ \asub -> do
                _ <- pushEdit esrc $ aModelEdit asub $ pure $ MkWholeReaderEdit st
                return ()
    gvBindWholeModel rmod (Just esrc) $ \st -> withSignalBlocked widget changedSignal $ set widget [#active := st]
    toWidget widget

createMaybeCheckButton :: Model (ROWUpdate Text) -> Model (WholeUpdate (Maybe Bool)) -> GView 'Locked Widget
createMaybeCheckButton label rmod = do
    initial <- gvRunResource rmod $ \asub -> aModelRead asub ReadWhole
    widget <- gvNew CheckButton [#active := initial == Just True, #inconsistent := initial == Nothing]
    gvBindReadOnlyWholeModel label $ \val -> gvLiftIO $ set widget [#label := val]
    let
        getWidgetState :: GView 'Locked (Maybe Bool)
        getWidgetState = do
            active <- Gtk.get widget #active
            inconsistent <- Gtk.get widget #inconsistent
            return $
                if inconsistent
                    then Nothing
                    else Just active
        setWidgetState :: Maybe Bool -> GView 'Locked ()
        setWidgetState st = set widget [#active := st == Just True, #inconsistent := st == Nothing]
    _ <-
        gvOnSignal widget #buttonPressEvent $ \event -> do
            click <- Gtk.get event #type
            case click of
                EventTypeButtonPress -> do
                    modifiers <- Gtk.get event #state
                    oldst <- getWidgetState
                    let
                        newst =
                            if elem ModifierTypeShiftMask modifiers
                                then Nothing
                                else Just (oldst /= Just True)
                    _ <-
                        gvRunResource rmod $ \asub ->
                            pushEdit noEditSource $ aModelEdit asub $ pure $ MkWholeReaderEdit newst
                    return True
                _ -> return False
    gvBindWholeModel rmod Nothing $ \mb -> setWidgetState mb
    toWidget widget
