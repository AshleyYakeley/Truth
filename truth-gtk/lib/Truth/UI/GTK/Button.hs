module Truth.UI.GTK.Button
    ( buttonGetView
    ) where

import Data.IORef
import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

createWidget ::
       ReadOnlySubscriber (WholeUpdate Text)
    -> ReadOnlySubscriber (WholeUpdate (Maybe (IO ())))
    -> CreateView sel Widget
createWidget rlabel raction = do
    aref <- liftIO $ newIORef Nothing
    widget <- new Button []
    cvBindUpdateFunction Nothing rlabel $ \label -> set widget [#label := label]
    cvBindUpdateFunction Nothing raction $ \maction ->
        liftIO $ do
            writeIORef aref maction
            set widget [#sensitive := isJust maction]
    _ <-
        cvLiftView $
        viewOn widget #clicked $
        liftIO $ do
            maction <- readIORef aref
            case maction of
                Nothing -> return ()
                Just action -> action
    toWidget widget

buttonGetView :: GetGView
buttonGetView =
    MkGetView $ \_ uispec -> do
        MkButtonUISpec rlabel raction <- isUISpec uispec
        return $ createWidget rlabel raction
