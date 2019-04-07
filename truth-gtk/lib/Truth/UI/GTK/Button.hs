module Truth.UI.GTK.Button
    ( buttonGetView
    ) where

import Data.IORef
import GI.Gdk
import GI.Gtk as Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful
import Truth.Debug.Object

createWidget ::
       EditFunction edit (WholeEdit Text) -> EditFunction edit (WholeEdit (Maybe (IO ()))) -> CreateView sel edit Widget
createWidget rlabel raction = do
    aref <- liftIO $ newIORef Nothing
    widget <- new Button []
    cvBindEditFunction Nothing rlabel $ \label -> set widget [#label := label]
    traceBracket "GTK.Button:create.bind" $ cvBindEditFunction Nothing raction $ \maction ->
        liftIO $ do
            writeIORef aref maction
            set widget [#sensitive := isJust maction]
    _ <-
        cvLiftView $
        viewOn widget #clicked $
        traceBracket "GTK.Button:click" $
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
