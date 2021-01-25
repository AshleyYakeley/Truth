module Changes.UI.GTK.Cairo
    ( createCairo
    ) where

import Changes.Core
import Changes.UI.GTK.Useful
import Data.IORef
import GI.Cairo.Render
import GI.Cairo.Render.Connector
import GI.Gtk
import Shapes

createCairo :: Model (ROWUpdate (DrawingArea -> Render ())) -> CreateView Widget
createCairo model = do
    widget <- cvNew DrawingArea []
    renderRef <- liftIO $ newIORef $ \_ -> return ()
    cvBindReadOnlyWholeModel model $ \render -> do
        liftIO $ writeIORef renderRef render
        #queueDraw widget
    _ <-
        cvOn widget #draw $ \context ->
            liftIO $ do
                render <- readIORef renderRef
                renderWithContext (render widget) context
                return True
    toWidget widget
