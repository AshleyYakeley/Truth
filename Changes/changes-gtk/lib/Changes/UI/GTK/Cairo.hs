module Changes.UI.GTK.Cairo
    ( UIEvents
    , UIDrawing
    , createCairo
    , onMouseEvent
    , fallThrough
    , onClick
    ) where

import Changes.Core
import Changes.GI
import Data.IORef
import GI.Cairo.Render
import GI.Cairo.Render.Connector
import GI.Gdk as GI
import GI.Gtk as GI
import Graphics.Cairo.Functional
import Shapes
import Shapes.Numeric

data UIEvents = MkUIEvents
    { unUIEvents :: EventButton -> GView 'Locked Bool
    }

instance Semigroup UIEvents where
    MkUIEvents evta <> MkUIEvents evtb =
        MkUIEvents $ \et -> do
            sa <- evta et
            if sa
                then return True
                else evtb et

instance Monoid UIEvents where
    mempty = MkUIEvents $ \_ -> return False

type UIDrawing = Drawing (PixelPoint -> UIEvents)

onMouseEvent :: ((Double, Double) -> EventButton -> GView 'Locked Bool) -> UIDrawing
onMouseEvent f = pointDrawing $ \p -> MkUIEvents $ f p

fallThrough :: UIDrawing -> UIDrawing
fallThrough = fmap $ \f p -> MkUIEvents $ \evt -> fmap (\_ -> False) $ (unUIEvents $ f p) evt

onClick :: GView 'Locked () -> UIDrawing
onClick action =
    onMouseEvent $ \_ event -> do
        click <- GI.get event #type
        case click of
            EventTypeButtonPress -> do
                action
                return True
            _ -> return False

createCairo :: Model (ROWUpdate ((Int32, Int32) -> UIDrawing)) -> GView 'Locked Widget
createCairo model = do
    widget <- gvNew DrawingArea []
    drawingRef :: IORef ((Int32, Int32) -> UIDrawing) <- liftIO $ newIORef $ \_ -> mempty
    gvBindReadOnlyWholeModel model $ \pdrawing ->
        gvRunLocked $ do
            liftIO $ writeIORef drawingRef pdrawing
            #queueDraw widget
    let
        getDrawing :: IO UIDrawing
        getDrawing = do
            pdrawing <- readIORef drawingRef
            w <- #getAllocatedWidth widget
            h <- #getAllocatedHeight widget
            return $ pdrawing (w, h)
    _ <-
        gvOnSignal widget #draw $ \context ->
            liftIO $ do
                drawing <- getDrawing
                renderWithContext (drawingRender drawing) context
                return True
    widgetAddEvents widget [EventMaskButtonPressMask]
    _ <-
        gvOnSignal widget #buttonPressEvent $ \event -> do
            drawing <- liftIO getDrawing
            h <- GI.get event #x
            v <- GI.get event #y
            unUIEvents (drawingPoint drawing (h, v)) event
    toWidget widget
