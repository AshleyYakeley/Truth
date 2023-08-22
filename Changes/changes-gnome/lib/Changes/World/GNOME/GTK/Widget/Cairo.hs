module Changes.World.GNOME.GTK.Widget.Cairo
    ( UIEvents(..)
    , UIDrawing
    , createCairo
    , onMouseEvent
    , fallThrough
    , onClick
    ) where

import Changes.Core
import Changes.World.GNOME.GI
import Data.IORef
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

eventsfallThrough :: UIEvents -> UIEvents
eventsfallThrough (MkUIEvents uie) = MkUIEvents $ \evt -> fmap (\_ -> False) $ uie evt

onMouseEvent :: ((Double, Double) -> EventButton -> GView 'Locked Bool) -> UIDrawing
onMouseEvent f = pointDrawing $ \p -> MkUIEvents $ f p

fallThrough :: UIDrawing -> UIDrawing
fallThrough = fmap $ fmap eventsfallThrough

onClick :: GView 'Locked () -> UIDrawing
onClick action =
    onMouseEvent $ \_ event -> do
        click <- GI.get event #type
        case click of
            EventTypeButtonPress -> do
                action
                return True
            _ -> return False

createCairo :: Model (ROWUpdate ((Int32, Int32) -> UIDrawing)) -> GView 'Unlocked Widget
createCairo model = do
    drawingRef :: IORef ((Int32, Int32) -> UIDrawing) <- gvLiftIONoUI $ newIORef $ \_ -> mempty
    gvRunLockedThen $ do
        (drawingArea, widget) <- gvNewWidget DrawingArea []
        let
            getDrawing :: IO UIDrawing
            getDrawing = do
                pdrawing <- readIORef drawingRef
                w <- #getAllocatedWidth drawingArea
                h <- #getAllocatedHeight drawingArea
                return $ pdrawing (w, h)
        _ <-
            gvOnSignal drawingArea #draw $ \context ->
                liftIO $ do
                    drawing <- getDrawing
                    renderWithContext (drawingRender drawing) context
                    return True
        widgetAddEvents drawingArea [EventMaskButtonPressMask]
        _ <-
            gvOnSignal drawingArea #buttonPressEvent $ \event -> do
                drawing <- liftIO getDrawing
                h <- GI.get event #x
                v <- GI.get event #y
                unUIEvents (drawingPoint drawing (h, v)) event
        return $ do
            gvBindReadOnlyWholeModel model $ \pdrawing ->
                gvLiftIONoUI $ do
                    writeIORef drawingRef pdrawing
                    #queueDraw drawingArea
            return widget
