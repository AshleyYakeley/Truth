module Main
    ( main
    ) where

import Changes.Core
import Changes.UI.GTK
import Changes.World.Clock
import Data.Time
import GI.Cairo.Render
import GI.Gtk.Objects.DrawingArea
import Shapes hiding (rotate)
import Shapes.Numeric

render :: TimeZone -> UTCTime -> DrawingArea -> Render ()
render tz t widget = do
    w <- #getAllocatedWidth widget
    h <- #getAllocatedHeight widget
    -- move to middle
    translate (fromIntegral w / 2) (fromIntegral h / 2)
    let
    -- fit to window
        size = min w h
    scale (fromIntegral size) (fromIntegral size)
    -- set rotating frame of reference, y-axis up
    scale (-0.5) (-0.5)
    let LocalTime _ (TimeOfDay _ _ s) = utcToLocalTime tz t
    rotate (realToFrac s * pi / 30)
    -- set up pen
    setOperator OperatorOver
    setLineCap LineCapSquare
    setSourceRGB 0.3 0 1 -- purple
    setLineWidth 0.01
    -- make line
    moveTo 0 0
    lineTo 0 1
    -- draw
    stroke

zeroTime :: UTCTime
zeroTime = UTCTime (fromGregorian 2000 1 1) 0

main :: IO ()
main = do
    changesMainGTK $ \cc -> do
        let newWindow spec = ccExitOnClosed cc $ createWindow spec
        (clockModel, ()) <- liftLifeCycle $ makeSharedModel $ clockPremodel zeroTime $ secondsToNominalDiffTime 1
        tz <- liftIO getCurrentTimeZone
        rec
            (_, closer) <-
                lifeCycleEarlyCloser $
                newWindow $ let
                    wsPosition = WindowPositionCenter
                    wsSize = (600, 600)
                    wsCloseBoxAction :: View ()
                    wsCloseBoxAction = liftIO closer
                    wsTitle :: Model (ROWUpdate Text)
                    wsTitle = constantModel "Cairo"
                    wsMenuBar :: Maybe (Model (ROWUpdate MenuBar))
                    wsMenuBar = Nothing
                    wsContent :: CreateView Widget
                    wsContent = createCairo $ mapModel (funcChangeLens $ render tz) clockModel
                    in MkWindowSpec {..}
        return ()
