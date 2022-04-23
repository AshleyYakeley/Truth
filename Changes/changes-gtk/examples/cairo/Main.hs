module Main
    ( main
    ) where

import Changes.Core
import Changes.UI.GTK
import Changes.World.Clock
import Data.Time
import qualified GI.Gtk as GI
import Graphics.Cairo.Functional
import Shapes hiding (rotate)
import Shapes.Numeric

showPoint :: String -> UIDrawing
showPoint t =
    onMouseEvent $ \p _ -> do
        liftIO $ putStrLn $ t <> ": " <> show p
        return True

withShowPoint :: String -> UIDrawing -> UIDrawing
withShowPoint s d = mappend (fallThrough $ showPoint s) d

drawing :: TimeZone -> UTCTime -> (Int32, Int32) -> UIDrawing
drawing tz t (fromIntegral -> w, fromIntegral -> h) = let
    size = min w h
    LocalTime _ (TimeOfDay _ _ s) = utcToLocalTime tz t
    in translate (w / 2, h / 2) $
       scale (size, size) $
       scale (-0.5, -0.5) $
       rotate (realToFrac s * pi / 30) $
       operatorOver $
       lineCapSquare $
       sourceRGB (0.3, 0, 1) $ lineWidth 0.01 $ withShowPoint "P" $ stroke $ mconcat [moveTo (0, 0), lineTo (0, 1)]

zeroTime :: UTCTime
zeroTime = UTCTime (fromGregorian 2000 1 1) 0

main :: IO ()
main = do
    changesMainGTK $ \cc -> do
        let newWindow spec = ccExitOnClosed cc $ createWindow spec
        (clockModel, ()) <- viewLiftLifeCycle $ makeSharedModel $ clockPremodel zeroTime $ secondsToNominalDiffTime 1
        tz <- liftIO getCurrentTimeZone
        rec
            (_, closer) <-
                viewGetCloser $
                newWindow $ let
                    wsPosition = WindowPositionCenter
                    wsSize = (600, 600)
                    wsCloseBoxAction :: View ()
                    wsCloseBoxAction = liftIO closer
                    wsTitle :: Model (ROWUpdate Text)
                    wsTitle = constantModel "Cairo"
                    wsMenuBar :: Maybe (Model (ROWUpdate MenuBar))
                    wsMenuBar = Nothing
                    wsContent :: View Widget
                    wsContent = do
                        w1 <- createButton (constantModel "Button") (constantModel Nothing)
                        w2 <- createCairo $ mapModel (funcChangeLens $ drawing tz) clockModel
                        GI.set w2 [#marginStart GI.:= 100, #marginTop GI.:= 200]
                        createLayout
                            OrientationHorizontal
                            [(defaultLayoutOptions, w1), (defaultLayoutOptions {loGrow = True}, w2)]
                    in MkWindowSpec {..}
        return ()
