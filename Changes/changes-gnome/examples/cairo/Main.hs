module Main
    ( main
    )
where

import Changes.Core
import Changes.World.Clock
import Data.Time
import GI.Gtk qualified as GI
import Graphics.Cairo.Functional
import Shapes hiding (rotate)
import Shapes.Numeric

import Changes.World.GNOME.GTK

type UIDrawing a = Drawing (PixelPoint -> Alt Maybe a)

createUIDrawing :: forall a. Model (ROWUpdate ((Int32, Int32) -> UIDrawing a)) -> InputHandler a -> GView 'Unlocked GI.Widget
createUIDrawing model iActions = createCairo model $ contramapMaybe getAlt iActions

pointUIDrawing :: ((Double, Double) -> a) -> UIDrawing a
pointUIDrawing f = fmap (fmap pure) $ pointDrawing f

clickInputAction :: InputHandler (IO ())
clickInputAction = inputAction ClickInputType $ \(_, _, ioa) -> Just $ gvLiftIO ioa

showPoint :: String -> (Double, Double) -> IO ()
showPoint t p = putStrLn $ t <> ": " <> show p

withShowPoint :: String -> UIDrawing (IO ()) -> UIDrawing (IO ())
withShowPoint s d = mappend (pointUIDrawing $ showPoint s) d

drawing :: TimeZone -> UTCTime -> (Int32, Int32) -> UIDrawing (IO ())
drawing tz t (fromIntegral -> w, fromIntegral -> h) =
    let
        size = min w h
        LocalTime _ (TimeOfDay _ _ s) = utcToLocalTime tz t
        in translate (w / 2, h / 2)
            $ scale (size, size)
            $ scale (-0.5, -0.5)
            $ operatorOver
            $ lineCapSquare
            $ lineWidth 0.01
            $ mconcat
                [ sourceRGB (1, 0.3, 0)
                    $ stroke
                    $ arc (0, 0) 1 0 (2 * pi)
                , sourceRGB (0.3, 0, 1)
                    $ rotate (realToFrac s * pi / 30)
                    $ withShowPoint "P"
                    $ stroke
                    $ mconcat [moveTo (0, 0), lineTo (0, 1)]
                ]

zeroTime :: UTCTime
zeroTime = UTCTime (YearMonthDay 2000 1 1) 0

main :: IO ()
main = do
    runLifecycle
        $ runGTK
        $ \gtkContext ->
            runView
                $ runGView gtkContext
                $ do
                    (clockModel, ()) <-
                        gvLiftLifecycle $ makeSharedModel $ regularClockPremodel zeroTime $ secondsToNominalDiffTime 1
                    tz <- gvLiftIOTrustMeNoUI getCurrentTimeZone
                    rec (_, closer) <-
                            lift
                                $ gsvGetState
                                $ createWindow
                                $ let
                                    wsSize = (600, 600)
                                    wsCloseBoxAction :: GView 'Locked ()
                                    wsCloseBoxAction = gvRunUnlocked $ gvCloseState closer
                                    wsTitle :: Model (ROWUpdate Text)
                                    wsTitle = constantModel "Cairo"
                                    wsContent :: GView 'Unlocked Widget
                                    wsContent = do
                                        w1 <- createButton (constantModel "Button") (constantModel Nothing)
                                        w2 <- createUIDrawing (mapModel (funcChangeLens $ drawing tz) clockModel) clickInputAction
                                        gvRunLocked $ GI.set w2 [#marginStart GI.:= 100, #marginTop GI.:= 200]
                                        createLayout
                                            OrientationHorizontal
                                            [(defaultLayoutOptions, w1), (defaultLayoutOptions{loGrow = True}, w2)]
                                    in MkWindowSpec{..}
                    return ()
