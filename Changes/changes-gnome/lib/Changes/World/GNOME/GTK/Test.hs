module Changes.World.GNOME.GTK.Test where

import Data.GI.Base.GValue
import GI.GObject
import GI.Gtk
import Shapes hiding (get)

import Changes.World.GNOME.GI

getAllWidgets :: Widget -> GView 'Locked [Widget]
getAllWidgets w = do
    mcont <- liftIO $ castTo Container w
    case mcont of
        Nothing -> return [w]
        Just cont -> do
            cc <- #getChildren cont
            ww <- for cc getAllWidgets
            return $ w : mconcat ww

gobjectEmitClicked ::
    forall t.
    GObject t =>
    t ->
    GView 'Locked ()
gobjectEmitClicked obj = do
    gtype <- liftIO $ glibType @t
    (_, signalId, detail) <- signalParseName "clicked" gtype False
    liftIO
        $ withManagedPtr obj
        $ \entryPtr -> do
            gvalObj <- buildGValue gtype set_object entryPtr
            _ <- signalEmitv [gvalObj] signalId detail
            return ()

getWindows :: GView 'Locked [Window]
getWindows = do
    ww <- windowListToplevels
    forf ww $ \w -> liftIO $ castTo Window w

getVisibleWindows :: GView 'Locked [Window]
getVisibleWindows = do
    ww <- getWindows
    forf ww $ \w -> do
        v <- liftIO $ get w #visible
        return
            $ if v
                then Just w
                else Nothing

clickOnlyWindowButton :: GView 'Locked ()
clickOnlyWindowButton = do
    ww <- getVisibleWindows
    case ww of
        [w] -> do
            w' <- toWidget w
            cc <- getAllWidgets w'
            bb <- forf cc $ \c -> liftIO $ castTo Button c
            case bb of
                [b] -> gobjectEmitClicked b
                _ -> fail $ show (length bb) <> " Buttons"
        _ -> fail $ show (length ww) <> " visible windows"
