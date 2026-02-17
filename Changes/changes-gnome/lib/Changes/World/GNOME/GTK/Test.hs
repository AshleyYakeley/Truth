module Changes.World.GNOME.GTK.Test where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

getAllWidgets :: GI.Widget -> GView 'Locked [GI.Widget]
getAllWidgets w = do
    mcont <- liftIO $ GI.castTo Container w
    case mcont of
        Nothing -> return [w]
        Just cont -> do
            cc <- #getChildren cont
            ww <- for cc getAllWidgets
            return $ w : mconcat ww

gobjectEmitClicked ::
    forall t.
    GI.GObject t =>
    t ->
    GView 'Locked ()
gobjectEmitClicked obj = do
    gtype <- liftIO $ GI.glibType @t
    (_, signalId, detail) <- GI.signalParseName "clicked" gtype False
    liftIO
        $ GI.withManagedPtr obj
        $ \entryPtr -> do
            gvalObj <- GI.buildGValue gtype GI.set_object entryPtr
            _ <- GI.signalEmitv [gvalObj] signalId detail
            return ()

getWindows :: GView 'Locked [GI.Window]
getWindows = do
    ww <- GI.windowListToplevels
    forf ww $ \w -> liftIO $ GI.castTo Window w

getVisibleWindows :: GView 'Locked [GI.Window]
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
            w' <- GI.toWidget w
            cc <- getAllWidgets w'
            bb <- forf cc $ \c -> liftIO $ GI.castTo Button c
            case bb of
                [b] -> gobjectEmitClicked b
                _ -> fail $ show (length bb) <> " Buttons"
        _ -> fail $ show (length ww) <> " visible windows"
