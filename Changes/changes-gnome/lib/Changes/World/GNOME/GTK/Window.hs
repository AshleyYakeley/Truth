module Changes.World.GNOME.GTK.Window
    ( WindowSpec (..)
    , UIWindow
    , uiWindowHide
    , uiWindowShow
    , uiWindowDebugDescribe
    , createWindow
    , WindowPosition (..)
    )
where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

data WindowSpec = MkWindowSpec
    { wsSize :: (Int32, Int32)
    , wsCloseBoxAction :: GView 'Locked ()
    , wsTitle :: Model (ROWUpdate Text)
    , wsContent :: GI.AccelGroup -> GView 'Unlocked GI.Widget
    }

data UIWindow = MkUIWindow
    { uiWindowHide :: GView 'Locked ()
    , uiWindowShow :: GView 'Locked ()
    , uiWindowDebugDescribe :: IO Text
    }

createWindow :: WindowSpec -> GView 'Unlocked UIWindow
createWindow MkWindowSpec{..} = do
    window <-
        gvExitOnClosed
            $ gvRunLocked
            $ gvNewWindow Window [#defaultWidth := fst wsSize, #defaultHeight := snd wsSize]
    gvBindReadOnlyWholeModel wsTitle $ \title -> gvRunLocked $ set window [#title := title]
    ag <-
        gvRunLocked $ do
            _ <-
                gvOnSignal window #deleteEvent $ \_ -> do
                    wsCloseBoxAction
                    return True -- don't run existing handler that closes the window
            ag <- gvNew AccelGroup []
            #addAccelGroup window ag
            return ag
    content <- wsContent ag
    gvRunLocked $ do
        #add window content
        #show content
        #showAll window
        let
            uiWindowHide = #hide window
            uiWindowShow = #show window
            uiWindowDebugDescribe = do
                w <- toWidget window
                widgetInfoText w
        return $ MkUIWindow{..}
