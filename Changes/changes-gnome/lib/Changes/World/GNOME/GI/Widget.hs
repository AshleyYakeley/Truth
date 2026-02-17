module Changes.World.GNOME.GI.Widget where

import Changes.World.GNOME.GI.GView
import Changes.World.GNOME.GI.LockState
import Changes.World.GNOME.GI.Type
import Import
import Import.GI qualified as GI

getWidgetChildren :: GI.Widget -> IO [GI.Widget]
getWidgetChildren cont = do
    lmodel <- GI.widgetObserveChildren cont
    n <- GI.listModelGetNItems lmodel
    forf [0 .. pred n] $ \i -> do
        mobj <- GI.listModelGetItem lmodel i
        forf mobj $ \obj -> GI.castTo GI.Widget obj

getWidgetForest :: GI.Widget -> IO (Forest GI.Widget)
getWidgetForest w = do
    cc <- getWidgetChildren w
    tt <- for cc getWidgetTree
    return $ MkForest tt

getWidgetTree :: GI.Widget -> IO (Tree GI.Widget)
getWidgetTree w = do
    f <- getWidgetForest w
    return $ MkTree w f

widgetInfoText :: GI.Widget -> IO Text
widgetInfoText w = do
    tn <- getObjectTypeName w
    vis <- GI.getWidgetVisible w
    let
        hh =
            tn
                <> if vis
                    then ""
                    else "{hidden}"
    mww <- getWidgetTree w
    case toList mww of
        [] -> return hh
        ww -> do
            tt <- for ww widgetInfoText
            return $ hh <> " (" <> intercalate ", " tt <> ")"

isScrollable :: GI.GObject widget => widget -> IO Bool
isScrollable widget = do
    mViewport <- GI.castTo GI.Viewport widget
    mTextView <- GI.castTo GI.TextView widget
    return
        $ case (mViewport, mTextView) of
            (Nothing, Nothing) -> False
            _ -> True

gvBoxPrepend :: (GI.IsBox box, GI.IsWidget w) => box -> w -> GView 'Locked ()
gvBoxPrepend box w = do
    GI.boxPrepend box w
    gvOnClose $ gvLiftIO $ GI.boxRemove box w
