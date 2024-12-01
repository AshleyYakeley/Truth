module Changes.World.GNOME.GI.Widget where

import Changes.World.GNOME.GI.GView
import Changes.World.GNOME.GI.LockState
import Changes.World.GNOME.GI.Type
import Data.GI.Base
import Data.GI.Gtk
import Data.IORef
import Shapes

containerGetAllChildren :: Container -> IO [Widget]
containerGetAllChildren cont = do
    ref <- newIORef []
    containerForall cont $ \child -> do
        children <- readIORef ref
        writeIORef ref $ children ++ [child]
    readIORef ref

getWidgetChildren :: Bool -> Widget -> IO (Maybe [Widget])
getWidgetChildren full w = do
    mcont <- castTo Container w
    for mcont $
        if full
            then containerGetAllChildren
            else containerGetChildren

widgetInfoText :: Widget -> IO Text
widgetInfoText w = do
    tn <- getObjectTypeName w
    vis <- getWidgetVisible w
    let
        hh =
            tn <>
            if vis
                then ""
                else "{hidden}"
    mww <- getWidgetChildren True w
    case mww of
        Nothing -> return hh
        Just ww -> do
            tt <- for ww widgetInfoText
            return $ hh <> " (" <> intercalate ", " tt <> ")"

widgetGetTree :: Bool -> Widget -> IO [Widget]
widgetGetTree full w = do
    mchildren <- getWidgetChildren full w
    case mchildren of
        Just children -> do
            ww <- for children $ widgetGetTree full
            return $ w : mconcat ww
        Nothing -> return [w]

isScrollable :: GObject widget => widget -> IO Bool
isScrollable widget = do
    mViewport <- castTo Viewport widget
    mTextView <- castTo TextView widget
    return $
        case (mViewport, mTextView) of
            (Nothing, Nothing) -> False
            _ -> True

gvAdd :: (IsContainer c, IsWidget w) => c -> w -> GView 'Locked ()
gvAdd c w = do
    containerAdd c w
    gvOnClose $ gvLiftIO $ containerRemove c w

gvPackStart :: (IsContainer box, IsBox box, IsWidget w) => Bool -> box -> w -> GView 'Locked ()
gvPackStart grow box w = do
    boxPackStart box w grow grow 0
    gvOnClose $ gvLiftIO $ containerRemove box w
