module Changes.World.GNOME.GI.Lifecycle where

import Changes.World.GNOME.GI.GView
import Changes.World.GNOME.GI.LockState
import Import
import Import.GI qualified as GI

gvAcquire :: GI.IsObject a => a -> GView 'Locked ()
gvAcquire a = do
    _ <- GI.objectRef a
    gvOnClose $ gvLiftIO $ GI.objectUnref a

gvNew :: (GI.Constructible a tag, GI.IsObject a) => (GI.ManagedPtr a -> a) -> [GI.AttrOp a tag] -> GView 'Locked a
gvNew cc attrs = do
    a <- GI.new cc attrs
    gvAcquire a
    return a

gvNewWidget ::
    (GI.Constructible a tag, GI.IsObject a, GI.IsWidget a) =>
    (GI.ManagedPtr a -> a) ->
    [GI.AttrOp a tag] ->
    GView 'Locked (a, GI.Widget)
gvNewWidget cc attrs = do
    a <- gvNew cc attrs
    widget <- GI.toWidget a
    return (a, widget)

gvNewWindow ::
    (GI.Constructible a tag, GI.IsObject a, GI.IsWindow a) => (GI.ManagedPtr a -> a) -> [GI.AttrOp a tag] -> GView 'Locked a
gvNewWindow cc attrs = do
    a <- gvNew cc attrs
    gvOnClose $ gvLiftIO $ GI.windowDestroy a
    return a

gvSet ::
    (GI.AttrClearC info obj attr, GI.AttrSetC info obj attr value) =>
    obj ->
    GI.AttrLabelProxy attr ->
    value ->
    GView 'Locked ()
gvSet obj prop val = do
    GI.set obj [prop GI.:= val]
    gvOnClose $ gvLiftIO $ GI.clear obj prop
