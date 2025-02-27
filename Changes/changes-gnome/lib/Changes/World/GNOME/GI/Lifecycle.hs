module Changes.World.GNOME.GI.Lifecycle where

import Data.GI.Base
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.GI.Gtk
import GI.GObject
import Shapes

import Changes.World.GNOME.GI.GView
import Changes.World.GNOME.GI.LockState

gvAcquire :: IsObject a => a -> GView 'Locked ()
gvAcquire a = do
    _ <- objectRef a
    gvOnClose $ gvLiftIO $ objectUnref a

gvNew :: (Constructible a tag, IsObject a) => (ManagedPtr a -> a) -> [AttrOp a tag] -> GView 'Locked a
gvNew cc attrs = do
    a <- new cc attrs
    gvAcquire a
    return a

gvNewWidget ::
    (Constructible a tag, IsObject a, IsWidget a) =>
    (ManagedPtr a -> a) ->
    [AttrOp a tag] ->
    GView 'Locked (a, Widget)
gvNewWidget cc attrs = do
    a <- gvNew cc attrs
    widget <- toWidget a
    return (a, widget)

-- | Probably only use this for top-level widgets
gvTopLevelNew ::
    (Constructible a tag, IsObject a, IsWidget a) => (ManagedPtr a -> a) -> [AttrOp a tag] -> GView 'Locked a
gvTopLevelNew cc attrs = do
    a <- gvNew cc attrs
    gvOnClose $ gvLiftIO $ widgetDestroy a
    return a

gvSet ::
    (AttrClearC info obj attr, AttrSetC info obj attr value) =>
    obj ->
    AttrLabelProxy attr ->
    value ->
    GView 'Locked ()
gvSet obj prop val = do
    set obj [prop := val]
    gvOnClose $ gvLiftIO $ clear obj prop
