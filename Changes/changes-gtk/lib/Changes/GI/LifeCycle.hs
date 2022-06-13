module Changes.GI.LifeCycle where

import Changes.GI.GView
import Data.GI.Base
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.GI.Gtk
import GI.GObject
import Shapes

gvAcquire :: IsObject a => a -> GView 'Locked ()
gvAcquire a = do
    _ <- objectRef a
    gvOnClose $ objectUnref a

gvNew :: (Constructible a tag, IsObject a) => (ManagedPtr a -> a) -> [AttrOp a tag] -> GView 'Locked a
gvNew cc attrs = do
    a <- new cc attrs
    gvAcquire a
    return a

-- | Probably only use this for top-level widgets
gvTopLevelNew ::
       (Constructible a tag, IsObject a, IsWidget a) => (ManagedPtr a -> a) -> [AttrOp a tag] -> GView 'Locked a
gvTopLevelNew cc attrs = do
    a <- gvNew cc attrs
    gvOnClose $ widgetDestroy a
    return a

gvSet ::
       (AttrClearC info obj attr, AttrSetC info obj attr value)
    => obj
    -> AttrLabelProxy attr
    -> value
    -> GView 'Locked ()
gvSet obj prop val = do
    set obj [prop := val]
    gvOnClose $ clear obj prop
