module Changes.GI.LifeCycle where

import Changes.Debug
import Changes.GI.GView
import Changes.GI.LockState
import Data.GI.Base
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.GI.Gtk
import GI.GObject
import Shapes

gvAcquire :: IsObject a => a -> GView 'Locked ()
gvAcquire a = do
    _ <- traceBracket "ref" $ objectRef a
    gvOnClose $ gvLiftIO $ traceBracket "unref" $ objectUnref a

gvNew :: (Constructible a tag, IsObject a) => (ManagedPtr a -> a) -> [AttrOp a tag] -> GView 'Locked a
gvNew cc attrs = do
    a <- traceBracket "new" $ new cc attrs
    gvAcquire a
    return a

-- | Probably only use this for top-level widgets
gvTopLevelNew ::
       (Constructible a tag, IsObject a, IsWidget a) => (ManagedPtr a -> a) -> [AttrOp a tag] -> GView 'Locked a
gvTopLevelNew cc attrs = do
    a <- traceBracket "toplevel.new" $ gvNew cc attrs
    gvOnClose $ gvLiftIO $ traceBracket "toplevel.destroy" $ widgetDestroy a
    return a

gvSet ::
       (AttrClearC info obj attr, AttrSetC info obj attr value)
    => obj
    -> AttrLabelProxy attr
    -> value
    -> GView 'Locked ()
gvSet obj prop val = do
    set obj [prop := val]
    gvOnClose $ gvLiftIO $ clear obj prop
