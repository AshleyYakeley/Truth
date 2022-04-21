module Changes.GI.LifeCycle where

import Data.GI.Base
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.GI.Gtk
import GI.GObject
import Shapes

cvAcquire :: (MonadIO m, IsObject a) => a -> LifeCycleT m ()
cvAcquire a = do
    _ <- objectRef a
    lifeCycleCloseIO $ objectUnref a

cvNew :: (MonadIO m, Constructible a tag, IsObject a) => (ManagedPtr a -> a) -> [AttrOp a tag] -> LifeCycleT m a
cvNew cc attrs = do
    a <- new cc attrs
    cvAcquire a
    return a

-- | Probably only use this for top-level widgets
cvTopLevelNew ::
       (MonadIO m, Constructible a tag, IsObject a, IsWidget a)
    => (ManagedPtr a -> a)
    -> [AttrOp a tag]
    -> LifeCycleT m a
cvTopLevelNew cc attrs = do
    a <- cvNew cc attrs
    lifeCycleCloseIO $ widgetDestroy a
    return a

cvSet ::
       (MonadIO m, AttrClearC info obj attr, AttrSetC info obj attr value)
    => obj
    -> AttrLabelProxy attr
    -> value
    -> LifeCycleT m ()
cvSet obj prop val = do
    set obj [prop := val]
    lifeCycleCloseIO $ clear obj prop
