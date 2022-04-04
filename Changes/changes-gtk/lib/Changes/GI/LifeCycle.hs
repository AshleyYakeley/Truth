module Changes.GI.LifeCycle where

import Data.GI.Base
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.GI.Gtk
import GI.GObject
import Shapes

cvAcquire :: (MonadLifeCycleIO m, IsObject a) => a -> m ()
cvAcquire a = do
    _ <- objectRef a
    lifeCycleClose $ objectUnref a

cvNew :: (MonadLifeCycleIO m, Constructible a tag, IsObject a) => (ManagedPtr a -> a) -> [AttrOp a tag] -> m a
cvNew cc attrs = do
    a <- new cc attrs
    cvAcquire a
    return a

-- | Probably only use this for top-level widgets
cvTopLevelNew ::
       (MonadLifeCycleIO m, Constructible a tag, IsObject a, IsWidget a) => (ManagedPtr a -> a) -> [AttrOp a tag] -> m a
cvTopLevelNew cc attrs = do
    a <- cvNew cc attrs
    lifeCycleClose $ widgetDestroy a
    return a

cvSet ::
       (MonadLifeCycleIO m, AttrClearC info obj attr, AttrSetC info obj attr value)
    => obj
    -> AttrLabelProxy attr
    -> value
    -> m ()
cvSet obj prop val = do
    set obj [prop := val]
    lifeCycleClose $ clear obj prop
