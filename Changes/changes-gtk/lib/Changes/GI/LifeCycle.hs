module Changes.GI.LifeCycle where

import Changes.Core
import Data.GI.Base
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.GI.Gtk
import GI.GObject
import Shapes

cvAcquire :: IsObject a => a -> View ()
cvAcquire a = do
    _ <- objectRef a
    viewOnClose $ objectUnref a

cvNew :: (Constructible a tag, IsObject a) => (ManagedPtr a -> a) -> [AttrOp a tag] -> View a
cvNew cc attrs = do
    a <- new cc attrs
    cvAcquire a
    return a

-- | Probably only use this for top-level widgets
cvTopLevelNew :: (Constructible a tag, IsObject a, IsWidget a) => (ManagedPtr a -> a) -> [AttrOp a tag] -> View a
cvTopLevelNew cc attrs = do
    a <- cvNew cc attrs
    viewOnClose $ widgetDestroy a
    return a

cvSet :: (AttrClearC info obj attr, AttrSetC info obj attr value) => obj -> AttrLabelProxy attr -> value -> View ()
cvSet obj prop val = do
    set obj [prop := val]
    viewOnClose $ clear obj prop
