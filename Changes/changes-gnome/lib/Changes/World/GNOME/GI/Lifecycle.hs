module Changes.World.GNOME.GI.Lifecycle where

import Changes.World.GNOME.GI.GView
import Import
import Import.GI qualified as GI

-- | Bind an object to a lifecycle, causing it to be deterministically disposed at the end of a lifecycle
-- rather than on finalisation. Not strictly necessary for correctness, but improves determinism.
gvBind :: GI.IsObject a => a -> GView 'Locked ()
gvBind a =
    gvOnClose
        $ gsvLiftIO
        $ do
            GI.objectUnref a
            _ <- GI.disownObject a
            return ()

gvNewUnbound :: GI.Constructible a tag => (GI.ManagedPtr a -> a) -> [GI.AttrOp a tag] -> GView 'Locked a
gvNewUnbound = GI.new

gvNew :: (GI.Constructible a tag, GI.IsObject a) => (GI.ManagedPtr a -> a) -> [GI.AttrOp a tag] -> GView 'Locked a
gvNew cc attrs = do
    a <- gvNewUnbound cc attrs
    gvBind a
    return a

gvDuplicateUnbound :: GI.IsObject a => (GI.ManagedPtr a -> a) -> a -> GView 'Locked a
gvDuplicateUnbound cc obj = do
    obj' <- GI.objectRef obj
    gvLiftIO $ GI.unsafeCastTo cc obj'

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
    gvOnClose $ gsvLiftIO $ GI.windowDestroy a
    return a

gvSet ::
    (GI.AttrClearC info obj attr, GI.AttrSetC info obj attr value) =>
    obj ->
    GI.AttrLabelProxy attr ->
    value ->
    GView 'Locked ()
gvSet obj prop val = do
    GI.set obj [prop GI.:= val]
    gvOnClose $ gsvLiftIO $ GI.clear obj prop
