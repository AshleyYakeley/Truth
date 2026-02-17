module Changes.World.GNOME.GI.Polymorphic where

import Import
import Import.GI qualified as GI
import Shapes.Unsafe (unsafeRefl)
import Type.Reflection
import Data.Dynamic
import Changes.World.GNOME.GI.Dynamic ()
import Changes.World.GNOME.GI.GView
import Changes.World.GNOME.GI.LockState
import GI.Gtk.Objects.ListStore qualified

type IndexType = Type

type UItem :: IndexType -> Type
type family UItem index where {}

assignUItem :: forall (index :: IndexType) (t :: Type) r. (UItem index ~ t => r) -> r
assignUItem = withRefl $ unsafeRefl @Type @(UItem index) @t

newtype WItem (index :: IndexType) = MkWItem (UItem index)

uitemToDynamic :: forall index. Typeable index => UItem index -> Dynamic
uitemToDynamic item = toDyn $ MkWItem @index item

uitemFromDynamic :: forall index. Typeable index => Dynamic -> Maybe (UItem index)
uitemFromDynamic d = do
    MkWItem item <- fromDynamic @(WItem index) d
    return item

data SeqStoreIndex :: IndexType

type SeqStoreItem = UItem SeqStoreIndex

newtype SeqStore (a :: Type) = MkSeqStore GI.ListStore
    deriving newtype (GI.HasParentTypes)

type instance GI.ParentTypes (SeqStore _) = GI.ParentTypes GI.ListStore


seqStoreNew :: forall a. [a] -> GView 'Locked (SeqStore a)
seqStoreNew items = do
    gType <- liftIO $ GI.gvalueGType_ @Dynamic
    listStore <- GI.Gtk.Objects.ListStore.listStoreNew gType
    for_ items $ GI.listStoreAppend listStore
    return $ MkSeqStore listStore

{-seqStoreGetSize :: SeqStore a -> GView Locked Int32


seqStoreGetValue :: SeqStore a -> Int32 -> GView Locked a

seqStoreSetValue :: SeqStore a -> Int32 -> a -> GView Locked ()

seqStoreInsert :: SeqStore a -> Int32 -> a -> GView Locked ()
seqStoreRemove :: SeqStore a -> Int32 -> GView Locked ()

seqStoreToList :: SeqStore a -> GView Locked (t3 (DynamicStoreEntry t4))

seqStoreClear :: SeqStore a -> GView Locked ()
-}
