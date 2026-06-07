module Changes.World.GNOME.GI.Polymorphic
    ( SeqStore
    , seqStoreNew
    , seqStoreGetSize
    , seqStoreGetValue
    , seqStoreSetValue
    , seqStoreInsert
    , seqStoreRemove
    , seqStoreToList
    , seqStoreClear
    )
where

import Data.IORef

import Changes.World.GNOME.GI.GView
import Import

newtype SeqStore a = MkSeqStore (IORef [a])

seqStoreNew :: [a] -> GSemiview 'Locked (SeqStore a)
seqStoreNew items = MkSeqStore <$> liftIO (newIORef items)

seqStoreGetSize :: SeqStore a -> GSemiview 'Locked Int32
seqStoreGetSize (MkSeqStore ref) = fromIntegral . length <$> liftIO (readIORef ref)

seqStoreGetValue :: SeqStore a -> Int32 -> GSemiview 'Locked a
seqStoreGetValue (MkSeqStore ref) i = do
    items <- liftIO $ readIORef ref
    case drop (fromIntegral i) items of
        (x : _) -> pure x
        [] -> fail "seqStoreGetValue: index out of range"

seqStoreSetValue :: SeqStore a -> Int32 -> a -> GSemiview 'Locked ()
seqStoreSetValue (MkSeqStore ref) i val =
    liftIO $ modifyIORef' ref $ \items ->
        let
            (l, r) = splitAt (fromIntegral i) items
            in case r of
                [] -> items
                (_ : rr) -> l <> (val : rr)

seqStoreInsert :: SeqStore a -> Int32 -> a -> GSemiview 'Locked ()
seqStoreInsert (MkSeqStore ref) i val =
    liftIO $ modifyIORef' ref $ \items ->
        let
            (l, r) = splitAt (fromIntegral i) items
            in l <> (val : r)

seqStoreRemove :: SeqStore a -> Int32 -> GSemiview 'Locked ()
seqStoreRemove (MkSeqStore ref) i =
    liftIO $ modifyIORef' ref $ \items ->
        let
            (l, r) = splitAt (fromIntegral i) items
            in l <> drop 1 r

seqStoreToList :: SeqStore a -> GSemiview 'Locked [a]
seqStoreToList (MkSeqStore ref) = liftIO $ readIORef ref

seqStoreClear :: SeqStore a -> GSemiview 'Locked ()
seqStoreClear (MkSeqStore ref) = liftIO $ writeIORef ref []
