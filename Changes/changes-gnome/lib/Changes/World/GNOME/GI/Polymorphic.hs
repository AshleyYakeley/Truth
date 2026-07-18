module Changes.World.GNOME.GI.Polymorphic
    ( SeqStore
    , seqStoreGetListModel
    , seqStoreOnSetValue
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
import Import.GI qualified as GI

data SeqStore a = MkSeqStore
    { seqStoreValues :: IORef [a]
    , seqStoreGetListModel :: GI.StringList
    , seqStoreSetValueListeners :: IORef [(Unique, Int32 -> a -> GSemiview 'Locked ())]
    }

seqStoreNew :: [a] -> GSemiview 'Locked (SeqStore a)
seqStoreNew items = do
    seqStoreValues <- liftIO $ newIORef items
    seqStoreGetListModel <- liftIO $ GI.stringListNew $ Just $ replicate (length items) ""
    seqStoreSetValueListeners <- liftIO $ newIORef []
    return MkSeqStore{..}

seqStoreOnSetValue :: SeqStore a -> (Int32 -> a -> GSemiview 'Locked ()) -> GView 'Locked ()
seqStoreOnSetValue MkSeqStore{..} listener = do
    key <- gvLiftIO newUnique
    gvLiftIO $ modifyIORef' seqStoreSetValueListeners ((key, listener) :)
    let
        removeListener :: GSemiview 'Unlocked ()
        removeListener =
            gsvLiftIOTrustMeNoUI
                $ modifyIORef' seqStoreSetValueListeners
                $ filter ((/= key) . fst)
    gvOnClose removeListener

seqStoreGetSize :: SeqStore a -> GSemiview 'Locked Int32
seqStoreGetSize MkSeqStore{..} = fromIntegral . length <$> liftIO (readIORef seqStoreValues)

seqStoreGetValue :: SeqStore a -> Int32 -> GSemiview 'Locked a
seqStoreGetValue MkSeqStore{..} i = do
    items <- liftIO $ readIORef seqStoreValues
    case drop (fromIntegral i) items of
        (x : _) -> pure x
        [] -> fail "seqStoreGetValue: index out of range"

seqStoreSetValue :: SeqStore a -> Int32 -> a -> GSemiview 'Locked ()
seqStoreSetValue MkSeqStore{..} i val = do
    changed <-
        liftIO $ atomicModifyIORef' seqStoreValues $ \items ->
            let
                (l, r) = splitAt (fromIntegral i) items
                in case r of
                    [] -> (items, False)
                    (_ : rr) -> (l <> (val : rr), True)
    when changed $ do
        listeners <- liftIO $ readIORef seqStoreSetValueListeners
        for_ listeners $ \(_, listener) -> listener i val

seqStoreInsert :: SeqStore a -> Int32 -> a -> GSemiview 'Locked ()
seqStoreInsert MkSeqStore{..} i val = do
    liftIO $ modifyIORef' seqStoreValues $ \items ->
        let
            (l, r) = splitAt (fromIntegral i) items
            in l <> (val : r)
    liftIO $ GI.stringListSplice seqStoreGetListModel (fromIntegral i) 0 $ Just [""]

seqStoreRemove :: SeqStore a -> Int32 -> GSemiview 'Locked ()
seqStoreRemove MkSeqStore{..} i = do
    liftIO $ modifyIORef' seqStoreValues $ \items ->
        let
            (l, r) = splitAt (fromIntegral i) items
            in l <> drop 1 r
    liftIO $ GI.stringListRemove seqStoreGetListModel $ fromIntegral i

seqStoreToList :: SeqStore a -> GSemiview 'Locked [a]
seqStoreToList MkSeqStore{..} = liftIO $ readIORef seqStoreValues

seqStoreClear :: SeqStore a -> GSemiview 'Locked ()
seqStoreClear MkSeqStore{..} = do
    size <- liftIO $ atomicModifyIORef' seqStoreValues $ \items -> ([], length items)
    liftIO $ GI.stringListSplice seqStoreGetListModel 0 (fromIntegral size) Nothing
