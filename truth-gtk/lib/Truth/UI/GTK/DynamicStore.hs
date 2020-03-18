module Truth.UI.GTK.DynamicStore
    ( DynamicStoreEntry
    , dynamicStoreEntryValue
    , DynamicStore
    , getDynamicSeqStore
    , newDynamicStore
    , dynamicStoreClear
    , dynamicStoreInsert
    , dynamicStoreDelete
    , dynamicStoreMove
    ) where

import Data.GI.Gtk hiding (get)
import Data.IORef
import Shapes
import Truth.Core

data DynamicStoreEntry t = MkDynamicStoreEntry
    { dynamicStoreEntryKey :: Unique
    , dynamicStoreEntryValue :: t
    , dynamicStoreEntryState :: ViewState
    }

newtype DynamicStore t = MkDynamicStore
    { getDynamicSeqStore :: SeqStore (DynamicStoreEntry t)
    }

findInStore :: Unique -> SeqStore (DynamicStoreEntry t) -> IO (Maybe (Int32, DynamicStoreEntry t))
findInStore key store = do
    n <- seqStoreGetSize store
    let
        findit i
            | i >= n = return Nothing
        findit i = do
            entry <- seqStoreGetValue store i
            if dynamicStoreEntryKey entry == key
                then return $ Just (i, entry)
                else findit $ succ i
    findit 0

makeEntry ::
       forall t. (((t -> t) -> IO ()) -> CreateView ()) -> SeqStore (DynamicStoreEntry t) -> View (DynamicStoreEntry t)
makeEntry cvt store = do
    dynamicStoreEntryKey <- liftIO newUnique
    initValRef <- liftIO $ newIORef $ error "unset store entry"
    let
        setValStore :: (t -> t) -> IO ()
        setValStore tt = do
            mi <- findInStore dynamicStoreEntryKey store
            case mi of
                Nothing -> return ()
                Just (i, entry) -> do
                    let
                        oldt = dynamicStoreEntryValue entry
                        newt = tt oldt
                    seqStoreSetValue store i $ entry {dynamicStoreEntryValue = newt}
        setValInitial :: (t -> t) -> IO ()
        setValInitial tt = do
            t <- readIORef initValRef
            writeIORef initValRef $ tt t
    setValRef <- liftIO $ newIORef setValInitial
    let
        setVal :: (t -> t) -> IO ()
        setVal tt = do
            sv <- readIORef setValRef
            sv tt
    ((), dynamicStoreEntryState) <- viewCreateView $ cvt setVal
    liftIO $ writeIORef setValRef setValStore
    dynamicStoreEntryValue <- liftIO $ readIORef initValRef
    return MkDynamicStoreEntry {..}

newDynamicStore :: [((t -> t) -> IO ()) -> CreateView ()] -> CreateView (DynamicStore t)
newDynamicStore lcv = do
    rec
        entries <- for lcv $ \cvt -> cvLiftView $ makeEntry cvt store
        store <- seqStoreNew entries
    liftLifeCycleIO $ lifeCycleClose $ dynamicStoreClear $ MkDynamicStore store
    return $ MkDynamicStore store

dynamicStoreInsert :: Integral pos => pos -> (((t -> t) -> IO ()) -> CreateView ()) -> DynamicStore t -> View ()
dynamicStoreInsert i cvt (MkDynamicStore store) = do
    entry <- makeEntry cvt store
    seqStoreInsert store (fromIntegral i) entry

dynamicStoreClear :: MonadIO m => DynamicStore t -> m ()
dynamicStoreClear (MkDynamicStore store) = do
    entries <- seqStoreToList store
    liftIO $ for_ entries $ \entry -> closeLifeState $ dynamicStoreEntryState entry
    seqStoreClear store

dynamicStoreDelete :: Integral pos => pos -> DynamicStore t -> View ()
dynamicStoreDelete i (MkDynamicStore store) = do
    entry <- seqStoreGetValue store $ fromIntegral i
    liftIO $ closeLifeState $ dynamicStoreEntryState entry
    seqStoreRemove store $ fromIntegral i

dynamicStoreMove :: Integral pos => pos -> pos -> DynamicStore t -> View ()
dynamicStoreMove a b (MkDynamicStore store) = do
    entry <- seqStoreGetValue store $ fromIntegral a
    seqStoreRemove store $ fromIntegral a
    seqStoreInsert store (fromIntegral b) entry
