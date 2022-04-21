module Changes.UI.GTK.DynamicStore
    ( DynamicStoreEntry
    , dynamicStoreEntryValue
    , DynamicStore
    , getDynamicSeqStore
    , newDynamicStore
    , dynamicStoreClear
    , dynamicStoreInsert
    , dynamicStoreDelete
    , dynamicStoreMove
    , dynamicStoreGet
    , dynamicStoreLookup
    , dynamicStoreContents
    ) where

import Changes.Core
import Data.GI.Gtk hiding (get)
import Data.IORef
import Shapes

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
       forall t.
       t
    -> (((t -> t) -> IO ()) -> CreateView ())
    -> SeqStore (DynamicStoreEntry t)
    -> View (DynamicStoreEntry t)
makeEntry tdef cvt store = do
    dynamicStoreEntryKey <- liftIO newUnique
    initValRef <- liftIO $ newIORef tdef
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
    ((), dynamicStoreEntryState) <- getLifeState $ cvt setVal
    liftIO $ writeIORef setValRef setValStore
    dynamicStoreEntryValue <- liftIO $ readIORef initValRef
    return MkDynamicStoreEntry {..}

newDynamicStore :: t -> [((t -> t) -> IO ()) -> CreateView ()] -> CreateView (DynamicStore t)
newDynamicStore tdef lcv = do
    rec
        entries <- for lcv $ \cvt -> lift $ makeEntry tdef cvt store
        store <- seqStoreNew entries
    lifeCycleClose $ dynamicStoreClear $ MkDynamicStore store
    return $ MkDynamicStore store

dynamicStoreInsert :: Integral pos => pos -> t -> (((t -> t) -> IO ()) -> CreateView ()) -> DynamicStore t -> View ()
dynamicStoreInsert i tdef cvt (MkDynamicStore store) = do
    entry <- makeEntry tdef cvt store
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
dynamicStoreMove a b _
    | a == b = return ()
dynamicStoreMove a b (MkDynamicStore store) = do
    entry <- seqStoreGetValue store $ fromIntegral a
    seqStoreRemove store $ fromIntegral a
    seqStoreInsert store (fromIntegral b) entry

dynamicStoreGet :: Integral pos => pos -> DynamicStore t -> View (Unique, t)
dynamicStoreGet i (MkDynamicStore store) = do
    entry <- seqStoreGetValue store $ fromIntegral i
    return (dynamicStoreEntryKey entry, dynamicStoreEntryValue entry)

dynamicStoreLookup ::
       forall pos t. Integral pos
    => Unique
    -> DynamicStore t
    -> View (Maybe pos)
dynamicStoreLookup u (MkDynamicStore store) = do
    entries <- seqStoreToList store
    let
        testEntry :: DynamicStoreEntry t -> View Bool
        testEntry entry = return $ dynamicStoreEntryKey entry == u
    mi <- mFindIndex testEntry entries
    return $ fmap fromIntegral mi

dynamicStoreContents :: DynamicStore t -> View [t]
dynamicStoreContents (MkDynamicStore store) = do
    entries <- seqStoreToList store
    return $ fmap dynamicStoreEntryValue entries
