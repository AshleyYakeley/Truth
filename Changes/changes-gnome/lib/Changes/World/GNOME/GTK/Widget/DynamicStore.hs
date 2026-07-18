module Changes.World.GNOME.GTK.Widget.DynamicStore
    ( DynamicStoreEntry
    , dynamicStoreEntryValue
    , DynamicStore
    , getDynamicSeqStore
    , getDynamicListModel
    , dynamicStoreOnChange
    , newDynamicStore
    , dynamicStoreClear
    , dynamicStoreInsert
    , dynamicStoreDelete
    , dynamicStoreMove
    , dynamicStoreGet
    , dynamicStoreLookup
    , dynamicStoreContents
    )
where

import Data.IORef

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

data DynamicStoreEntry t = MkDynamicStoreEntry
    { dynamicStoreEntryKey :: Unique
    , dynamicStoreEntryValue :: t
    , dynamicStoreEntryState :: GViewState
    }

newtype DynamicStore t = MkDynamicStore
    { getDynamicSeqStore :: SeqStore (DynamicStoreEntry t)
    }

getDynamicListModel :: DynamicStore t -> GI.StringList
getDynamicListModel = seqStoreGetListModel . getDynamicSeqStore

dynamicStoreOnChange ::
    DynamicStore t ->
    (Int32 -> t -> GSemiview 'Locked ()) ->
    GView 'Locked ()
dynamicStoreOnChange (MkDynamicStore store) listener =
    seqStoreOnSetValue store $ \i entry -> listener i $ dynamicStoreEntryValue entry

findInStore :: Unique -> SeqStore (DynamicStoreEntry t) -> GSemiview 'Locked (Maybe (Int32, DynamicStoreEntry t))
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
    t ->
    (((t -> t) -> GSemiview 'Locked ()) -> GView 'Unlocked ()) ->
    SeqStore (DynamicStoreEntry t) ->
    GSemiview 'Unlocked (DynamicStoreEntry t)
makeEntry tdef cvt store = do
    dynamicStoreEntryKey <- gsvLiftIOTrustMeNoUI newUnique
    initValRef <- gsvLiftIOTrustMeNoUI $ newIORef tdef
    let
        setValStore :: (t -> t) -> GSemiview 'Locked ()
        setValStore tt = do
            mi <- findInStore dynamicStoreEntryKey store
            case mi of
                Nothing -> return ()
                Just (i, entry) -> do
                    let
                        oldt = dynamicStoreEntryValue entry
                        newt = tt oldt
                    seqStoreSetValue store i $ entry{dynamicStoreEntryValue = newt}
        setValInitial :: (t -> t) -> GSemiview 'Locked ()
        setValInitial tt =
            liftIO $ do
                t <- readIORef initValRef
                writeIORef initValRef $ tt t
    setValRef <- gsvLiftIOTrustMeNoUI $ newIORef setValInitial
    let
        setVal :: (t -> t) -> GSemiview 'Locked ()
        setVal tt = do
            sv <- liftIO $ readIORef setValRef
            sv tt
    ((), dynamicStoreEntryState) <- gsvGetState $ cvt setVal
    gsvLiftIOTrustMeNoUI $ writeIORef setValRef setValStore
    dynamicStoreEntryValue <- gsvLiftIOTrustMeNoUI $ readIORef initValRef
    return MkDynamicStoreEntry{..}

newDynamicStore :: t -> [((t -> t) -> GSemiview 'Locked ()) -> GView 'Unlocked ()] -> GView 'Unlocked (DynamicStore t)
newDynamicStore tdef lcv = do
    dstore <- lift $ mdo
        entries <- for lcv $ \cvt -> makeEntry tdef cvt store
        store <- gsvRunLocked $ seqStoreNew entries
        return $ MkDynamicStore store
    gvRunLocked $ gvBind $ getDynamicListModel dstore
    gvOnClose $ dynamicStoreClear dstore
    return dstore

dynamicStoreInsert ::
    Integral pos =>
    pos ->
    t ->
    (((t -> t) -> GSemiview 'Locked ()) -> GView 'Unlocked ()) ->
    DynamicStore t ->
    GSemiview 'Unlocked ()
dynamicStoreInsert i tdef cvt (MkDynamicStore store) = do
    entry <- makeEntry tdef cvt store
    gsvRunLocked $ seqStoreInsert store (fromIntegral i) entry

dynamicStoreClear :: DynamicStore t -> GSemiview 'Unlocked ()
dynamicStoreClear (MkDynamicStore store) = do
    entries <- gsvRunLocked $ seqStoreToList store
    for_ entries $ \entry -> gsvCloseState $ dynamicStoreEntryState entry
    gsvRunLocked $ seqStoreClear store

dynamicStoreDelete :: Integral pos => pos -> DynamicStore t -> GSemiview 'Unlocked ()
dynamicStoreDelete i (MkDynamicStore store) = do
    entry <- gsvRunLocked $ seqStoreGetValue store $ fromIntegral i
    gsvCloseState $ dynamicStoreEntryState entry
    gsvRunLocked $ seqStoreRemove store $ fromIntegral i

dynamicStoreMove :: Integral pos => pos -> pos -> DynamicStore t -> GSemiview 'Unlocked ()
dynamicStoreMove a b _
    | a == b = return ()
dynamicStoreMove a b (MkDynamicStore store) =
    gsvRunLocked $ do
        entry <- seqStoreGetValue store $ fromIntegral a
        seqStoreRemove store $ fromIntegral a
        seqStoreInsert store (fromIntegral b) entry

dynamicStoreGet :: Integral pos => pos -> DynamicStore t -> GSemiview 'Locked (Unique, t)
dynamicStoreGet i (MkDynamicStore store) = do
    entry <- seqStoreGetValue store $ fromIntegral i
    return (dynamicStoreEntryKey entry, dynamicStoreEntryValue entry)

dynamicStoreLookup ::
    forall pos t.
    Integral pos =>
    Unique ->
    DynamicStore t ->
    GSemiview 'Locked (Maybe pos)
dynamicStoreLookup u (MkDynamicStore store) = do
    entries <- seqStoreToList store
    let
        testEntry :: DynamicStoreEntry t -> GSemiview 'Locked Bool
        testEntry entry = return $ dynamicStoreEntryKey entry == u
    mi <- mFindIndex testEntry entries
    return $ fmap fromIntegral mi

dynamicStoreContents :: DynamicStore t -> GSemiview 'Locked [t]
dynamicStoreContents (MkDynamicStore store) = do
    entries <- seqStoreToList store
    return $ fmap dynamicStoreEntryValue entries
