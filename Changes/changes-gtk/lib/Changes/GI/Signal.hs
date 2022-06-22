module Changes.GI.Signal
    ( withSignalBlocked
    , withSignalsBlocked
    , gvOnSignal
    , gvAfterSignal
    , gvTraceSignal
    , gvObjectTraceSignal
    , gvObjectTraceAllSignals
    , gvObjectReportAllSignals
    ) where

import Changes.Debug
import Changes.GI.GView
import Changes.GI.LockState
import Changes.GI.Type
import Data.GI.Base
import Data.GI.Base.Signals
import GI.GObject
import Shapes

withSignalBlocked :: IsObject obj => obj -> SignalHandlerId -> GView 'Locked --> GView 'Locked
withSignalBlocked obj conn = bracket_ (signalHandlerBlock obj conn) (signalHandlerUnblock obj conn)

withSignalsBlocked :: IsObject obj => obj -> [SignalHandlerId] -> GView 'Locked --> GView 'Locked
withSignalsBlocked _obj [] = id
withSignalsBlocked obj (c:cc) = withSignalBlocked obj c . withSignalsBlocked obj cc

class GTKCallbackType t where
    type CallbackViewLifted t :: Type
    gCallbackUnlift :: (GView 'Locked --> IO) -> CallbackViewLifted t -> t

instance GTKCallbackType (IO r) where
    type CallbackViewLifted (IO r) = GView 'Locked r
    gCallbackUnlift mf v = mf v

instance GTKCallbackType r => GTKCallbackType (a -> r) where
    type CallbackViewLifted (a -> r) = a -> CallbackViewLifted r
    gCallbackUnlift mf av a = gCallbackUnlift mf $ av a

gvCloseDisconnectSignal :: IsObject object => object -> SignalHandlerId -> GView 'Locked ()
gvCloseDisconnectSignal object shid =
    gvOnClose $
    gvLiftIO $ do
        -- Widgets that have been destroyed have already had their signals disconnected, even if references to them still exist.
        -- So we need to check.
        isConnected <- signalHandlerIsConnected object shid
        if isConnected
            then liftIO $ disconnectSignalHandler object shid
            else return ()

gvOnSignal ::
       (IsObject object, SignalInfo info, GTKCallbackType (HaskellCallbackType info))
    => object
    -> SignalProxy object info
    -> CallbackViewLifted (HaskellCallbackType info)
    -> GView 'Locked SignalHandlerId
gvOnSignal object signal call = do
    shid <- gvWithUnliftAsync $ \unlift -> on object signal $ gCallbackUnlift unlift call
    gvCloseDisconnectSignal object shid
    return shid

gvAfterSignal ::
       (IsObject object, SignalInfo info, GTKCallbackType (HaskellCallbackType info))
    => object
    -> SignalProxy object info
    -> CallbackViewLifted (HaskellCallbackType info)
    -> GView 'Locked SignalHandlerId
gvAfterSignal object signal call = do
    shid <- gvWithUnliftAsync $ \unlift -> after object signal $ gCallbackUnlift unlift call
    gvCloseDisconnectSignal object shid
    return shid

getTypeSignalIDs :: MonadIO m => GType -> m [Word32]
getTypeSignalIDs t = do
    gbadtype <- liftIO $ glibType @InitiallyUnowned
    tt <- getTypeAncestry t
    sigidss <-
        for tt $ \t0 ->
            if t0 == gbadtype
                then return []
                else signalListIds t0
    return $ nub $ mconcat sigidss

gvTraceSignal :: Word32 -> ([GValue] -> GView 'Locked ()) -> GView 'Locked ()
gvTraceSignal sigid call = do
    sq <- signalQuery sigid
    sflags <- getSignalQuerySignalFlags sq
    if elem SignalFlagsNoHooks sflags
        then return ()
        else do
            hookid <-
                liftIOWithUnlift $ \unliftIO ->
                    signalAddEmissionHook sigid 0 $ \_ vals _ -> do
                        unliftIO $ call vals
                        return True
            gvOnClose $ gvLiftIO $ signalRemoveEmissionHook sigid hookid

gvObjectTraceSignal :: IsObject t => t -> Word32 -> GView 'Locked () -> GView 'Locked ()
gvObjectTraceSignal t sigid call = do
    obj <- toObject t
    gvTraceSignal sigid $ \case
        [] -> return ()
        sval:_ -> do
            msobj <- fromGValue sval
            case msobj of
                Just sobj
                    | sobj == obj -> call
                _ -> return ()

gvObjectTraceAllSignals :: IsObject t => t -> (Text -> GView 'Locked ()) -> GView 'Locked ()
gvObjectTraceAllSignals obj call = do
    t <- getObjectType obj
    sigids <- getTypeSignalIDs t
    for_ sigids $ \sigid -> do
        signame <- signalName sigid
        case signame of
            --"event" -> return ()
            --"event-after" -> return ()
            "motion-notify-event" -> return ()
            _ -> gvObjectTraceSignal obj sigid $ call signame

gvObjectReportAllSignals :: IsObject t => Text -> t -> GView 'Locked ()
gvObjectReportAllSignals name obj =
    gvObjectTraceAllSignals obj $ \signame -> traceIOM $ "SIGNAL: " <> unpack name <> ": " <> show signame
