module Changes.GI.Signal
    ( withSignalBlocked
    , withSignalsBlocked
    , cvOn
    , cvAfter
    , cvTraceSignal
    , cvTraceAllSignals
    , cvReportAllSignals
    ) where

import Changes.Core
import Changes.GI.Type
import Data.GI.Base
import Data.GI.Base.Signals
import GI.GObject
import Shapes
import Changes.Debug.Reference

withSignalBlocked :: IsObject obj => obj -> SignalHandlerId -> View a -> View a
withSignalBlocked obj conn = hoist $ bracket_ (signalHandlerBlock obj conn) (signalHandlerUnblock obj conn)

withSignalsBlocked :: IsObject obj => obj -> [SignalHandlerId] -> View a -> View a
withSignalsBlocked _obj [] = id
withSignalsBlocked obj (c:cc) = withSignalBlocked obj c . withSignalsBlocked obj cc

class GTKCallbackType t where
    type CallbackViewLifted t :: Type
    gCallbackUnlift :: (View --> IO) -> CallbackViewLifted t -> t

instance GTKCallbackType (IO r) where
    type CallbackViewLifted (IO r) = View r
    gCallbackUnlift mf v = mf v

instance GTKCallbackType r => GTKCallbackType (a -> r) where
    type CallbackViewLifted (a -> r) = a -> CallbackViewLifted r
    gCallbackUnlift mf av a = gCallbackUnlift mf $ av a

cvCloseDisconnectSignal :: IsObject object => object -> SignalHandlerId -> CreateView ()
cvCloseDisconnectSignal object shid =
    lifeCycleCloseIO $ do
        -- Widgets that have been destroyed have already had their signals disconnected, even if references to them still exist.
        -- So we need to check.
        isConnected <- traceBracket "GTK.cvCloseDisconnectSignal:test" $ signalHandlerIsConnected object shid
        if isConnected
            then traceBracket "GTK.cvCloseDisconnectSignal:disconnect" $ disconnectSignalHandler object shid
            else return ()

cvOn ::
       (IsObject object, SignalInfo info, GTKCallbackType (HaskellCallbackType info))
    => object
    -> SignalProxy object info
    -> CallbackViewLifted (HaskellCallbackType info)
    -> CreateView SignalHandlerId
cvOn object signal call = do
    shid <- lift $ liftIOViewAsync $ \unlift -> on object signal $ gCallbackUnlift (\ma -> traceBracketIO "THREAD: GTK on" $ unlift ma) call
    cvCloseDisconnectSignal object shid
    return shid

cvAfter ::
       (IsObject object, SignalInfo info, GTKCallbackType (HaskellCallbackType info))
    => object
    -> SignalProxy object info
    -> CallbackViewLifted (HaskellCallbackType info)
    -> CreateView SignalHandlerId
cvAfter object signal call = do
    shid <- lift $ liftIOViewAsync $ \unlift -> after object signal $ gCallbackUnlift (\ma -> traceBracketIO "THREAD: GTK after" $ unlift ma) call
    cvCloseDisconnectSignal object shid
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

cvTraceSignal :: IsObject t => t -> Word32 -> View () -> CreateView ()
cvTraceSignal t sigid call = do
    sq <- signalQuery sigid
    sflags <- getSignalQuerySignalFlags sq
    if elem SignalFlagsNoHooks sflags
        then return ()
        else do
            unliftIO <- lift askUnliftIO
            hookid <-
                signalAddEmissionHook sigid 0 $ \_ vals _ -> do
                    case vals of
                        [] -> return ()
                        sval:_ -> do
                            obj <- toObject t
                            msobj <- fromGValue sval
                            case msobj of
                                Just sobj
                                    | sobj == obj -> runWMFunction unliftIO call
                                _ -> return ()
                    return True
            lifeCycleClose $ signalRemoveEmissionHook sigid hookid

cvTraceAllSignals :: IsObject t => t -> (Text -> View ()) -> CreateView ()
cvTraceAllSignals obj call = do
    t <- getObjectType obj
    sigids <- getTypeSignalIDs t
    for_ sigids $ \sigid -> do
        signame <- signalName sigid
        case signame of
            "event" -> return ()
            "event-after" -> return ()
            "motion-notify-event" -> return ()
            _ -> cvTraceSignal obj sigid $ call signame

cvReportAllSignals :: IsObject t => Text -> t -> CreateView ()
cvReportAllSignals name obj =
    cvTraceAllSignals obj $ \signame -> liftIO $ hPutStrLn stderr $ unpack name <> ": signal " <> show signame
