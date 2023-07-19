module Changes.World.GNOME.GI.Trace
    ( gvTraceSignal
    , gvObjectTraceSignal
    , gvObjectTraceAllSignals
    , gvObjectReportAllSignals
    ) where

import Changes.Debug
import Changes.World.GNOME.GI.GView
import Changes.World.GNOME.GI.LockState
import Changes.World.GNOME.GI.Type
import Data.GI.Base
import GI.GObject
import Shapes

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
                    signalAddEmissionHook sigid 0 $ \_ vals -> do
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
        msigname <- signalName sigid
        case msigname of
            --Just "event" -> return ()
            --Just "event-after" -> return ()
            Just "motion-notify-event" -> return ()
            Just signame -> gvObjectTraceSignal obj sigid $ call signame
            Nothing -> return ()

gvObjectReportAllSignals :: IsObject t => Text -> t -> GView 'Locked ()
gvObjectReportAllSignals name obj =
    gvObjectTraceAllSignals obj $ \signame -> traceIOM $ "SIGNAL: " <> unpack name <> ": " <> show signame
