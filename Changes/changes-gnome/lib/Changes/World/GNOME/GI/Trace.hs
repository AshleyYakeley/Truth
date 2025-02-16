module Changes.World.GNOME.GI.Trace
    ( gvTraceBracket
    , gvTraceSignal
    , gvObjectTraceSignal
    , gvObjectTraceAllSignals
    , gvObjectReportAllSignals
    )
where

import Changes.Debug

import Changes.World.GNOME.GI.GView
import Changes.World.GNOME.GI.LockState
import Changes.World.GNOME.GI.Type
import Import
import Import.GI qualified as GI

gvTraceBracket :: String -> GView ls --> GView ls
gvTraceBracket s = gvHoistView $ traceBracket s

getTypeSignalIDs :: MonadIO m => GI.GType -> m [Word32]
getTypeSignalIDs t = do
    gbadtype <- liftIO $ GI.glibType @GI.InitiallyUnowned
    tt <- getTypeAncestry t
    sigidss <-
        for tt $ \t0 ->
            if t0 == gbadtype
                then return []
                else GI.signalListIds t0
    return $ nub $ mconcat sigidss

gvTraceSignal :: Word32 -> ([GI.GValue] -> GView 'Locked ()) -> GView 'Locked ()
gvTraceSignal sigid call = do
    sq <- GI.signalQuery sigid
    sflags <- GI.getSignalQuerySignalFlags sq
    if elem GI.SignalFlagsNoHooks sflags
        then return ()
        else do
            hookid <-
                liftIOWithUnlift $ \unliftIO ->
                    GI.signalAddEmissionHook sigid 0 $ \_ vals _ -> do
                        unliftIO $ call vals
                        return True
            gvOnClose $ gvLiftIO $ GI.signalRemoveEmissionHook sigid hookid

gvObjectTraceSignal :: GI.IsObject t => t -> Word32 -> GView 'Locked () -> GView 'Locked ()
gvObjectTraceSignal t sigid call = do
    obj <- GI.toObject t
    gvTraceSignal sigid $ \case
        [] -> return ()
        sval : _ -> do
            msobj <- GI.fromGValue sval
            case msobj of
                Just sobj
                    | sobj == obj -> call
                _ -> return ()

gvObjectTraceAllSignals :: GI.IsObject t => t -> (Text -> GView 'Locked ()) -> GView 'Locked ()
gvObjectTraceAllSignals obj call = do
    t <- getObjectType obj
    sigids <- getTypeSignalIDs t
    for_ sigids $ \sigid -> do
        msigname <- GI.signalName sigid
        case msigname of
            -- Just "event" -> return ()
            -- Just "event-after" -> return ()
            Just "motion-notify-event" -> return ()
            Just signame -> gvObjectTraceSignal obj sigid $ call signame
            Nothing -> return ()

gvObjectReportAllSignals :: GI.IsObject t => Text -> t -> GView 'Locked ()
gvObjectReportAllSignals name obj =
    gvObjectTraceAllSignals obj $ \signame -> traceIOM $ "SIGNAL: " <> unpack name <> ": " <> show signame
