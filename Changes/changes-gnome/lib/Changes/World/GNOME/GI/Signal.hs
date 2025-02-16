module Changes.World.GNOME.GI.Signal
    ( withSignalBlocked
    , withSignalsBlocked
    , gvOnSignal
    , gvAfterSignal
    )
where

import Changes.World.GNOME.GI.GView
import Changes.World.GNOME.GI.LockState
import Import
import Import.GI qualified as GI

withSignalBlocked :: (Is LockStateType ls, GI.IsObject obj) => obj -> GI.SignalHandlerId -> GView ls --> GView ls
withSignalBlocked obj conn =
    bracketNoMask_
        (gvRunLocked' $ gvLiftIO $ GI.signalHandlerBlock obj conn)
        (gvRunLocked' $ gvLiftIO $ GI.signalHandlerUnblock obj conn)

withSignalsBlocked :: (Is LockStateType ls, GI.IsObject obj) => obj -> [GI.SignalHandlerId] -> GView ls --> GView ls
withSignalsBlocked _obj [] = id
withSignalsBlocked obj (c : cc) = withSignalBlocked obj c . withSignalsBlocked obj cc

class GTKCallbackType t where
    type CallbackViewLifted t :: Type
    gCallbackUnlift :: (GView 'Locked --> IO) -> CallbackViewLifted t -> t

instance GTKCallbackType (IO r) where
    type CallbackViewLifted (IO r) = GView 'Locked r
    gCallbackUnlift mf v = mf v

instance GTKCallbackType r => GTKCallbackType (a -> r) where
    type CallbackViewLifted (a -> r) = a -> CallbackViewLifted r
    gCallbackUnlift mf av a = gCallbackUnlift mf $ av a

gvOnCloseDisconnectSignal :: GI.IsObject object => object -> GI.SignalHandlerId -> GView 'Locked ()
gvOnCloseDisconnectSignal object shid =
    gvOnClose
        $ gvLiftIO
        $ do
            -- Widgets that have been destroyed have already had their signals disconnected, even if references to them still exist.
            -- So we need to check.
            isConnected <- GI.signalHandlerIsConnected object shid
            if isConnected
                then liftIO $ GI.disconnectSignalHandler object shid
                else return ()

gvOnSignal ::
    (GI.IsObject object, GI.SignalInfo info, GTKCallbackType (GI.HaskellCallbackType info)) =>
    object ->
    GI.SignalProxy object info ->
    CallbackViewLifted (GI.HaskellCallbackType info) ->
    GView 'Locked GI.SignalHandlerId
gvOnSignal object signal call = do
    shid <- gvWithUnliftLockedAsync $ \unlift -> GI.on object signal $ gCallbackUnlift unlift call
    gvOnCloseDisconnectSignal object shid
    return shid

gvAfterSignal ::
    (GI.IsObject object, GI.SignalInfo info, GTKCallbackType (GI.HaskellCallbackType info)) =>
    object ->
    GI.SignalProxy object info ->
    CallbackViewLifted (GI.HaskellCallbackType info) ->
    GView 'Locked GI.SignalHandlerId
gvAfterSignal object signal call = do
    shid <- gvWithUnliftLockedAsync $ \unlift -> GI.after object signal $ gCallbackUnlift unlift call
    gvOnCloseDisconnectSignal object shid
    return shid
