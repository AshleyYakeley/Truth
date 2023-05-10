module Changes.World.GNOME.GI.Signal
    ( withSignalBlocked
    , withSignalsBlocked
    , gvOnSignal
    , gvAfterSignal
    ) where

import Changes.World.GNOME.GI.GView
import Changes.World.GNOME.GI.LockState
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
    shid <- gvWithUnliftLockedAsync $ \unlift -> on object signal $ gCallbackUnlift unlift call
    gvCloseDisconnectSignal object shid
    return shid

gvAfterSignal ::
       (IsObject object, SignalInfo info, GTKCallbackType (HaskellCallbackType info))
    => object
    -> SignalProxy object info
    -> CallbackViewLifted (HaskellCallbackType info)
    -> GView 'Locked SignalHandlerId
gvAfterSignal object signal call = do
    shid <- gvWithUnliftLockedAsync $ \unlift -> after object signal $ gCallbackUnlift unlift call
    gvCloseDisconnectSignal object shid
    return shid
