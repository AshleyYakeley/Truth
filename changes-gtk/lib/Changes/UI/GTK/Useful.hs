module Changes.UI.GTK.Useful
    ( cvGetObjectTypeName
    , widgetGetTree
    , withSignalBlocked
    , withSignalsBlocked
    , cvOn
    , cvAfter
    , cvAcquire
    , cvNew
    , cvTopLevelNew
    , cvSet
    , cvAdd
    , cvPackStart
    , seqStoreTraverse_
    , isScrollable
    ) where

import Changes.Core
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.GI.Base.GObject
import Data.GI.Base.Signals
import Data.GI.Gtk
import Data.IORef
import GI.GObject
import Shapes
import Changes.Debug.Reference

containerGetAllChildren :: Container -> IO [Widget]
containerGetAllChildren cont = do
    ref <- newIORef []
    containerForall cont $ \child -> do
        children <- readIORef ref
        writeIORef ref $ children ++ [child]
    readIORef ref

widgetGetTree :: Bool -> Widget -> IO [Widget]
widgetGetTree full w = do
    mwc <- castTo Container w
    case mwc of
        Just wc -> do
            children <-
                (if full
                     then containerGetAllChildren
                     else containerGetChildren) $
                wc
            ww <- for children $ widgetGetTree full
            return $ w : mconcat ww
        Nothing -> return [w]

withSignalBlocked :: IsObject obj => obj -> SignalHandlerId -> View a -> View a
withSignalBlocked obj conn = remonad $ bracket_ (signalHandlerBlock obj conn) (signalHandlerUnblock obj conn)

withSignalsBlocked :: IsObject obj => obj -> [SignalHandlerId] -> View a -> View a
withSignalsBlocked _obj [] = id
withSignalsBlocked obj (c:cc) = withSignalBlocked obj c . withSignalsBlocked obj cc

class GTKCallbackType t where
    type CallbackViewLifted t :: Type
    gCallbackUnlift :: MFunction View IO -> CallbackViewLifted t -> t

instance GTKCallbackType (IO r) where
    type CallbackViewLifted (IO r) = View r
    gCallbackUnlift mf v = mf v

instance GTKCallbackType r => GTKCallbackType (a -> r) where
    type CallbackViewLifted (a -> r) = a -> CallbackViewLifted r
    gCallbackUnlift mf av a = gCallbackUnlift mf $ av a

cvCloseDisconnectSignal :: IsObject object => object -> SignalHandlerId -> CreateView ()
cvCloseDisconnectSignal object shid =
    lifeCycleClose $ do
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
    shid <- liftToLifeCycle $ liftIOViewAsync $ \unlift -> on object signal $ gCallbackUnlift (\ma -> traceBracketIO "THREAD: GTK on" $ unlift ma) call
    cvCloseDisconnectSignal object shid
    return shid

cvAfter ::
       (IsObject object, SignalInfo info, GTKCallbackType (HaskellCallbackType info))
    => object
    -> SignalProxy object info
    -> CallbackViewLifted (HaskellCallbackType info)
    -> CreateView SignalHandlerId
cvAfter object signal call = do
    shid <- liftToLifeCycle $ liftIOViewAsync $ \unlift -> after object signal $ gCallbackUnlift (\ma -> traceBracketIO "THREAD: GTK after" $ unlift ma) call
    cvCloseDisconnectSignal object shid
    return shid

newtype Change m a =
    MkChange (a -> m (Maybe a))

instance Monad m => Semigroup (Change m a) where
    MkChange t1 <> MkChange t2 =
        MkChange $ \a0 -> do
            ma1 <- t1 a0
            case ma1 of
                Just a1 -> do
                    ma2 <- t2 a1
                    return $
                        Just $
                        case ma2 of
                            Just a2 -> a2
                            Nothing -> a1
                Nothing -> t2 a0

instance Monad m => Monoid (Change m a) where
    mempty = MkChange $ \_ -> return Nothing

seqStoreTraverse_ :: MonadIO m => SeqStore a -> Change m a -> m ()
seqStoreTraverse_ store (MkChange f) = do
    n <- seqStoreGetSize store
    for_ [0 .. (n - 1)] $ \i -> do
        oldval <- seqStoreGetValue store i
        mnewval <- f oldval
        case mnewval of
            Just newval -> seqStoreSetValue store i newval
            Nothing -> return ()

isScrollable :: GObject widget => widget -> IO Bool
isScrollable widget = do
    mViewport <- castTo Viewport widget
    mTextView <- castTo TextView widget
    return $
        case (mViewport, mTextView) of
            (Nothing, Nothing) -> False
            _ -> True

cvAcquire :: IsObject a => a -> CreateView ()
cvAcquire a = do
    _ <- traceBracket "GTK.cvAcquire:ref" $ objectRef a
    lifeCycleClose $ traceBracket "GTK.cvAcquire:unref" $ objectUnref a
    return ()

cvGetObjectTypeName :: IsObject a => a -> CreateView String
cvGetObjectTypeName a = do
    objtype <- liftIO $ gtypeFromInstance a
    tname <- typeName objtype
    return $ unpack tname

cvNew :: (Constructible a tag, IsObject a) => (ManagedPtr a -> a) -> [AttrOp a tag] -> CreateView a
cvNew cc attrs = do
    a <- traceBracket "cvNew.new" $ new cc attrs
    cvAcquire a
    return a

-- | Probably only use this for top-level widgets
cvTopLevelNew :: (Constructible a tag, IsObject a, IsWidget a) => (ManagedPtr a -> a) -> [AttrOp a tag] -> CreateView a
cvTopLevelNew cc attrs = do
    a <- cvNew cc attrs
    lifeCycleClose $ traceBracket "GTK.cvTopLevelNew:destroy" $ widgetDestroy a
    return a

cvSet ::
       (AttrClearC info obj attr, AttrSetC info obj attr value) => obj -> AttrLabelProxy attr -> value -> CreateView ()
cvSet obj prop val = do
    set obj [prop := val]
    lifeCycleClose $ clear obj prop

cvAdd :: (IsContainer c, IsWidget w) => c -> w -> CreateView ()
cvAdd c w = do
    containerAdd c w
    lifeCycleClose $ containerRemove c w

cvPackStart :: (IsObject w, IsContainer box, IsBox box, IsWidget w) => Bool -> box -> w -> CreateView ()
cvPackStart grow box w = do
    boxPackStart box w grow grow 0
    lifeCycleClose $ containerRemove box w
