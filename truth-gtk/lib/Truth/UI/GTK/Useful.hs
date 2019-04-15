module Truth.UI.GTK.Useful where

import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.GI.Base.Signals
import Data.GI.Gtk
import Data.IORef
import GI.GLib
import GI.GObject
import GI.Gdk
import Shapes
import Truth.Core
import Truth.Debug.Object

-- | Returns immediately.
deferToIdle :: MonadAskUnliftIO m => m () -> m ()
deferToIdle action = do
    unlift <- askUnliftIO
    _ <-
        liftIO $
        threadsAddIdle PRIORITY_DEFAULT_IDLE $
        traceBracket "deferToIdle: action" $
        runTransform unlift $ do
            action
            return SOURCE_REMOVE
    return ()

-- | Waits for the task to be done.
runInIdle :: MonadAskUnliftIO m => m a -> m a
runInIdle action = do
    var <- liftIO newEmptyMVar
    deferToIdle $ do
        a <- action
        traceBracket "runInIdle: done MVar" $ liftIO $ putMVar var a
    traceBracket "runInIdle: waiting for MVar" $ liftIO $ takeMVar var

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

withSignalBlocked :: IsObject obj => obj -> SignalHandlerId -> IO a -> IO a
withSignalBlocked obj conn = bracket_ (signalHandlerBlock obj conn) (signalHandlerUnblock obj conn)

viewOn ::
       (GObject widget, SignalInfo info, HaskellCallbackType info ~ IO a)
    => widget
    -> SignalProxy widget info
    -> View sel edit a
    -> View sel edit SignalHandlerId
viewOn widget signal v = liftIOView $ \unlift -> on widget signal $ unlift v

joinTraverse :: Monad m => (a -> m (Maybe a)) -> (a -> m (Maybe a)) -> a -> m (Maybe a)
joinTraverse t1 t2 a0 = do
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

seqStoreTraverse_ :: MonadIO m => SeqStore a -> (a -> m (Maybe a)) -> m ()
seqStoreTraverse_ store f = do
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

-- | Probably only use this for top-level widgets
lcNewDestroy :: (MonadLifeCycle m, Constructible a tag, IsWidget a) => (ManagedPtr a -> a) -> [AttrOp a tag] -> m a
lcNewDestroy cc attrs =
    liftLifeCycle $ do
        a <- liftIO $ new cc attrs
        lifeCycleClose $ traceBracket "GTK.Widget:close" $ widgetDestroy a
        return a

lcSetClear ::
       (MonadLifeCycle m, AttrClearC info obj attr, AttrSetC info obj attr value)
    => obj
    -> AttrLabelProxy attr
    -> value
    -> m ()
lcSetClear obj prop val =
    liftLifeCycle $ do
        set obj [prop := val]
        lifeCycleClose $ clear obj prop

lcContain :: (MonadLifeCycle m, IsContainer c, IsWidget w) => c -> w -> m ()
lcContain c w =
    liftLifeCycle $ do
        containerAdd c w
        lifeCycleClose $ containerRemove c w

lcContainPackStart :: (MonadLifeCycle m, IsContainer box, IsBox box, IsWidget w) => Bool -> box -> w -> m ()
lcContainPackStart grow box w =
    liftLifeCycle $ do
        boxPackStart box w grow grow 0
        lifeCycleClose $ containerRemove box w

makeButton :: MonadIO m => Text -> IO () -> m Button
makeButton name action = do
    button <- new Button [#label := name]
    _ <- liftIO $ on button #clicked action
    return button

cvMakeButton :: Text -> View sel edit () -> CreateView sel edit Button
cvMakeButton name action = do
    unlift <- cvLiftView $ askUnliftIO
    makeButton name $ runTransform unlift action
