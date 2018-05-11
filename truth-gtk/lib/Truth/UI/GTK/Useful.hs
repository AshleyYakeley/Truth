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

deferToIdle :: MonadAskUnliftIO m => m () -> m ()
deferToIdle action = do
    unlift <- askUnliftIO
    _ <-
        liftIO $
        threadsAddIdle PRIORITY_DEFAULT $
        runUnliftIO unlift $ do
            action
            return False
    return ()

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
    -> View seledit edit a
    -> View seledit edit SignalHandlerId
viewOn widget signal v = liftIOView $ \unlift -> on widget signal $ unlift v

tryWithMVar :: MVar a -> (Maybe a -> IO b) -> IO b
tryWithMVar mv f = do
    ma <- tryTakeMVar mv
    finally (f ma) $
        case ma of
            Just a -> putMVar mv a
            Nothing -> return ()

ifMVar :: MVar () -> IO () -> IO ()
ifMVar mv f =
    tryWithMVar mv $ \ma ->
        case ma of
            Just _ -> f
            _ -> return ()

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
        lifeCycleClose $ widgetDestroy a
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

cvMakeButton :: Text -> View seledit edit () -> CreateView seledit edit Button
cvMakeButton name action = do
    unlift <- cvLiftView $ askUnliftIO
    makeButton name $ runUnliftIO unlift action
