module Truth.UI.GTK.Useful where

import Data.GI.Base.Signals
import Data.GI.Gtk
import Data.IORef
import GI.GObject
import Shapes
import Truth.Core

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
    -> View edit a
    -> View edit SignalHandlerId
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
