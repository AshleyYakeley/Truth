module Control.Stream.Source where

import Data.ByteString
import System.IO.Error

import Control.Stream.Filter
import Control.Stream.Sink
import Data.Lens
import Shapes.Import

data Source m a = MkSource
    { sourceHasData :: m Bool
    , sourceTake :: m (ItemOrEnd a)
    }

instance Functor m => Functor (Source m) where
    fmap ab (MkSource hd stake) = MkSource hd $ fmap (fmap ab) stake

nullSource :: Monad m => Source m a
nullSource = MkSource (return True) (return End)

listSource :: [a] -> IO (Source IO a)
listSource aa = do
    var <- newMVar aa
    return
        $ MkSource (return True)
        $ do
            old <- takeMVar var
            case old of
                [] -> return End
                (a : ar) -> do
                    putMVar var ar
                    return $ Item a

hoistSource :: (m1 --> m2) -> Source m1 --> Source m2
hoistSource mm (MkSource hd stake) = MkSource (mm hd) (mm stake)

mvarSource :: forall a. MVar (ItemOrEnd a) -> Source IO a
mvarSource var = let
    sourceTake :: IO (ItemOrEnd a)
    sourceTake = takeMVar var
    sourceHasData :: IO Bool
    sourceHasData = fmap isJust $ tryReadMVar var
    in MkSource{..}

sourceTakeAvailable :: Monad m => Source m a -> m (Maybe (ItemOrEnd a))
sourceTakeAvailable source = do
    hd <- sourceHasData source
    if hd
        then fmap Just $ sourceTake source
        else return Nothing

sourceTakeAllAvailable :: Monad m => Source m a -> m ([a], Bool)
sourceTakeAllAvailable source = do
    mea <- sourceTakeAvailable source
    case mea of
        Nothing -> return ([], False)
        Just End -> return ([], True)
        Just (Item a) -> do
            (aa, isend) <- sourceTakeAllAvailable source
            return (a : aa, isend)

filterSource ::
    forall m a b.
    MonadIO m =>
    Filter m a b ->
    Source m a ->
    IO (Source m b)
filterSource (MkFilter (s0 :: s) f) source = do
    var <- newMVar (s0, [])
    let
        runStep :: ItemOrEnd a -> StateT s m [ItemOrEnd b]
        runStep ea = do
            bb <- f ea
            return
                $ case ea of
                    Item _ -> fmap Item bb
                    End -> fmap Item bb <> [End]
        stepAvailable :: [ItemOrEnd b] -> StateT s m [ItemOrEnd b]
        stepAvailable [] = do
            mea <- lift $ sourceTakeAvailable source
            case mea of
                Nothing -> return []
                Just ea -> do
                    newbb <- runStep ea
                    stepAvailable newbb
        stepAvailable bb = return bb
        hsd :: m Bool
        hsd =
            dangerousMVarRunStateT var $ do
                oldbb <- lensStateT sndLens $ get
                newbb <- lensStateT fstLens $ stepAvailable oldbb
                lensStateT sndLens $ put newbb
                return
                    $ case newbb of
                        _ : _ -> True
                        [] -> False
        stepTake :: [ItemOrEnd b] -> StateT s m (NonEmpty (ItemOrEnd b))
        stepTake [] = do
            ea <- lift $ sourceTake source
            newbb <- runStep ea
            stepTake newbb
        stepTake (b : bb) = return $ b :| bb
        stake :: m (ItemOrEnd b)
        stake =
            dangerousMVarRunStateT var $ do
                oldbb <- lensStateT sndLens $ get
                b :| newbb <- lensStateT fstLens $ stepTake oldbb
                lensStateT sndLens $ put newbb
                return b
    return $ MkSource hsd stake

connectSourceSink :: Monad m => Source m a -> Sink m a -> m ()
connectSourceSink source sink = do
    ma <- sourceTake source
    case ma of
        End -> return ()
        Item a -> do
            sinkWrite sink a
            connectSourceSink source sink

sourceGather :: MonadIO m => Source m a -> m [a]
sourceGather source = do
    (sink, getr, _) <- liftIO gatherSink
    connectSourceSink source $ hoistSink liftIO sink
    liftIO $ sinkWriteEnd sink
    liftIO $ getr

createPipe :: forall a. IO (Sink IO a, Source IO a)
createPipe = do
    bufferVar <- newTVarIO []
    let
        sink =
            MkSink $ \ea ->
                atomically $ do
                    aa <- readTVar bufferVar
                    writeTVar bufferVar $ aa <> [ea]
        source = let
            sourceHasData =
                atomically $ do
                    aa <- readTVar bufferVar
                    return
                        $ case aa of
                            [] -> False
                            _ : _ -> True
            sourceTake =
                atomically $ do
                    aa <- readTVar bufferVar
                    case aa of
                        [] -> mzero
                        a : ar -> do
                            writeTVar bufferVar ar
                            return a
            in MkSource{..}
    return (sink, source)

handleSource :: Handle -> Source IO StrictByteString
handleSource h = let
    sourceHasData =
        catch (hReady h) $ \err ->
            if isEOFError err
                then return True
                else throw err
    sourceTake = do
        eof <- hIsEOF h
        if eof
            then return End
            else do
                d <- hGetSome h 1000000
                return $ Item d
    in MkSource{..}

stdinTextSource :: Source IO Text
stdinTextSource = fmap decodeUtf8Lenient $ handleSource stdin
