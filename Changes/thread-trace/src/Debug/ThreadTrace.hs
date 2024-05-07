module Debug.ThreadTrace
    ( contextStr
    , traceNameThread
    , traceHide
    , traceShow
    , traceHidePrefix
    , traceIOM
    , traceBracketArgs
    , traceBracket_
    , traceBracketWithLift
    , DebugMonadIO
    , traceBracket
    , traceTime
    , traceCost
    , traceCosts
    , traceThread
    , traceForkIO
    , traceEvaluate
    , traceBarrier_
    , traceBarrier
    , traceMVarRunStateT
    , tracePure
    , tracePureM
    , tracePureBracket
    , TraceThing(..)
    , traceSTM
    , traceBracketSTM
    , module Debug.ThreadTrace.Lookup
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Ology
import Data.Fixed
import Data.Foldable
import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Maybe
import Data.Time.Clock.System
import Data.Word
import Debug.ThreadTrace.Lookup
import Debug.Trace (traceIO)
import GHC.Conc
import Prelude
import qualified System.Clock as Clock
import System.IO.Unsafe

contextStr :: String -> String -> String
contextStr "" b = b
contextStr a b = a ++ ": " ++ b

data ThreadData = MkThreadData
    { tdName :: String
    , tdShow :: Bool
    , tdShowPrefix :: Bool
    , tdShowCosts :: Bool
    , tdCosts :: Map.Map String Nano
    }

defaultThreadData :: ThreadData
defaultThreadData = let
    tdName = ""
    tdShow = True
    tdShowPrefix = True
    tdShowCosts = False
    tdCosts = mempty
    in MkThreadData {..}

traceThreadData :: MVar (Map.Map ThreadId ThreadData)
{-# NOINLINE traceThreadData #-}
traceThreadData = unsafePerformIO $ newMVar mempty

threadGetData :: IO ThreadData
threadGetData = do
    tid <- myThreadId
    tdmap <- readMVar traceThreadData
    return $
        case Map.lookup tid tdmap of
            Just tdata -> tdata
            Nothing -> defaultThreadData

threadModifyData :: (ThreadData -> ThreadData) -> IO ()
threadModifyData m = do
    tid <- myThreadId
    modifyMVar_ traceThreadData $ \tdmap ->
        return $ let
            oldtdata =
                case Map.lookup tid tdmap of
                    Just tdata -> tdata
                    Nothing -> defaultThreadData
            newtdata = m oldtdata
            in Map.insert tid newtdata tdmap

traceNameThread :: String -> IO ()
traceNameThread name = threadModifyData $ \tdata -> tdata {tdName = name}

traceSaveData :: MonadIO m => m r -> m r
traceSaveData mr = do
    tdata <- liftIO threadGetData
    a <- mr
    liftIO $ threadModifyData $ \_ -> tdata
    return a

traceHide :: MonadIO m => m r -> m r
traceHide mr =
    traceSaveData $ do
        liftIO $ threadModifyData $ \tdata -> tdata {tdShow = False}
        mr

traceShow :: MonadIO m => m r -> m r
traceShow mr =
    traceSaveData $ do
        liftIO $ threadModifyData $ \tdata -> tdata {tdShow = True}
        mr

traceHidePrefix :: MonadIO m => m r -> m r
traceHidePrefix mr =
    traceSaveData $ do
        liftIO $ threadModifyData $ \tdata -> tdata {tdShowPrefix = False}
        mr

traceIOM :: MonadIO m => String -> m ()
traceIOM msg =
    liftIO $ do
        tdata <- threadGetData
        if tdShow tdata
            then if tdShowPrefix tdata
                     then do
                         MkSystemTime s ns <- getSystemTime
                         tid <- myThreadId
                         let
                             threadtext =
                                 case show tid of
                                     'T':'h':'r':'e':'a':'d':'I':'d':' ':t -> '#' : t
                                     t -> t
                             nametxt =
                                 case tdName tdata of
                                     "" -> ""
                                     name -> " (" ++ name ++ ")"
                             showMod :: Int -> Word32 -> String
                             showMod 0 _ = ""
                             showMod n x = showMod (pred n) (div x 10) <> show (mod x 10)
                         traceIO $ show s <> "." <> showMod 9 ns <> ": " <> threadtext <> nametxt <> ": " <> msg
                     else traceIO msg
            else return ()

traceBracketArgs :: MonadIO m => String -> String -> (r -> String) -> m r -> m r
traceBracketArgs s args showr ma = do
    traceIOM $
        s ++
        " [" ++
        (if null args
             then ""
             else " " ++ args)
    a <- ma
    let ret = showr a
    traceIOM $
        s ++
        " ]" ++
        (if null ret
             then ""
             else " => " ++ ret)
    return a

traceBracket_ :: MonadIO m => String -> m r -> m r
traceBracket_ s = traceBracketArgs s "" (\_ -> "")

traceBracketWithLift :: Monad m => (IO () -> m ()) -> String -> m r -> m r
traceBracketWithLift lft s ma = do
    lft $ traceIOM $ s ++ " ["
    a <- ma
    lft $ traceIOM $ s ++ " ]"
    return a

type DebugMonadIO m = (MonadException m, Show (Exc m), MonadIO m)

getSeconds :: IO Nano
getSeconds = do
    Clock.TimeSpec s ns <- Clock.getTime Clock.ProcessCPUTime
    return $ fromIntegral s + fromIntegral ns / 1000000000

getCost :: MonadIO m => m r -> m (Nano, r)
getCost ma = do
    t0 <- liftIO getSeconds
    a <- ma
    t1 <- liftIO getSeconds
    return (t1 - t0, a)

traceTime :: MonadIO m => m r -> m r
traceTime ma = do
    (c, a) <- getCost ma
    traceIOM $ "timed: " <> show c <> "s"
    return a

traceBracket :: DebugMonadIO m => String -> m r -> m r
traceBracket s ma = do
    traceIOM $ s ++ " ["
    catchExc
        (do
             a <- ma
             traceIOM $ s ++ " ]"
             return a) $ \e -> do
        traceIOM $ s ++ " ! " ++ show e
        throwExc e

addCost :: (Ord key, Num cc) => key -> cc -> Map.Map key cc -> Map.Map key cc
addCost k c m = let
    oldc = fromMaybe 0 $ Map.lookup k m
    newc = c + oldc
    in Map.insert k newc m

traceCost :: MonadIO m => String -> m r -> m r
traceCost key ma = do
    oldtdata <- liftIO $ threadGetData
    let
        w =
            if tdShowCosts oldtdata
                then traceBracket_ ("COST: " <> key)
                else id
    w $ do
        (c, a) <- getCost ma
        liftIO $ threadModifyData $ \tdata -> tdata {tdCosts = addCost key c (tdCosts tdata)}
        return a

traceCosts :: MonadIO m => Bool -> m r -> m r
traceCosts sh ma = do
    liftIO $ threadModifyData $ \tdata -> tdata {tdShowCosts = sh, tdCosts = mempty}
    a <- ma
    tdata <- liftIO $ threadGetData
    for_ (sortOn fst $ Map.toList $ tdCosts tdata) $ \(k, c) -> traceIOM $ k <> ": " <> show c <> "s"
    return a

traceThread :: String -> IO r -> IO r
traceThread name ma = do
    traceNameThread name
    traceBracket "THREAD" ma

traceForkIO :: String -> IO () -> IO ThreadId
traceForkIO name ma = traceBracketArgs "FORK" "" show $ forkIO $ traceThread name ma

traceEvaluate :: MonadIO m => String -> r -> m r
traceEvaluate s a = liftIO $ traceBracket ("evaluate " <> s) $ evaluate a

traceBarrier_ :: (MonadIO m1, MonadIO m2) => String -> (m1 a -> m2 b) -> m1 a -> m2 b
traceBarrier_ s tr ma = traceBracket_ (contextStr s "outside") $ tr $ traceBracket_ (contextStr s "inside") ma

traceBarrier :: (DebugMonadIO m1, DebugMonadIO m2) => String -> (m1 a -> m2 b) -> m1 a -> m2 b
traceBarrier s tr ma = traceBracket (contextStr s "outside") $ tr $ traceBracket (contextStr s "inside") ma

traceMVarRunStateT :: String -> MVar s -> Unlift MonadTunnelIO (StateT s)
traceMVarRunStateT s var ma = do
    name <- lookupMVar var
    traceBarrier_ (s <> " on " <> name) (mVarRunStateT var) ma

{-# NOINLINE tracePure #-}
tracePure :: String -> a -> a
tracePure s a =
    unsafePerformIO $ do
        traceIOM s
        return a

tracePureM :: Applicative m => String -> m ()
tracePureM s = tracePure s $ pure ()

tracePureBracket :: Monad m => String -> m a -> m a
tracePureBracket s ma = do
    () <- tracePureM $ s ++ " ["
    a <- ma
    () <- tracePureM $ s ++ " ]"
    return a

class TraceThing t where
    traceThing :: String -> t -> t

instance {-# OVERLAPPABLE #-} MonadIO m => TraceThing (m a) where
    traceThing s ma = traceBracket_ s ma

instance TraceThing t => TraceThing (a -> t) where
    traceThing s at a = traceThing s $ at a

traceSTM :: String -> STM ()
traceSTM msg = unsafeIOToSTM $ traceIOM msg

traceBracketSTM :: String -> STM a -> STM a
traceBracketSTM s ma = do
    traceSTM $ s ++ " ["
    a <-
        ma <|> do
            traceSTM $ s ++ " ] RETRY"
            empty
    traceSTM $ s ++ " ]"
    return a
