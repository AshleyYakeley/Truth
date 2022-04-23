module Control.Clock
    ( clock
    ) where

import Control.Monad.LifeCycle
import Data.Fixed
import Data.Time.Clock
import Shapes.Import

data Cancelled =
    MkCancelled
    deriving (Show)

instance Exception Cancelled

clock :: UTCTime -> NominalDiffTime -> (UTCTime -> IO ()) -> LifeCycle UTCTime
clock utcBase ndtInterval call = do
    let
        getDiffTimes :: IO (NominalDiffTime, NominalDiffTime)
        getDiffTimes = do
            utcCurrent <- getCurrentTime
            let
                ndtTime = diffUTCTime utcCurrent utcBase
                ndtOffset = mod' ndtTime ndtInterval
            return (ndtTime, ndtOffset)
    var <- liftIO newEmptyMVar
    thread <-
        liftIO $
        forkIO $
        handle (\MkCancelled -> return ()) $
        forever $ do
            (ndtTime, ndtOffset) <- getDiffTimes
            let
                ndtRemaining = ndtInterval - ndtOffset
                ndtNext = ndtTime + ndtRemaining
            putMVar var ()
            threadDelay $ truncate $ (nominalDiffTimeToSeconds ndtRemaining) * 1E6
            takeMVar var
            call $ addUTCTime ndtNext utcBase
    (ndtTime, ndtOffset) <- liftIO getDiffTimes
    lifeCycleCloser $ do
        takeMVar var
        throwTo thread MkCancelled
    return $ addUTCTime (ndtTime - ndtOffset) utcBase
