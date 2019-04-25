module Control.Clock where

import Control.Monad.LifeCycleIO
import Control.Monad.Trans.LifeCycle
import Data.Time.Clock
import Data.Time.Clock.System
import Shapes.Import

data Cancelled =
    MkCancelled
    deriving (Show)

instance Exception Cancelled

clock :: NominalDiffTime -> (UTCTime -> IO ()) -> LifeCycleIO ()
clock ndtInterval call = do
    let
        musInterval :: Integer
        musInterval = truncate $ nominalDiffTimeToSeconds ndtInterval * 1000000
    var <- liftIO newEmptyMVar
    thread <-
        liftIO $
        forkIO $
        handle (\MkCancelled -> return ()) $
        forever $ do
            MkSystemTime s ns <- getSystemTime
            let
                musTime :: Integer
                musTime = (toInteger s) * 1000000 + div (toInteger ns) 1000
                musRemaining :: Integer
                musRemaining = musInterval - (mod musTime musInterval)
                musNext :: Integer
                musNext = musTime + musRemaining
                sysTimeNext :: SystemTime
                sysTimeNext =
                    MkSystemTime (fromInteger $ div musNext 1000000) $ fromInteger $ (mod musNext 1000000) * 1000
                utcTimeNext :: UTCTime
                utcTimeNext = systemToUTCTime sysTimeNext
            putMVar var ()
            threadDelay $ fromInteger musRemaining
            takeMVar var
            call utcTimeNext
    lifeCycleClose $ do
        takeMVar var
        throwTo thread MkCancelled
