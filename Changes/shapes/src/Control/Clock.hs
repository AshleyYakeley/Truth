module Control.Clock
    ( ClockMachine(..)
    , listClockMachine
    , regularClockMachine
    , utcDayClockMachine
    , localDayClockMachine
    , clock
    ) where

import Control.Task
import Data.Time.Clock
import Shapes.Import
import Shapes.Numeric

data ClockMachine a =
    forall s. MkClockMachine (IO (a, s))
                             (s -> IO (Maybe (s, (UTCTime, a))))

instance Functor ClockMachine where
    fmap ab (MkClockMachine ias f) =
        MkClockMachine (fmap (\(a, s) -> (ab a, s)) ias) $ fmap (fmap $ fmap $ fmap $ fmap ab) f

listClockMachine :: IO (a, [(UTCTime, a)]) -> ClockMachine a
listClockMachine getInitial =
    MkClockMachine getInitial $ \case
        [] -> return Nothing
        ta:tt -> return $ Just (tt, ta)

regularClockMachine :: UTCTime -> NominalDiffTime -> ClockMachine UTCTime
regularClockMachine utcBase ndtInterval =
    listClockMachine $ do
        utcCurrent <- getCurrentTime
        let
            ndtTime = diffUTCTime utcCurrent utcBase
            ndtPrevious = (realToFrac $ (div' ndtTime ndtInterval :: Int)) * ndtInterval
            utcPrevious = addUTCTime ndtPrevious utcBase
            getTime :: Int -> (UTCTime, UTCTime)
            getTime i = let
                t = addUTCTime (realToFrac i * ndtInterval) utcPrevious
                in (t, t)
        return (snd $ getTime 0, fmap getTime [1 ..])

utcDayClockMachine :: ClockMachine Day
utcDayClockMachine =
    listClockMachine $ do
        utcCurrent <- getCurrentTime
        let
            today = utctDay utcCurrent
            getTime :: Integer -> (UTCTime, Day)
            getTime i = let
                day = addDays i today
                time = UTCTime day 0
                in (time, day)
            times :: [(UTCTime, Day)]
            times = fmap getTime [1 ..]
        return (today, times)

localDayClockMachine :: ClockMachine Day
localDayClockMachine = let
    getInitial :: IO (Day, Day)
    getInitial = do
        ztCurrent <- liftIO getZonedTime
        let today = localDay $ zonedTimeToLocalTime ztCurrent
        return (today, today)
    f :: Day -> IO (Maybe (Day, (UTCTime, Day)))
    f prevDay = do
        let
            day = succ prevDay
            lt = LocalTime day midnight
            time0 = UTCTime day 0
        tz <- getTimeZone time0
        let time = localTimeToUTC tz lt
        return $ Just (day, (time, day))
    in MkClockMachine getInitial f

clock :: forall a. ClockMachine a -> (a -> IO ()) -> Lifecycle a
clock (MkClockMachine (getInitial :: IO (a, s)) f) call = do
    var <- liftIO newEmptyMVar
    let
        run :: s -> IO ()
        run oldstate = do
            msta <- f oldstate
            case msta of
                Nothing -> return ()
                Just (newstate, (t, a)) -> do
                    utcCurrent <- getCurrentTime
                    let ndtRemaining = diffUTCTime t utcCurrent
                    putMVar var () -- must allow var even if not sleeping
                    threadSleep $
                        if ndtRemaining > 0
                            then ndtRemaining
                            else 0
                    takeMVar var
                    call a
                    run newstate
    (initialA, initialState) <- liftIO getInitial
    forkEndlessInLifecycle $ run initialState
    lifecycleOnClose $ takeMVar var
    return initialA
