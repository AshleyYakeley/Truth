module Changes.World.Clock where

import Changes.Core
import Data.Fixed
import Data.IORef
import Data.Time
import Shapes

clockPremodel :: forall a. ClockMachine a -> Premodel (ROWUpdate a) ()
clockPremodel cm pmrUpdatesTask update = do
    rec
        ref <- liftIO $ newIORef firstVal
        firstVal <-
            clock cm $ \val -> do
                writeIORef ref val
                update emptyResourceContext (pure $ MkReadOnlyUpdate $ MkWholeReaderUpdate val) noEditContext
    run <-
        liftIO $
        newResourceRunner $ \rt -> do
            t <- liftIO $ readIORef ref -- read once before opening, to keep value consistent while reference is open
            runReaderT rt t
    let
        pmrReference :: Reference (ConstEdit (WholeReader a))
        pmrReference = MkResource run $ immutableAReference $ \ReadWhole -> ask
        pmrValue = ()
    return MkPremodelResult {..}

regularClockPremodel :: UTCTime -> NominalDiffTime -> Premodel (ROWUpdate UTCTime) ()
regularClockPremodel utcBase ndtInterval = clockPremodel $ regularClockMachine utcBase ndtInterval

utcDayClockPremodel :: Premodel (ROWUpdate Day) ()
utcDayClockPremodel = clockPremodel utcDayClockMachine

localDayClockPremodel :: Premodel (ROWUpdate Day) ()
localDayClockPremodel = clockPremodel localDayClockMachine

clockTimeZoneLens :: FloatingChangeLens (WholeUpdate UTCTime) (ROWUpdate TimeZone)
clockTimeZoneLens = let
    minuteChanges = liftReadOnlyFloatingChangeLens $ changeOnlyUpdateFunction @UTCTime
    tzChanges = liftReadOnlyFloatingChangeLens $ changeOnlyUpdateFunction @TimeZone
    wholeMinute :: UTCTime -> UTCTime
    wholeMinute (UTCTime d t) = UTCTime d $ secondsToDiffTime $ (div' t 60) * 60
    in tzChanges .
       (liftReadOnlyFloatingChangeLens $ changeLensToFloating $ ioFuncChangeLens getTimeZone) .
       minuteChanges . changeLensToFloating (funcChangeLens wholeMinute)
