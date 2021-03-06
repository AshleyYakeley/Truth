module Changes.World.Clock where

import Changes.Core
import Data.Fixed
import Data.IORef
import Data.Time
import Shapes

clockPremodel :: UTCTime -> NominalDiffTime -> Premodel (ROWUpdate UTCTime) ()
clockPremodel basetime interval pmrUpdatesTask update = do
    rec
        ref <- liftIO $ newIORef first
        first <-
            clock basetime interval $ \t -> do
                writeIORef ref t
                update emptyResourceContext (pure $ MkReadOnlyUpdate $ MkWholeReaderUpdate t) noEditContext
    run <-
        liftIO $
        newResourceRunner $ \rt -> do
            t <- liftIO $ readIORef ref -- read once before opening, to keep value consistent while reference is open
            runReaderT rt t
    let
        pmrReference :: Reference (ConstEdit (WholeReader UTCTime))
        pmrReference = MkResource run $ immutableAReference $ \ReadWhole -> ask
        pmrValue = ()
    return MkPremodelResult {..}

clockTimeZoneLens :: FloatingChangeLens (WholeUpdate UTCTime) (ROWUpdate TimeZone)
clockTimeZoneLens = let
    minuteChanges = liftReadOnlyFloatingChangeLens $ changeOnlyUpdateFunction @UTCTime
    tzChanges = liftReadOnlyFloatingChangeLens $ changeOnlyUpdateFunction @TimeZone
    wholeMinute :: UTCTime -> UTCTime
    wholeMinute (UTCTime d t) = UTCTime d $ secondsToDiffTime $ (div' t 60) * 60
    in tzChanges .
       (liftReadOnlyFloatingChangeLens $ changeLensToFloating $ ioFuncChangeLens getTimeZone) .
       minuteChanges . changeLensToFloating (funcChangeLens wholeMinute)
