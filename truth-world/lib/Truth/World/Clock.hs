module Truth.World.Clock where

import Data.Fixed
import Data.IORef
import Data.Time
import Shapes
import Truth.Core

clockObjectMaker :: UTCTime -> NominalDiffTime -> ObjectMaker (ROWUpdate UTCTime) ()
clockObjectMaker basetime interval omrUpdatesTask update = do
    rec
        ref <- liftIO $ newIORef first
        first <-
            clock basetime interval $ \t -> do
                writeIORef ref t
                update emptyResourceContext (pure $ MkReadOnlyUpdate $ MkWholeReaderUpdate t) noEditContext
    run <-
        liftIO $
        newResourceRunner $ \rt -> do
            t <- liftIO $ readIORef ref -- read once before opening, to keep value consistent while object is open
            runReaderT rt t
    let
        omrObject :: Object (ConstEdit (WholeReader UTCTime))
        omrObject = MkResource run $ immutableAnObject $ \ReadWhole -> ask
        omrValue = ()
    return MkObjectMakerResult {..}

clockTimeZoneLens :: FloatingChangeLens (WholeUpdate UTCTime) (ROWUpdate TimeZone)
clockTimeZoneLens = let
    minuteChanges = liftReadOnlyFloatingChangeLens $ changeOnlyUpdateFunction @UTCTime
    tzChanges = liftReadOnlyFloatingChangeLens $ changeOnlyUpdateFunction @TimeZone
    wholeMinute :: UTCTime -> UTCTime
    wholeMinute (UTCTime d t) = UTCTime d $ secondsToDiffTime $ (div' t 60) * 60
    in tzChanges .
       (liftReadOnlyFloatingChangeLens $ changeLensToFloating $ ioFuncChangeLens getTimeZone) .
       minuteChanges . changeLensToFloating (funcChangeLens wholeMinute)
