module Truth.World.Clock where

import Data.Fixed
import Data.IORef
import Data.Time
import Shapes
import Truth.Core

clockObjectMaker :: UTCTime -> NominalDiffTime -> ObjectMaker (ReadOnlyUpdate (WholeUpdate UTCTime)) ()
clockObjectMaker basetime interval update = do
    rec
        ref <- liftIO $ newIORef first
        first <-
            clock basetime interval $ \t -> do
                writeIORef ref t
                update (pure $ MkReadOnlyUpdate $ MkWholeReaderUpdate t) noEditContext
    run <-
        liftIO $
        newResourceRunner $ \rt -> do
            t <- liftIO $ readIORef ref -- read once before opening, to keep value consistent while object is open
            runReaderT rt t
    let
        object :: Object (NoEdit (WholeReader UTCTime))
        object = MkResource run $ immutableAnObject $ \ReadWhole -> ask
    return (object, ())

clockTimeZoneLens :: FloatingEditLens (WholeUpdate UTCTime) (ReadOnlyUpdate (WholeUpdate TimeZone))
clockTimeZoneLens = let
    minuteChanges = liftReadOnlyFloatingEditLens $ changeOnlyUpdateFunction @UTCTime
    tzChanges = liftReadOnlyFloatingEditLens $ changeOnlyUpdateFunction @TimeZone
    wholeMinute :: UTCTime -> UTCTime
    wholeMinute (UTCTime d t) = UTCTime d $ secondsToDiffTime $ (div' t 60) * 60
    in tzChanges .
       (liftReadOnlyFloatingEditLens $ editLensToFloating $ ioFuncEditLens getTimeZone) .
       minuteChanges . editLensToFloating (funcEditLens wholeMinute)
