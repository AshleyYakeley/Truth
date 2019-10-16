module Truth.World.Clock where

import Data.Fixed
import Data.IORef
import Data.Time
import Shapes
import Truth.Core

clockObjectMaker :: UTCTime -> NominalDiffTime -> ObjectMaker (WholeUpdate UTCTime) ()
clockObjectMaker basetime interval update = do
    rec
        ref <- liftIO $ newIORef first
        first <-
            clock basetime interval $ \t -> do
                writeIORef ref t
                update [MkWholeReaderUpdate t] noEditContext
    let
        object :: Object (WholeEdit UTCTime)
        run :: IOFunction (ReaderT UTCTime IO)
        run rt = do
            t <- readIORef ref -- read once before opening, to keep value consistent while object is open
            runReaderT rt t
        object = MkRunnableIO run $ immutableAnObject $ \ReadWhole -> ask
    return (object, ())

makeClockTimeZoneEF :: IO (UpdateFunction (WholeUpdate UTCTime) (WholeUpdate TimeZone))
makeClockTimeZoneEF = do
    minuteChanges <- changeOnlyUpdateFunction @UTCTime
    tzChanges <- changeOnlyUpdateFunction @TimeZone
    let
        wholeMinute :: UTCTime -> UTCTime
        wholeMinute (UTCTime d t) = UTCTime d $ secondsToDiffTime $ (div' t 60) * 60
    return $ tzChanges . ioFuncUpdateFunction getTimeZone . minuteChanges . funcUpdateFunction wholeMinute