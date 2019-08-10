module Truth.World.Clock where

import Data.Fixed
import Data.IORef
import Data.Time
import Truth.Core
import Truth.Core.Import

clockUpdatingObject :: UTCTime -> NominalDiffTime -> UpdatingObject (WholeEdit UTCTime) ()
clockUpdatingObject basetime interval update = do
    rec
        ref <- liftIO $ newIORef first
        first <-
            clock basetime interval $ \t -> do
                writeIORef ref t
                update [MkWholeEdit t] noEditSource
    let
        object :: Object (WholeEdit UTCTime)
        run :: UnliftIO (ReaderT UTCTime IO)
        run =
            MkTransform $ \rt -> do
                t <- readIORef ref -- read once before opening, to keep value consistent while object is open
                runReaderT rt t
        object = MkCloseUnliftIO run $ immutableAnObject $ \ReadWhole -> ask
    return (object, ())

makeClockTimeZoneEF :: IO (EditFunction (WholeEdit UTCTime) (WholeEdit TimeZone))
makeClockTimeZoneEF = do
    minuteChanges <- changeOnlyEditFunction @UTCTime
    tzChanges <- changeOnlyEditFunction @TimeZone
    let
        wholeMinute :: UTCTime -> UTCTime
        wholeMinute (UTCTime d t) = UTCTime d $ secondsToDiffTime $ (div' t 60) * 60
    return $ tzChanges . ioFuncEditFunction getTimeZone . minuteChanges . funcEditFunction wholeMinute
