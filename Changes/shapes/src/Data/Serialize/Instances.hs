{-# OPTIONS -fno-warn-orphans #-}

module Data.Serialize.Instances
    (
    ) where

import Data.Coercion
import Data.Serializer
import Data.Time
import Shapes.Import
import Shapes.Numeric

instance HasSerializer Void where
    serializer = rVoid

instance HasSerializer () where
    serializer = rUnit

instance HasSerializer (Fixed a) where
    serializer = isoCoerce $ serializer @Integer

instance HasSerializer DiffTime where
    serializer = invmap picosecondsToDiffTime diffTimeToPicoseconds serializer

instance HasSerializer NominalDiffTime where
    serializer = invmap secondsToNominalDiffTime nominalDiffTimeToSeconds serializer

instance HasSerializer Day where
    serializer = isoCoerce $ serializer @Integer

instance HasSerializer TimeOfDay where
    serializer = let
        tupleToTimeOfDay :: (Word8, (Word8, Word64)) -> TimeOfDay
        tupleToTimeOfDay (h, (m, s)) = TimeOfDay (fromIntegral h) (fromIntegral m) (MkFixed $ toInteger s)
        timeOfDayToTuple :: TimeOfDay -> (Word8, (Word8, Word64))
        timeOfDayToTuple (TimeOfDay h m (MkFixed s)) = (fromIntegral h, (fromIntegral m, fromInteger s))
        in invmap tupleToTimeOfDay timeOfDayToTuple $ serializer <***> serializer <***> serializer

instance HasSerializer LocalTime where
    serializer = let
        pairToLocalTime :: (Day, TimeOfDay) -> LocalTime
        pairToLocalTime (d, t) = LocalTime d t
        localTimeToPair :: LocalTime -> (Day, TimeOfDay)
        localTimeToPair (LocalTime d t) = (d, t)
        in invmap pairToLocalTime localTimeToPair $ serializer <***> serializer

instance HasSerializer UTCTime where
    serializer = let
        pairToUTCTime :: (Day, DiffTime) -> UTCTime
        pairToUTCTime (d, t) = UTCTime d t
        utcTimeToPair :: UTCTime -> (Day, DiffTime)
        utcTimeToPair (UTCTime d t) = (d, t)
        in invmap pairToUTCTime utcTimeToPair $ serializer <***> serializer
