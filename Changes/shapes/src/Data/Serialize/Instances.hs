{-# OPTIONS -fno-warn-orphans #-}

module Data.Serialize.Instances
    (
    ) where

import Data.Coercion
import Data.IsoVariant
import qualified Data.Serialize as Serialize
import Data.Serializer
import Data.Time
import Shapes.Import
import Shapes.Numeric

instance Serialize (Fixed a) where
    put (MkFixed i) = Serialize.put i
    get = do
        i <- Serialize.get
        return $ MkFixed i

diffTimeSerializer :: Serializer DiffTime
diffTimeSerializer = isoMap picosecondsToDiffTime diffTimeToPicoseconds serializer

instance Serialize DiffTime where
    put = serialize diffTimeSerializer
    get = deserialize diffTimeSerializer

nominalDiffTimeSerializer :: Serializer NominalDiffTime
nominalDiffTimeSerializer = isoMap secondsToNominalDiffTime nominalDiffTimeToSeconds serializer

instance Serialize NominalDiffTime where
    put = serialize nominalDiffTimeSerializer
    get = deserialize nominalDiffTimeSerializer

daySerializer :: Serializer Day
daySerializer = isoCoerce (serializer @Integer)

instance Serialize Day where
    put = serialize daySerializer
    get = deserialize daySerializer

timeOfDaySerializer :: Serializer TimeOfDay
timeOfDaySerializer = let
    tupleToTimeOfDay :: (Word8, (Word8, Word64)) -> TimeOfDay
    tupleToTimeOfDay (h, (m, s)) = TimeOfDay (fromIntegral h) (fromIntegral m) (MkFixed $ toInteger s)
    timeOfDayToTuple :: TimeOfDay -> (Word8, (Word8, Word64))
    timeOfDayToTuple (TimeOfDay h m (MkFixed s)) = (fromIntegral h, (fromIntegral m, fromInteger s))
    in isoMap tupleToTimeOfDay timeOfDayToTuple $ serializer <***> serializer <***> serializer

instance Serialize TimeOfDay where
    put = serialize timeOfDaySerializer
    get = deserialize timeOfDaySerializer

localTimeSerializer :: Serializer LocalTime
localTimeSerializer = let
    pairToLocalTime :: (Day, TimeOfDay) -> LocalTime
    pairToLocalTime (d, t) = LocalTime d t
    localTimeToPair :: LocalTime -> (Day, TimeOfDay)
    localTimeToPair (LocalTime d t) = (d, t)
    in isoMap pairToLocalTime localTimeToPair $ serializer <***> serializer

instance Serialize LocalTime where
    put = serialize localTimeSerializer
    get = deserialize localTimeSerializer

utcTimeSerializer :: Serializer UTCTime
utcTimeSerializer = let
    pairToUTCTime :: (Day, DiffTime) -> UTCTime
    pairToUTCTime (d, t) = UTCTime d t
    utcTimeToPair :: UTCTime -> (Day, DiffTime)
    utcTimeToPair (UTCTime d t) = (d, t)
    in isoMap pairToUTCTime utcTimeToPair $ serializer <***> serializer

instance Serialize UTCTime where
    put = serialize utcTimeSerializer
    get = deserialize utcTimeSerializer
