module Pinafore.Language.Library.Entity.Time
    ( timeEntityLibSection
    )
where

import Changes.World.Clock
import Data.Time
import Data.Time.Clock.POSIX
import Shapes.Numeric

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Entity.Literal
import Pinafore.Language.Library.Entity.Numeric
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Showable
import Pinafore.Language.Type
import Pinafore.Language.Value

zeroTime :: UTCTime
zeroTime = UTCTime (fromGregorian 2000 1 1) 0

newClock :: NominalDiffTime -> Action (ImmutableWholeModel UTCTime)
newClock duration = do
    (clockOM, ()) <- actionLiftLifecycle $ makeSharedModel $ regularClockPremodel zeroTime duration
    return $ functionImmutableModel $ MkWModel $ clockOM

newClockUTC :: Action (ImmutableWholeModel Day)
newClockUTC = do
    (clockOM, ()) <- actionLiftLifecycle $ makeSharedModel utcDayClockPremodel
    return $ functionImmutableModel $ MkWModel $ clockOM

newClockLocal :: Action (ImmutableWholeModel Day)
newClockLocal = do
    (clockOM, ()) <- actionLiftLifecycle $ makeSharedModel localDayClockPremodel
    return $ functionImmutableModel $ MkWModel $ clockOM

newTimeZoneModel :: ImmutableWholeModel UTCTime -> Action (ImmutableWholeModel Int)
newTimeZoneModel now = do
    model <-
        actionFloatMapReadOnly
            (floatLift (\mr ReadWhole -> fmap (fromKnow zeroTime) $ mr ReadWhole) liftROWChangeLens clockTimeZoneLens)
            $ immutableModelToReadOnlyModel now
    return $ fmap timeZoneMinutes $ MkImmutableWholeModel model

getLocalTime :: IO LocalTime
getLocalTime = fmap zonedTimeToLocalTime getZonedTime

unixFormat ::
    forall t.
    FormatTime t =>
    Text ->
    t ->
    Text
unixFormat fmt t = pack $ formatTime defaultTimeLocale (unpack fmt) t

unixParse ::
    forall t.
    ParseTime t =>
    Text ->
    Text ->
    Maybe t
unixParse fmt text = parseTimeM True defaultTimeLocale (unpack fmt) (unpack text)

unixAsText ::
    forall a.
    (FormatTime a, ParseTime a) =>
    Text ->
    LangPrism '(Text, Text) '(a, a)
unixAsText fmt = prism (unixParse fmt) (unixFormat fmt)

unixFormattingDef ::
    forall t.
    (HasQType QPolyShim 'Positive t, HasQType QPolyShim 'Negative t, FormatTime t, ParseTime t) =>
    Text ->
    LibraryStuff
unixFormattingDef lname =
    valBDS
        (UnqualifiedFullNameRef $ MkName $ "unixAsText")
        ( "Represent "
            <> plainText lname
            <> " as text, using the given [Unix-like format/parsing](https://hackage.haskell.org/package/time-1.12.2/docs/Data-Time-Format.html) string."
        )
        $ unixAsText @t

pattern TimeOfDayNat :: Natural -> Natural -> Pico -> TimeOfDay
pattern TimeOfDayNat h m s <-
    TimeOfDay (toNaturalForce -> h) (toNaturalForce -> m) s
    where
        TimeOfDayNat h m s =
            TimeOfDay (fromIntegral h) (fromIntegral m) s

{-# COMPLETE TimeOfDayNat #-}

pattern YearMonthDayNat :: Integer -> Natural -> Natural -> Day
pattern YearMonthDayNat y m d <-
    YearMonthDay y (toNaturalForce -> m) (toNaturalForce -> d)
    where
        YearMonthDayNat y m d =
            YearMonthDay y (fromIntegral m) (fromIntegral d)

{-# COMPLETE YearMonthDayNat #-}

timeEntityLibSection :: LibraryStuff
timeEntityLibSection =
    headingBDS
        "Date & Time"
        ""
        [ headingBDS "Duration" ""
            $ [ typeBDS
                    "Duration"
                    ""
                    (MkSomeGroundType durationGroundType)
                    [ addNameInRootBDS
                        $ valPatBDS "Seconds" "Construct a `Duration` from seconds." secondsToNominalDiffTime
                        $ PureFunction
                        $ pure
                        $ \d -> (nominalDiffTimeToSeconds d, ())
                    ]
              , literalSubtypeRelationEntry @NominalDiffTime
              , showableSubtypeRelationEntry @NominalDiffTime "" showT
              , namespaceBDS "Duration"
                    $ ordEntries @NominalDiffTime
                    <> [ valBDS "zero" "No duration." $ (0 :: NominalDiffTime)
                       , valBDS "day" "One day duration." nominalDay
                       , valBDS "+" "Add durations." $ (+) @NominalDiffTime
                       , valBDS "-" "Subtract durations." $ (-) @NominalDiffTime
                       , valBDS "negate" "Negate duration." $ negate @NominalDiffTime
                       , valBDS "*" "Multiply a duration by a number." $ \(n :: Number) (d :: NominalDiffTime) ->
                            (realToFrac n) * d
                       , valBDS "/" "Divide durations." $ \(a :: NominalDiffTime) (b :: NominalDiffTime) ->
                            (realToFrac (a / b) :: Number)
                       ]
              ]
        , headingBDS "Time" ""
            $ [ typeBDS
                    "Time"
                    "Absolute time as measured by UTC."
                    (MkSomeGroundType timeGroundType)
                    [ addNameInRootBDS
                        $ valPatBDS
                            "UTCDateAndSinceMidnight"
                            "Construct a `Time` from a `Date` and a `Duration` since UTC midnight."
                            UTCTime
                        $ PureFunction
                        $ pure
                        $ \(UTCTime d t) -> (d, (t, ()))
                    , addNameInRootBDS
                        $ valPatBDS "UTC" "Construct a `Time` from a `LocalTime` in UTC." (localTimeToUTC utc)
                        $ PureFunction
                        $ pure
                        $ \d -> (utcToLocalTime utc d, ())
                    , addNameInRootBDS
                        $ valPatBDS
                            "SinceUnixEpoch"
                            "Construct a `Time` from a `Duration` since the Unix epoch (beginning of 1970 UTC)."
                            posixSecondsToUTCTime
                        $ PureFunction
                        $ pure
                        $ \d -> (utcTimeToPOSIXSeconds d, ())
                    ]
              , literalSubtypeRelationEntry @UTCTime
              , showableSubtypeRelationEntry @UTCTime "" showT
              , namespaceBDS "Time"
                    $ ordEntries @UTCTime
                    <> [ plainFormattingDef @UTCTime "a time"
                       , unixFormattingDef @UTCTime "a time"
                       , valBDS "+" "Add duration to time." addUTCTime
                       , valBDS "-" "Difference of times." diffUTCTime
                       , valBDS "getNow" "Get the current time." $ getCurrentTime
                       , addNameInRootBDS
                            $ valBDS "newClock" "Make a model of the current time that updates per the given duration." newClock
                       ]
              ]
        , headingBDS "Date" ""
            $ [ typeBDS
                    "Date"
                    ""
                    (MkSomeGroundType dateGroundType)
                    [ addNameInRootBDS
                        $ valPatBDS "YearMonthDay" "Construct a `Date` from year, month, day." YearMonthDayNat
                        $ PureFunction
                        $ pure
                        $ \(YearMonthDayNat y m d) -> (y, (m, (d, ())))
                    , addNameInRootBDS
                        $ valPatBDS "ModifiedJulianDay" "Construct a `Date` from its MJD." ModifiedJulianDay
                        $ PureFunction
                        $ pure
                        $ \day -> (toModifiedJulianDay day, ())
                    ]
              , literalSubtypeRelationEntry @Day
              , showableSubtypeRelationEntry @Day "" showT
              , namespaceBDS "Date"
                    $ enumEntries @Day
                    <> ordEntries @Day
                    <> [ plainFormattingDef @Day "a date"
                       , unixFormattingDef @Day "a date"
                       , valBDS "+" "Add count to days to date." addDays
                       , valBDS "-" "Difference of days between dates." diffDays
                       , valBDS "getNowUTC" "Get the current UTC date." $ fmap utctDay getCurrentTime
                       , valBDS "getNowLocal" "Get the current local date." $ fmap localDay getLocalTime
                       , valBDS "newClockUTC" "Get a whole model for the current UTC date." newClockUTC
                       , valBDS "newClockLocal" "Get a whole model for the current local date." newClockLocal
                       ]
              ]
        , headingBDS "Time of Day" ""
            $ [ typeBDS
                    "TimeOfDay"
                    ""
                    (MkSomeGroundType timeOfDayGroundType)
                    [ addNameInRootBDS
                        $ valPatBDS "HourMinuteSecond" "Construct a `TimeOfDay` from hour, minute, second." TimeOfDayNat
                        $ PureFunction
                        $ pure
                        $ \(TimeOfDayNat h m s) -> (h, (m, (s, ())))
                    , addNameInRootBDS
                        $ valPatBDS
                            "SinceMidnight"
                            "Construct a `TimeOfDay` from duration since midnight (wrapping whole days)."
                            (snd . timeToDaysAndTimeOfDay)
                            (PureFunction $ pure $ \t -> (daysAndTimeOfDayToTime 0 t, ()))
                    ]
              , literalSubtypeRelationEntry @TimeOfDay
              , showableSubtypeRelationEntry @TimeOfDay "" showT
              , namespaceBDS "TimeOfDay"
                    $ ordEntries @TimeOfDay
                    <> [ plainFormattingDef @TimeOfDay "a time of day"
                       , unixFormattingDef @TimeOfDay "a time of day"
                       , addNameInRootBDS $ valBDS "midnight" "Midnight." midnight
                       , addNameInRootBDS $ valBDS "midday" "Midday." midday
                       ]
              ]
        , headingBDS "Local Time" ""
            $ [ typeBDS
                    "LocalTime"
                    ""
                    (MkSomeGroundType localTimeGroundType)
                    [ addNameInRootBDS
                        $ valPatBDS "DateAndTime" "Construct a `LocalTime` from day and time of day." LocalTime
                        $ PureFunction
                        $ pure
                        $ \LocalTime{..} -> (localDay, (localTimeOfDay, ()))
                    ]
              , literalSubtypeRelationEntry @LocalTime
              , showableSubtypeRelationEntry @LocalTime "" showT
              , namespaceBDS "LocalTime"
                    $ ordEntries @LocalTime
                    <> [ plainFormattingDef @LocalTime "a local time"
                       , unixFormattingDef @LocalTime "a local time"
                       , valBDS "fromTime" "Convert a time to local time, given a time zone offset in minutes" $ \i ->
                            utcToLocalTime $ minutesToTimeZone i
                       , valBDS "toTime" "Convert a local time to time, given a time zone offset in minutes" $ \i ->
                            localTimeToUTC $ minutesToTimeZone i
                       , valBDS "getTimeZone" "Get the offset for a time in the current time zone." $ \t ->
                            fmap timeZoneMinutes $ getTimeZone t
                       , valBDS "getCurrentTimeZone" "Get the current time zone offset in minutes."
                            $ fmap timeZoneMinutes getCurrentTimeZone
                       , valBDS "getNow" "Get the current local time." getLocalTime
                       , valBDS "newTimeZoneModel" "The current time zone offset in minutes." newTimeZoneModel
                       ]
              ]
        ]
