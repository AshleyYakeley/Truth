{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Base
    ( baseLibSections
    , showableSubtypeRelationEntry
    , literalSubtypeRelationEntry
    ) where

import Changes.Core
import Changes.World.Clock
import qualified Data.Text
import Data.Time
import Data.Time.Clock.POSIX
import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.Convert.Types
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Optics ()
import Pinafore.Language.Library.Types
import Pinafore.Language.Name
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var
import Pinafore.Text
import Shapes
import Shapes.Numeric
import qualified Text.Collate

showableSubtypeRelationEntry ::
       forall a context. (HasQType 'Negative a, TextShow a)
    => BindDocStuff context
showableSubtypeRelationEntry = hasSubtypeRelationBDS @a @Showable Verify "" $ functionToShim "textShowable" textShowable

literalSubtypeRelationEntry ::
       forall a context. (HasQType 'Negative a, AsLiteral a)
    => BindDocStuff context
literalSubtypeRelationEntry = hasSubtypeRelationBDS @a @Literal Verify "" $ functionToShim "toLiteral" toLiteral

entityAnchor :: Entity -> Text
entityAnchor p = pack $ show p

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
            (floatLift (\mr ReadWhole -> fmap (fromKnow zeroTime) $ mr ReadWhole) liftROWChangeLens clockTimeZoneLens) $
        immutableModelToReadOnlyModel now
    return $ fmap timeZoneMinutes $ MkImmutableWholeModel model

asTextPrism ::
       forall a. (Read a, TextShow a)
    => LangPrism '( Text, Text) '( a, a)
asTextPrism = prism (readMaybe . unpack) textShow

plainFormattingDef ::
       forall t context. (HasQType 'Positive t, HasQType 'Negative t, Read t, TextShow t)
    => Text
    -> BindDocStuff context
plainFormattingDef lname = valBDS "asText" ("Represent " <> plainText lname <> " as text.") $ asTextPrism @t

unixFormat ::
       forall t. FormatTime t
    => Text
    -> t
    -> Text
unixFormat fmt t = pack $ formatTime defaultTimeLocale (unpack fmt) t

unixParse ::
       forall t. ParseTime t
    => Text
    -> Text
    -> Maybe t
unixParse fmt text = parseTimeM True defaultTimeLocale (unpack fmt) (unpack text)

unixAsText ::
       forall a. (FormatTime a, ParseTime a)
    => Text
    -> LangPrism '( Text, Text) '( a, a)
unixAsText fmt = prism (unixParse fmt) (unixFormat fmt)

unixFormattingDef ::
       forall t context. (HasQType 'Positive t, HasQType 'Negative t, FormatTime t, ParseTime t)
    => Text
    -> BindDocStuff context
unixFormattingDef lname =
    valBDS
        (UnqualifiedFullNameRef $ MkName $ "unixAsText")
        ("Represent " <>
         plainText lname <>
         " as text, using the given [Unix-like format/parsing](https://hackage.haskell.org/package/time-1.12.2/docs/Data-Time-Format.html) string.") $
    unixAsText @t

getLocalTime :: IO LocalTime
getLocalTime = fmap zonedTimeToLocalTime getZonedTime

revap :: A -> (A -> B) -> B
revap x f = f x

append :: NonEmpty A -> [A] -> NonEmpty A
append (a :| aa) bb = a :| (aa <> bb)

mconcat1 :: NonEmpty (NonEmpty A) -> NonEmpty A
mconcat1 (na :| lna) = append na $ mconcat $ fmap toList lna

orderOn :: (B -> A) -> (A -> A -> Ordering) -> B -> B -> Ordering
orderOn ba order b1 b2 = order (ba b1) (ba b2)

baseLibSections :: [BindDocStuff context]
baseLibSections =
    [ headingBDS "Literals & Entities" "" $
      [ typeBDS "Entity" "" (MkSomeGroundType entityGroundType) []
      , namespaceBDS "Entity" $
        fmap addNameInRootBDS (eqEntries @_ @Entity) <>
        [ valBDS "order" "An arbitrary order on `Entity`." $ compare @Entity
        , valBDS "anchor" "The anchor of an entity, as text." entityAnchor
        ]
      , typeBDS "Literal" "" (MkSomeGroundType literalGroundType) []
      , hasSubtypeRelationBDS @Literal @Entity Verify "" $ functionToShim "literalToEntity" literalToEntity
      , headingBDS
            "Showable"
            ""
            [ typeBDS
                  "Showable"
                  "Something that can be represented as `Text`."
                  (MkSomeGroundType showableGroundType)
                  [valPatBDS "Mk" "" MkShowable $ PureFunction $ \(MkShowable t) -> (t, ())]
            , namespaceBDS "Showable" [addNameInRootBDS $ valBDS "show" "Show something as `Text`" $ textShow @Showable]
            ]
      , headingBDS
            "Unit"
            ""
            [ typeBDS "Unit" "" (MkSomeGroundType unitGroundType) []
            , literalSubtypeRelationEntry @()
            , showableSubtypeRelationEntry @()
            , namespaceBDS "Unit" $ monoidEntries @_ @() <> eqEntries @_ @()
            ]
      , headingBDS
            "Boolean"
            ""
            [ typeBDS "Boolean" "" (MkSomeGroundType booleanGroundType) $
              fmap
                  addNameInRootBDS
                  [ valPatBDS "True" "Boolean TRUE." True $
                    ImpureFunction $ \v ->
                        if v
                            then Just ()
                            else Nothing
                  , valPatBDS "False" "Boolean FALSE." False $
                    ImpureFunction $ \v ->
                        if v
                            then Nothing
                            else Just ()
                  ]
            , literalSubtypeRelationEntry @Bool
            , showableSubtypeRelationEntry @Bool
            , namespaceBDS "Boolean" $
              eqEntries @_ @Bool <>
              [ addNameInRootBDS $ valBDS "&&" "Boolean AND." (&&)
              , addNameInRootBDS $ valBDS "||" "Boolean OR." (||)
              , addNameInRootBDS $ valBDS "not" "Boolean NOT." not
              ]
            ]
      , headingBDS
            "Ordering"
            ""
            [ typeBDS "Ordering" "" (MkSomeGroundType orderingGroundType) $
              fmap
                  addNameInRootBDS
                  [ valPatBDS "LT" "Less than." LT $
                    ImpureFunction $ \v ->
                        case v of
                            LT -> Just ()
                            _ -> Nothing
                  , valPatBDS "EQ" "Equal to." EQ $
                    ImpureFunction $ \v ->
                        case v of
                            EQ -> Just ()
                            _ -> Nothing
                  , valPatBDS "GT" "Greater than." GT $
                    ImpureFunction $ \v ->
                        case v of
                            GT -> Just ()
                            _ -> Nothing
                  ]
            , literalSubtypeRelationEntry @Ordering
            , showableSubtypeRelationEntry @Ordering
            , namespaceBDS "Ordering" $
              ordEntries @_ @Ordering <>
              monoidEntries @_ @Ordering <>
              [ addNameInRootBDS $ valBDS "eq" "Equal." $ (==) EQ
              , addNameInRootBDS $ valBDS "ne" "Not equal." $ (/=) EQ
              , addNameInRootBDS $ valBDS "lt" "Less than." $ (==) LT
              , addNameInRootBDS $ valBDS "le" "Less than or equal to." $ (/=) GT
              , addNameInRootBDS $ valBDS "gt" "Greater than." $ (==) GT
              , addNameInRootBDS $ valBDS "ge" "Greater than or equal to." $ (/=) LT
              ]
            ]
      , headingBDS
            "Order"
            ""
            [ namespaceBDS "Order" $
              monoidEntries @_ @(A -> A -> Ordering) <>
              [ valBDS "reverse" "Reverse an order." $ reverseOrder @A
              , addNameInRootBDS $ valBDS "lesser" "The lesser of two items in this order." $ lesser @A
              , addNameInRootBDS $ valBDS "greater" "The greater of two items in this order." $ greater @A
              , addNameInRootBDS $ valBDS "on" "Map an order by a function" orderOn
              ]
            ]
      , headingBDS
            "Text"
            ""
            [ typeBDS "Text" "" (MkSomeGroundType textGroundType) []
            , literalSubtypeRelationEntry @Text
            , showableSubtypeRelationEntry @Text
            , namespaceBDS "Text" $
              monoidEntries @_ @Text <>
              orderEntries
                  (Text.Collate.collate Text.Collate.rootCollator)
                  "Order alphabetical first, then lower case before upper, per Unicode normalisation." <>
              [ valBDS "length" "The length of a text." $ olength @Text
              , valBDS
                    "section"
                    "`section start len text` is the section of `text` beginning at `start` of length `len`." $ \start len (text :: Text) ->
                    take len $ drop start text
              , valBDS "toUpperCase" "" Data.Text.toUpper
              , valBDS "toLowerCase" "" Data.Text.toLower
              ]
            ]
      , let
            arithList :: (Num a, Ord a) => a -> a -> Maybe a -> [a]
            arithList step a mb = let
                cond =
                    case mb of
                        Nothing -> \_ -> True
                        Just b ->
                            case compare step 0 of
                                GT -> \x -> x <= b
                                LT -> \x -> x >= b
                                EQ -> \_ -> True
                in takeWhile cond $ iterate (+ step) a
            range :: (Num a, Ord a) => a -> a -> [a]
            range a b = arithList 1 a $ Just b
            in headingBDS
                   "Numeric"
                   ""
                   [ headingBDS "Integer" "" $
                     [ typeBDS "Integer" "" (MkSomeGroundType integerGroundType) []
                     , hasSubtypeRelationBDS @Integer @SafeRational Verify "" $
                       functionToShim "integerSafeRational" $ encode integerSafeRational
                     , namespaceBDS "Integer" $
                       pickNamesInRootBDS ["min", "max"] (ordEntries @_ @Integer) <>
                       pickNamesInRootBDS ["succ", "pred"] (enumEntries @_ @Integer) <>
                       [ plainFormattingDef @Integer "an integer"
                       , addNameInRootBDS $ valBDS "+" "Add." $ (+) @Integer
                       , addNameInRootBDS $ valBDS "-" "Subtract." $ (-) @Integer
                       , addNameInRootBDS $ valBDS "*" "Multiply." $ (*) @Integer
                       , addNameInRootBDS $ valBDS "negate" "Negate." $ negate @Integer
                       , addNameInRootBDS $ valBDS "abs" "Absolute value." $ abs @Integer
                       , addNameInRootBDS $ valBDS "signum" "Sign." $ signum @Integer
                       , addNameInRootBDS $ valBDS "mod" "Modulus, leftover from `div`" $ mod' @Integer
                       , addNameInRootBDS $ valBDS "even" "Is even?" $ even @Integer
                       , addNameInRootBDS $ valBDS "odd" "Is odd?" $ odd @Integer
                       , addNameInRootBDS $ valBDS "gcd" "Greatest common divisor." $ gcd @Integer
                       , addNameInRootBDS $ valBDS "lcm" "Least common multiple." $ lcm @Integer
                       , addNameInRootBDS $ valBDS "^" "Raise to non-negative power." $ (^) @Integer @Integer
                       , addNameInRootBDS $ valBDS "sum" "Sum." $ sum @[] @Integer
                       , addNameInRootBDS $ valBDS "product" "Product." $ product @[] @Integer
                       , addNameInRootBDS $
                         valBDS
                             "range"
                             "`range a b` is an arithmetic sequence starting from `a`, with all numbers `<= b`. Step is +1." $
                         range @Integer
                       , addNameInRootBDS $
                         valBDS
                             "arithList"
                             "`arithList step a (Just b)` is an arithmetic sequence starting from `a`, with all numbers `<= b` (for positive step) or `>= b` (for negative step).\n\n\
                                \`arithList step a Nothing` is an infinite arithmetic sequence starting from `a`." $
                         arithList @Integer
                       ]
                     ]
                   , headingBDS "Rational" "" $
                     [ typeBDS "Rational" "" (MkSomeGroundType rationalGroundType) []
                     , hasSubtypeRelationBDS @SafeRational @Number Verify "" $
                       functionToShim "safeRationalNumber" $ encode safeRationalNumber
                     , namespaceBDS
                           "Rational"
                           [ plainFormattingDef @SafeRational "a rational"
                           , valBDS "min" "Lesser of two Rationals" $ min @SafeRational
                           , valBDS "max" "Greater of two Rationals" $ max @SafeRational
                           , valBDS "+" "Add." $ (+) @SafeRational
                           , valBDS "-" "Subtract." $ (-) @SafeRational
                           , valBDS "*" "Multiply." $ (*) @SafeRational
                           , addNameInRootBDS $ valBDS "/" "Divide." $ (/) @SafeRational
                           , valBDS "negate" "Negate." $ negate @SafeRational
                           , valBDS "recip" "Reciprocal." $ recip @SafeRational
                           , valBDS "abs" "Absolute value." $ abs @SafeRational
                           , valBDS "signum" "Sign." $ signum @SafeRational
                           , valBDS "mod" "Modulus, leftover from `div`" $ mod' @SafeRational
                           , valBDS "^" "Raise to Integer power." $ ((^^) :: SafeRational -> Integer -> SafeRational)
                           , valBDS "sum" "Sum." $ sum @[] @SafeRational
                           , valBDS "mean" "Mean." $ \(vv :: [SafeRational]) -> sum vv / toSafeRational (length vv)
                           , valBDS "product" "Product." $ product @[] @SafeRational
                           ]
                     ]
                   , headingBDS "Number" "" $
                     [ typeBDS "Number" "" (MkSomeGroundType numberGroundType) []
                     , literalSubtypeRelationEntry @Number
                     , showableSubtypeRelationEntry @Number
                     , namespaceBDS "Number" $
                       pickNamesInRootBDS ["<", "<=", ">", ">="] (ordEntries @_ @Number) <>
                       [ plainFormattingDef @Number "a number"
                       , valBDS "+" "Add." $ (+) @Number
                       , valBDS "-" "Subtract." $ (-) @Number
                       , valBDS "*" "Multiply." $ (*) @Number
                       , valBDS "/" "Divide." $ (/) @Number
                       , valBDS "negate" "Negate." $ negate @Number
                       , valBDS "recip" "Reciprocal." $ recip @Number
                       , addNameInRootBDS $ valBDS "pi" "Half the radians in a circle." $ pi @Number
                       , addNameInRootBDS $ valBDS "exp" "Exponent" $ exp @Number
                       , addNameInRootBDS $ valBDS "log" "Natural logarithm" $ log @Number
                       , addNameInRootBDS $ valBDS "sqrt" "Square root." $ sqrt @Number
                       , valBDS "^" "Raise to power." $ (**) @Number
                       , addNameInRootBDS $ valBDS "logBase" "" $ logBase @Number
                       , addNameInRootBDS $ valBDS "sin" "Sine of an angle in radians." $ sin @Number
                       , addNameInRootBDS $ valBDS "cos" "Cosine of an angle in radians." $ cos @Number
                       , addNameInRootBDS $ valBDS "tan" "Tangent of an angle in radians." $ tan @Number
                       , addNameInRootBDS $ valBDS "asin" "Radian angle of a sine." $ asin @Number
                       , addNameInRootBDS $ valBDS "acos" "Radian angle of a cosine." $ acos @Number
                       , addNameInRootBDS $ valBDS "atan" "Radian angle of a tangent." $ atan @Number
                       , addNameInRootBDS $ valBDS "sinh" "Hyperbolic sine." $ sinh @Number
                       , addNameInRootBDS $ valBDS "cosh" "Hyperbolic cosine." $ cosh @Number
                       , addNameInRootBDS $ valBDS "tanh" "Hyperbolic tangent." $ tanh @Number
                       , addNameInRootBDS $ valBDS "asinh" "Inverse hyperbolic sine." $ asinh @Number
                       , addNameInRootBDS $ valBDS "acosh" "Inverse hyperbolic cosine." $ acosh @Number
                       , addNameInRootBDS $ valBDS "atanh" "Inverse hyperbolic tangent." $ atanh @Number
                       , valBDS "abs" "Absolute value." $ abs @Number
                       , valBDS "signum" "Sign. Note this will be the same exact or inexact as the number." $
                         signum @Number
                       , addNameInRootBDS $
                         valBDS "floor" "Integer towards negative infinity." (floor :: Number -> Integer)
                       , addNameInRootBDS $
                         valBDS "ceiling" "Integer towards positive infinity." (ceiling :: Number -> Integer)
                       , addNameInRootBDS $ valBDS "round" "Closest Integer." (round :: Number -> Integer)
                       , addNameInRootBDS $ valBDS "inexact" "Convert a number to inexact." numberToDouble
                       , addNameInRootBDS $
                         valBDS
                             "approximate"
                             "`approximate d x` gives the exact number that's a multiple of `d` that's closest to `x`."
                             approximate
                       , addNameInRootBDS $
                         valBDS
                             "div"
                             "Division to Integer, towards negative infinity."
                             (div' :: Number -> Number -> Integer)
                       , addNameInRootBDS $ valBDS "mod" "Modulus, leftover from `div`" $ mod' @Number
                       , addNameInRootBDS $ valBDS "isNaN" "Is not a number?" numberIsNaN
                       , addNameInRootBDS $ valBDS "isInfinite" "Is infinite?" numberIsInfinite
                       , addNameInRootBDS $ valBDS "isNegativeZero" "Is negative zero?" numberIsNegativeZero
                       , addNameInRootBDS $ valBDS "isExact" "Is exact?" numberIsExact
                       , valBDS "sum" "Sum." $ sum @[] @Number
                       , valBDS "mean" "Mean." $ \(vv :: [Number]) -> sum vv / (ExactNumber $ toRational $ length vv)
                       , valBDS "product" "Product." $ product @[] @Number
                       , addNameInRootBDS $
                         valBDS "checkSafeRational" "Get the exact value of a Number, if it is one." $
                         decode safeRationalNumber
                       , addNameInRootBDS $
                         valBDS
                             "checkExactInteger"
                             "Get the exact Integer value of a Number, if it is one. Works as expected on Rationals." $
                         decode $ integerSafeRational . safeRationalNumber
                       ]
                     ]
                   ]
      , headingBDS
            "Date & Time"
            ""
            [ headingBDS "Duration" "" $
              [ typeBDS
                    "Duration"
                    ""
                    (MkSomeGroundType durationGroundType)
                    [ addNameInRootBDS $
                      valPatBDS "Seconds" "Construct a `Duration` from seconds." secondsToNominalDiffTime $
                      PureFunction $ \d -> (nominalDiffTimeToSeconds d, ())
                    ]
              , literalSubtypeRelationEntry @NominalDiffTime
              , showableSubtypeRelationEntry @NominalDiffTime
              , namespaceBDS "Duration" $
                ordEntries @_ @NominalDiffTime <>
                [ valBDS "zero" "No duration." $ (0 :: NominalDiffTime)
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
            , headingBDS "Time" "" $
              [ typeBDS
                    "Time"
                    "Absolute time as measured by UTC."
                    (MkSomeGroundType timeGroundType)
                    [ addNameInRootBDS $
                      valPatBDS
                          "UTCDateAndSinceMidnight"
                          "Construct a `Time` from a `Date` and a `Duration` since UTC midnight."
                          UTCTime $
                      PureFunction $ \(UTCTime d t) -> (d, (t, ()))
                    , addNameInRootBDS $
                      valPatBDS "UTC" "Construct a `Time` from a `LocalTime` in UTC." (localTimeToUTC utc) $
                      PureFunction $ \d -> (utcToLocalTime utc d, ())
                    , addNameInRootBDS $
                      valPatBDS
                          "SinceUnixEpoch"
                          "Construct a `Time` from a `Duration` since the Unix epoch (beginning of 1970 UTC)."
                          posixSecondsToUTCTime $
                      PureFunction $ \d -> (utcTimeToPOSIXSeconds d, ())
                    ]
              , literalSubtypeRelationEntry @UTCTime
              , showableSubtypeRelationEntry @UTCTime
              , namespaceBDS "Time" $
                ordEntries @_ @UTCTime <>
                [ plainFormattingDef @UTCTime "a time"
                , unixFormattingDef @UTCTime "a time"
                , valBDS "+" "Add duration to time." addUTCTime
                , valBDS "-" "Difference of times." diffUTCTime
                , valBDS "getNow" "Get the current time." $ getCurrentTime
                , addNameInRootBDS $
                  valBDS "newClock" "Make a model of the current time that updates per the given duration." newClock
                ]
              ]
            , headingBDS "Date" "" $
              [ typeBDS
                    "Date"
                    ""
                    (MkSomeGroundType dateGroundType)
                    [ addNameInRootBDS $
                      valPatBDS "YearMonthDay" "Construct a `Date` from year, month, day." fromGregorian $
                      PureFunction $ \day -> let
                          (y, m, d) = toGregorian day
                          in (y, (m, (d, ())))
                    , addNameInRootBDS $
                      valPatBDS "ModifiedJulianDay" "Construct a `Date` from its MJD." ModifiedJulianDay $
                      PureFunction $ \day -> (toModifiedJulianDay day, ())
                    ]
              , literalSubtypeRelationEntry @Day
              , showableSubtypeRelationEntry @Day
              , namespaceBDS "Date" $
                enumEntries @_ @Day <>
                ordEntries @_ @Day <>
                [ plainFormattingDef @Day "a date"
                , unixFormattingDef @Day "a date"
                , valBDS "+" "Add count to days to date." addDays
                , valBDS "-" "Difference of days between dates." diffDays
                , valBDS "getNowUTC" "Get the current UTC date." $ fmap utctDay getCurrentTime
                , valBDS "getNowLocal" "Get the current local date." $ fmap localDay getLocalTime
                , valBDS "newClockUTC" "Get a whole model for the current UTC date." newClockUTC
                , valBDS "newClockLocal" "Get a whole model for the current local date." newClockLocal
                ]
              ]
            , headingBDS "Time of Day" "" $
              [ typeBDS
                    "TimeOfDay"
                    ""
                    (MkSomeGroundType timeOfDayGroundType)
                    [ addNameInRootBDS $
                      valPatBDS "HourMinuteSecond" "Construct a `TimeOfDay` from hour, minute, second." TimeOfDay $
                      PureFunction $ \TimeOfDay {..} -> (todHour, (todMin, (todSec, ())))
                    , addNameInRootBDS $
                      valPatBDS
                          "SinceMidnight"
                          "Construct a `TimeOfDay` from duration since midnight (wrapping whole days)."
                          (snd . timeToDaysAndTimeOfDay)
                          (PureFunction $ \t -> (daysAndTimeOfDayToTime 0 t, ()))
                    ]
              , literalSubtypeRelationEntry @TimeOfDay
              , showableSubtypeRelationEntry @TimeOfDay
              , namespaceBDS "TimeOfDay" $
                ordEntries @_ @TimeOfDay <>
                [ plainFormattingDef @TimeOfDay "a time of day"
                , unixFormattingDef @TimeOfDay "a time of day"
                , addNameInRootBDS $ valBDS "midnight" "Midnight." midnight
                , addNameInRootBDS $ valBDS "midday" "Midday." midday
                ]
              ]
            , headingBDS "Local Time" "" $
              [ typeBDS
                    "LocalTime"
                    ""
                    (MkSomeGroundType localTimeGroundType)
                    [ addNameInRootBDS $
                      valPatBDS "DateAndTime" "Construct a `LocalTime` from day and time of day." LocalTime $
                      PureFunction $ \LocalTime {..} -> (localDay, (localTimeOfDay, ()))
                    ]
              , literalSubtypeRelationEntry @LocalTime
              , showableSubtypeRelationEntry @LocalTime
              , namespaceBDS "LocalTime" $
                ordEntries @_ @LocalTime <>
                [ plainFormattingDef @LocalTime "a local time"
                , unixFormattingDef @LocalTime "a local time"
                , valBDS "fromTime" "Convert a time to local time, given a time zone offset in minutes" $ \i ->
                      utcToLocalTime $ minutesToTimeZone i
                , valBDS "toTime" "Convert a local time to time, given a time zone offset in minutes" $ \i ->
                      localTimeToUTC $ minutesToTimeZone i
                , valBDS "getTimeZone" "Get the offset for a time in the current time zone." $ \t ->
                      fmap timeZoneMinutes $ getTimeZone t
                , valBDS "getCurrentTimeZone" "Get the current time zone offset in minutes." $
                  fmap timeZoneMinutes getCurrentTimeZone
                , valBDS "getNow" "Get the current local time." getLocalTime
                , valBDS "newTimeZoneModel" "The current time zone offset in minutes." newTimeZoneModel
                ]
              ]
            ]
      , headingBDS
            "Open Entity Types"
            ""
            [ namespaceBDS
                  "OpenEntity"
                  [ specialFormBDS
                        "point"
                        "An open entity for this anchor. `A` is an open entity type."
                        ["@A", "<anchor>"]
                        "A" $
                    MkQSpecialForm (ConsListType AnnotPositiveType $ ConsListType AnnotAnchor NilListType) $ \(t, (anchor, ())) -> do
                        mtp <- getOpenEntityType t
                        return $
                            case mtp of
                                MkSome (tp :: OpenEntityType tid) -> let
                                    typef = openEntityShimWit tp
                                    pt :: OpenEntity tid
                                    pt = MkOpenEntity $ MkEntity anchor
                                    in MkSomeOf typef pt
                  , specialFormBDS "new" "Generate an open entity. `A` is an open entity type." ["@A"] "Action A" $
                    MkQSpecialForm (ConsListType AnnotPositiveType NilListType) $ \(t, ()) -> do
                        mtp <- getOpenEntityType t
                        return $
                            case mtp of
                                MkSome (tp :: OpenEntityType tid) -> let
                                    pt :: Action (OpenEntity tid)
                                    pt = liftIO $ newKeyContainerItem @(FiniteSet (OpenEntity tid))
                                    typef = actionShimWit $ openEntityShimWit tp
                                    in MkSomeOf typef pt
                  ]
            ]
      , headingBDS
            "Dynamic Entity Types"
            ""
            [ typeBDS "DynamicEntity" "" (MkSomeGroundType dynamicStorableGroundType) []
            , hasSubtypeRelationBDS @DynamicEntity @Entity Verify "" $
              functionToShim "dynamicStoreAdapter" $ storeAdapterConvert $ dynamicStoreAdapter Nothing
            , namespaceBDS
                  "DynamicEntity"
                  [ specialFormBDS
                        "point"
                        "A dynamic entity for this anchor. `A` is a concrete dynamic entity type."
                        ["@A", "<anchor>"]
                        "A" $
                    MkQSpecialForm (ConsListType AnnotPositiveType $ ConsListType AnnotAnchor NilListType) $ \(t, (anchor, ())) -> do
                        (n, dt) <- getConcreteDynamicEntityType t
                        let
                            typef = concreteDynamicEntityShimWit n dt
                            pt :: DynamicEntity
                            pt = MkDynamicEntity dt $ MkEntity anchor
                        return $ MkSomeOf typef pt
                  , specialFormBDS
                        "new"
                        "Generate a dynamic entity. `A` is a concrete dynamic entity type."
                        ["@A"]
                        "Action A" $
                    MkQSpecialForm (ConsListType AnnotPositiveType NilListType) $ \(t, ()) -> do
                        (n, dt) <- getConcreteDynamicEntityType t
                        let
                            pt :: Action DynamicEntity
                            pt =
                                liftIO $ do
                                    e <- newKeyContainerItem @(FiniteSet Entity)
                                    return $ MkDynamicEntity dt e
                            typef = actionShimWit $ concreteDynamicEntityShimWit n dt
                        return $ MkSomeOf typef pt
                  ]
            ]
      ]
    , headingBDS
          "Maybe"
          ""
          [ typeBDS "Maybe" "" (MkSomeGroundType maybeGroundType) $
            fmap
                addNameInRootBDS
                [ valPatBDS "Just" "Construct a Maybe from a value." (Just @A) $
                  ImpureFunction $ \(v :: Maybe A) ->
                      case v of
                          Just a -> Just (a, ())
                          _ -> Nothing
                , valPatBDS "Nothing" "Construct a Maybe without a value." (Nothing @BottomType) $
                  ImpureFunction $ \(v :: Maybe A) ->
                      case v of
                          Nothing -> Just ()
                          _ -> Nothing
                ]
          , hasSubtypeRelationBDS @(Maybe Entity) @Entity Verify "" $
            functionToShim "maybeEntityConvert" maybeEntityConvert
          , hasSubtypeRelationBDS @(Maybe Showable) @Showable Verify "" $ functionToShim "show" textShowable
          , namespaceBDS "Maybe" $ monadEntries @_ @Maybe
          ]
    , headingBDS
          "Type Product"
          ""
          [ typeBDS "*:" "" (MkSomeGroundType pairGroundType) []
          , hasSubtypeRelationBDS @(Entity, Entity) @Entity Verify "" $
            functionToShim "pairEntityConvert" pairEntityConvert
          , hasSubtypeRelationBDS @(Showable, Showable) @Showable Verify "" $ functionToShim "show" textShowable
          , namespaceBDS
                "Product"
                [ addNameInRootBDS $ valBDS "fst" "Get the first member of a pair." $ fst @A @B
                , addNameInRootBDS $ valBDS "snd" "Get the second member of a pair." $ snd @A @B
                , valBDS "to" "Construct a pair." $ (,) @A @B
                , addNameInRootBDS $ valBDS "both" "Construct a pair." $ \(a :: A) -> (a, a)
                ]
          ]
    , headingBDS
          "Type Sum"
          ""
          [ typeBDS "+:" "" (MkSomeGroundType eitherGroundType) $
            fmap
                addNameInRootBDS
                [ valPatBDS "Left" "Construct an Either from the left." (Left @A @B) $
                  ImpureFunction $ \(v :: Either A B) ->
                      case v of
                          Left a -> Just (a, ())
                          _ -> Nothing
                , valPatBDS "Right" "Construct an Either from the right." (Right @A @B) $
                  ImpureFunction $ \(v :: Either A B) ->
                      case v of
                          Right a -> Just (a, ())
                          _ -> Nothing
                ]
          , hasSubtypeRelationBDS @(Either Entity Entity) @Entity Verify "" $
            functionToShim "eitherEntityConvert" eitherEntityConvert
          , hasSubtypeRelationBDS @(Either Showable Showable) @Showable Verify "" $ functionToShim "show" textShowable
          , namespaceBDS "Sum" $
            monadEntries @_ @(Either P) <>
            [ valBDS "from" "Eliminate a sum" $ either @A @C @B
            , addNameInRootBDS $
              valBDS "either" "Eliminate a sum" $ \(v :: Either A A) ->
                  case v of
                      Left a -> a
                      Right a -> a
            ]
          ]
    , headingBDS
          "List"
          ""
          [ typeBDS
                "List"
                "A list."
                (MkSomeGroundType listGroundType)
                [ addNameInRootBDS $
                  typeBDS "List1" "A list with at least one element." (MkSomeGroundType list1GroundType) []
                , hasSubtypeRelationBDS @(NonEmpty A) @[A] Verify "" $ functionToShim "NonEmpty.toList" toList
                , addNameInRootBDS $
                  valPatBDS "[]" "Empty list" ([] @BottomType) $
                  ImpureFunction $ \(v :: [A]) ->
                      case v of
                          [] -> Just ()
                          _ -> Nothing
                , addNameInRootBDS $
                  valPatBDS "::" "Construct a list" ((:|) @A) $
                  ImpureFunction $ \(v :: [A]) ->
                      case v of
                          a:b -> Just (a, (b, ()))
                          _ -> Nothing
                ]
          , hasSubtypeRelationBDS @[Entity] @Entity Verify "" $ functionToShim "listEntityConvert" listEntityConvert
          , hasSubtypeRelationBDS @[Showable] @Showable Verify "" $ functionToShim "show" textShowable
          , namespaceBDS "List" $
            pickNamesInRootBDS ["<>"] (monoidEntries @_ @[A]) <>
            monadEntries @_ @[] <>
            [ valBDS "from" "Eliminate a list" $ \(fnil :: B) fcons (l :: [A]) ->
                  case l of
                      [] -> fnil
                      (a:aa) -> fcons a aa
            , addNameInRootBDS $ valBDS "length" "Number of items in a list" (length :: [TopType] -> Int)
            , addNameInRootBDS $ valBDS "index" "Get item from list by index." (index :: [A] -> Int -> Maybe A)
            , addNameInRootBDS $ valBDS "filter" "Filter a list." (filter :: (A -> Bool) -> [A] -> [A])
            , addNameInRootBDS $ valBDS "maybeMap" "Map and filter a list." (mapMaybe :: (A -> Maybe B) -> [A] -> [B])
            , addNameInRootBDS $ valBDS "take" "Take the first n elements." (take :: Int -> [A] -> [A])
            , addNameInRootBDS $ valBDS "drop" "Drop the first n elements." (drop :: Int -> [A] -> [A])
            , addNameInRootBDS $
              valBDS "takeWhile" "Take while the condition holds." (takeWhile :: (A -> Bool) -> [A] -> [A])
            , addNameInRootBDS $
              valBDS "dropWhile" "Drop while the condition holds." (dropWhile :: (A -> Bool) -> [A] -> [A])
            , addNameInRootBDS $ valBDS "zip" "Zip two lists." $ zip @A @B
            , addNameInRootBDS $ valBDS "sort" "Sort list by an order." (sortBy :: (A -> A -> Ordering) -> [A] -> [A])
            ]
          , namespaceBDS "List1" $
            applicativeEntries @_ @NonEmpty <>
            [ valBDS "<>" "Concatenate a non-empty list with a list." append
            , addNameInRootBDS $ valBDS "concat1" "Concatenate a non-empty list of non-empty lists." mconcat1
            , valBDS
                  "sort"
                  "Sort non-empty list by an order."
                  (sortBy :: (A -> A -> Ordering) -> NonEmpty A -> NonEmpty A)
            ]
          ]
    , headingBDS
          "Map"
          ""
          [ typeBDS "Map" "A hash map." (MkSomeGroundType mapGroundType) []
          , hasSubtypeRelationBDS @(LangMap Entity) @Entity Verify "" $
            functionToShim "mapEntityConvert" mapEntityConvert
          , namespaceBDS "Map" $
            monoidEntries @_ @(LangMap A) <>
            [ valBDS "lookup" "Look up element." $ langMapLookup @A
            , valBDS "insert" "Insert into map." $ langMapInsert @A
            , valBDS "delete" "Delete from map." $ langMapDelete @A
            , valBDS "single" "A map with one element." $ langMapSingle @A
            , valBDS "keys" "The keys of the map." $ langMapKeys @TopType
            , valBDS "values" "The values of the map." $ langMapValues @A
            , valBDS "fromList" "Construct from list." $ langMapFromList @A
            , valBDS "toList" "Convert to list." $ langMapToList @A
            ]
          ]
    , headingBDS
          "Function"
          ""
          [ typeBDS "->" "A pure function." (MkSomeGroundType funcGroundType) []
          , namespaceBDS "Function" $
            monadEntries @_ @((->) P) <>
            [ addNameInRootBDS $ valBDS "$" "Apply a function to a value." $ id @(->) @(A -> B)
            , addNameInRootBDS $ valBDS ">-" "Apply a value to a function." revap
            , addNameInRootBDS $ valBDS "id" "The identity function." $ id @(->) @A
            , addNameInRootBDS $ valBDS "." "Compose functions." $ (.) @(->) @A @B @C
            , addNameInRootBDS $ valBDS "fix" "Fixed point of a function." $ fix @A
            , addNameInRootBDS $ valBDS "error" "Error." $ ((\t -> error (unpack t)) :: Text -> BottomType)
            , addNameInRootBDS $
              valBDS "undefined" "Same as `error \"undefined\"`." $ ((error "undefined") :: BottomType)
            , addNameInRootBDS $
              valBDS
                  "seq"
                  "Evaluate the first argument, then if that's not \"bottom\" (error or non-termination), return the second argument."
                  (seq :: TopType -> A -> A)
            , addNameInRootBDS $
              specialFormBDS "check" "Check from a dynamic supertype." ["@A"] "D(A) -> Maybe A" $
              MkQSpecialForm (ConsListType AnnotNegativeType NilListType) $ \(MkSome tn, ()) -> do
                  let dtw = getGreatestDynamicSupertype tn
                  tpw <- invertType tn
                  return $ MkSomeOf (funcShimWit dtw $ maybeShimWit tpw) id
            , addNameInRootBDS $
              specialFormBDS "coerce" "Coerce from a dynamic supertype." ["@A"] "D(A) -> A" $
              MkQSpecialForm (ConsListType AnnotNegativeType NilListType) $ \(MkSome tn, ()) -> do
                  let dtw = getGreatestDynamicSupertype tn
                  tpw <- invertType tn
                  return $
                      MkSomeOf (funcShimWit dtw tpw) $ \case
                          Just t -> t
                          Nothing ->
                              error $
                              unpack $ toText $ "coercion from " <> exprShow dtw <> " to " <> exprShow tn <> " failed"
            ]
          ]
    ]
