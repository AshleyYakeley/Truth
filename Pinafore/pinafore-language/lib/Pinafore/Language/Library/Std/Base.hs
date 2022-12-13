{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Std.Base
    ( baseLibEntries
    , showableSubtypeRelationEntry
    , literalSubtypeRelationEntry
    ) where

import Changes.Core
import Changes.World.Clock
import qualified Data.Text
import Data.Time
import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.Convert.Types
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.Library.Std.Types
import Pinafore.Language.Name
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var
import Pinafore.Markdown
import Shapes
import Shapes.Numeric
import qualified Text.Collate

showableSubtypeRelationEntry ::
       forall a context. (HasQType 'Negative a, TextShow a)
    => BindDocTree context
showableSubtypeRelationEntry = hasSubtypeRelationBDT @a @Showable Verify "" $ functionToShim "textShowable" textShowable

literalSubtypeRelationEntry ::
       forall a context. (HasQType 'Negative a, AsLiteral a)
    => BindDocTree context
literalSubtypeRelationEntry = hasSubtypeRelationBDT @a @Literal Verify "" $ functionToShim "toLiteral" toLiteral

entityAnchor :: Entity -> Text
entityAnchor p = pack $ show p

zeroTime :: UTCTime
zeroTime = UTCTime (fromGregorian 2000 1 1) 0

newClock :: NominalDiffTime -> Action (ImmutableWholeModel UTCTime)
newClock duration = do
    (clockOM, ()) <- actionLiftLifecycle $ makeSharedModel $ clockPremodel zeroTime duration
    return $ functionImmutableModel $ MkWModel $ clockOM

newTimeZoneModel :: ImmutableWholeModel UTCTime -> Action (ImmutableWholeModel Int)
newTimeZoneModel now = do
    model <-
        actionFloatMapReadOnly
            (floatLift (\mr ReadWhole -> fmap (fromKnow zeroTime) $ mr ReadWhole) liftROWChangeLens clockTimeZoneLens) $
        immutableModelToReadOnlyModel now
    return $ fmap timeZoneMinutes $ MkImmutableWholeModel model

interpretAsText ::
       forall a. (Read a, TextShow a)
    => LangWholeModel '( a, a)
    -> LangWholeModel '( Text, Text)
interpretAsText = let
    getter :: Maybe a -> Maybe Text
    getter Nothing = Just ""
    getter (Just a) = Just $ textShow a
    setter :: Maybe Text -> Maybe a -> Maybe (Maybe a)
    setter Nothing _ = Just Nothing
    setter (Just "") _ = Just Nothing
    setter (Just t) _ = fmap Just $ textReadMaybe t
    in maybeLensLangWholeModel getter setter

textReadMaybe :: Read t => Text -> Maybe t
textReadMaybe t = readMaybe $ unpack t

plainFormattingDefs ::
       forall t context. (HasQType 'Positive t, HasQType 'Negative t, Read t, TextShow t)
    => Text
    -> Text
    -> [BindDocTree context]
plainFormattingDefs uname lname =
    [ valBDT
          (UnqualifiedFullNameRef $ MkName $ "parse" <> uname)
          ("Parse text as " <> plainText lname <> ". Inverse of `show`.") $
      textReadMaybe @t
    , valBDT
          (UnqualifiedFullNameRef $ MkName $ "interpret" <> uname <> "AsText")
          ("Interpret " <> plainText lname <> " model as text, interpreting deleted values as empty text.") $
      interpretAsText @t
    ]

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

unixInterpretAsText ::
       forall a. (FormatTime a, ParseTime a)
    => Text
    -> LangWholeModel '( a, a)
    -> LangWholeModel '( Text, Text)
unixInterpretAsText fmt = let
    getter :: Maybe a -> Maybe Text
    getter Nothing = Just ""
    getter (Just a) = Just $ unixFormat fmt a
    setter :: Maybe Text -> Maybe a -> Maybe (Maybe a)
    setter Nothing _ = Just Nothing
    setter (Just "") _ = Just Nothing
    setter (Just t) _ = fmap Just $ unixParse fmt t
    in maybeLensLangWholeModel getter setter

unixFormattingDefs ::
       forall t context. (HasQType 'Positive t, HasQType 'Negative t, FormatTime t, ParseTime t)
    => Text
    -> Text
    -> [BindDocTree context]
unixFormattingDefs uname lname =
    [ valBDT
          (UnqualifiedFullNameRef $ MkName $ "unixFormat" <> uname)
          ("Format " <> plainText lname <> " as text, using a UNIX-style formatting string.") $
      unixFormat @t
    , valBDT
          (UnqualifiedFullNameRef $ MkName $ "unixParse" <> uname)
          ("Parse text as " <> plainText lname <> ", using a UNIX-style formatting string.") $
      unixParse @t
    , valBDT
          (UnqualifiedFullNameRef $ MkName $ "unixInterpret" <> uname <> "AsText")
          ("Interpret " <> plainText lname <> " model as text, interpreting deleted values as empty text.") $
      unixInterpretAsText @t
    ]

getLocalTime :: IO LocalTime
getLocalTime = fmap zonedTimeToLocalTime getZonedTime

lesser :: (A -> A -> Ordering) -> A -> A -> A
lesser f a b =
    case f a b of
        GT -> b
        _ -> a

greater :: (A -> A -> Ordering) -> A -> A -> A
greater f a b =
    case f a b of
        GT -> a
        _ -> b

revap :: A -> (A -> B) -> B
revap x f = f x

baseLibEntries :: [BindDocTree context]
baseLibEntries =
    [ headingBDT
          "Literals & Entities"
          ""
          [ typeBDT "Entity" "" (MkSomeGroundType entityGroundType) []
          , valBDT "==" "Entity equality." $ (==) @Entity
          , valBDT "/=" "Entity non-equality." $ (/=) @Entity
          , valBDT "entityAnchor" "The anchor of an entity, as text." entityAnchor
          , typeBDT "Literal" "" (MkSomeGroundType literalGroundType) []
          , hasSubtypeRelationBDT @Literal @Entity Verify "" $ functionToShim "literalToEntity" literalToEntity
          , headingBDT
                "Showable"
                ""
                [ typeBDT
                      "Showable"
                      "Something that can be represented as `Text`."
                      (MkSomeGroundType showableGroundType)
                      []
                , valBDT "show" "Show something as `Text`" $ textShow @Showable
                ]
          , headingBDT
                "Unit"
                ""
                [ typeBDT "Unit" "" (MkSomeGroundType unitGroundType) []
                , literalSubtypeRelationEntry @()
                , showableSubtypeRelationEntry @()
                , valBDT "concatUnit" "Concatenate units." $ mconcat @()
                ]
          , headingBDT
                "Boolean"
                ""
                [ typeBDT
                      "Boolean"
                      ""
                      (MkSomeGroundType booleanGroundType)
                      [ valPatBDT "True" "Boolean TRUE." True $
                        ImpureFunction $ \v ->
                            if v
                                then Just ()
                                else Nothing
                      , valPatBDT "False" "Boolean FALSE." False $
                        ImpureFunction $ \v ->
                            if v
                                then Nothing
                                else Just ()
                      ]
                , literalSubtypeRelationEntry @Bool
                , showableSubtypeRelationEntry @Bool
                , valBDT "&&" "Boolean AND." (&&)
                , valBDT "||" "Boolean OR." (||)
                , valBDT "not" "Boolean NOT." not
                ]
          , headingBDT
                "Ordering"
                ""
                [ typeBDT
                      "Ordering"
                      ""
                      (MkSomeGroundType orderingGroundType)
                      [ valPatBDT "LT" "Less than." LT $
                        ImpureFunction $ \v ->
                            case v of
                                LT -> Just ()
                                _ -> Nothing
                      , valPatBDT "EQ" "Equal to." EQ $
                        ImpureFunction $ \v ->
                            case v of
                                EQ -> Just ()
                                _ -> Nothing
                      , valPatBDT "GT" "Greater than." GT $
                        ImpureFunction $ \v ->
                            case v of
                                GT -> Just ()
                                _ -> Nothing
                      ]
                , literalSubtypeRelationEntry @Ordering
                , showableSubtypeRelationEntry @Ordering
                , valBDT "eq" "Equal." $ (==) EQ
                , valBDT "ne" "Not equal." $ (/=) EQ
                , valBDT "lt" "Less than." $ (==) LT
                , valBDT "le" "Less than or equal to." $ (/=) GT
                , valBDT "gt" "Greater than." $ (==) GT
                , valBDT "ge" "Greater than or equal to." $ (/=) LT
                , valBDT "lesser" "The lesser of two weevils." lesser
                , valBDT "greater" "The greater of two weevils." greater
                , valBDT "alphabetical" "Alphabetical first, then lower case before upper, per Unicode normalisation." $
                  Text.Collate.collate Text.Collate.rootCollator
                , valBDT "numerical" "Numercal order." $ compare @Number
                , valBDT "chronological" "Chronological order." $ compare @UTCTime
                , valBDT "durational" "Durational order." $ compare @NominalDiffTime
                , valBDT "calendrical" "Date order." $ compare @Day
                , valBDT "horological" "Time of day order." $ compare @TimeOfDay
                , valBDT "localChronological" "Local time order." $ compare @LocalTime
                , valBDT "noOrder" "No order, everything EQ." $ noOrder @TopType
                , valBDT "orders" "Join orders by priority." $ concatOrders @A
                , valBDT "reverse" "Reverse an order." $ reverseOrder @A
                , valBDT "sort" "Sort by an order." (sortBy :: (A -> A -> Ordering) -> [A] -> [A])
                ]
          , headingBDT
                "Text"
                ""
                [ typeBDT "Text" "" (MkSomeGroundType textGroundType) []
                , literalSubtypeRelationEntry @Text
                , showableSubtypeRelationEntry @Text
                , valBDT "<>" "Concatenate text." $ (<>) @Text
                , valBDT "textLength" "The length of a piece of text." $ olength @Text
                , valBDT
                      "textSection"
                      "`textSection start len text` is the section of `text` beginning at `start` of length `len`." $ \start len (text :: Text) ->
                      take len $ drop start text
                , valBDT "textConcat" "Concatenate texts." $ mconcat @Text
                , valBDT "toUpperCase" "" Data.Text.toUpper
                , valBDT "toLowerCase" "" Data.Text.toLower
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
                in headingBDT
                       "Numeric"
                       ""
                       [ valBDT "~==" "Numeric equality, folding exact and inexact numbers." $ (==) @Number
                       , valBDT "~/=" "Numeric non-equality." $ (/=) @Number
                       , valBDT "<" "Numeric strictly less." $ (<) @Number
                       , valBDT "<=" "Numeric less or equal." $ (<=) @Number
                       , valBDT ">" "Numeric strictly greater." $ (>) @Number
                       , valBDT ">=" "Numeric greater or equal." $ (>=) @Number
                       , headingBDT "Integer" "" $
                         [ typeBDT "Integer" "" (MkSomeGroundType integerGroundType) []
                         , hasSubtypeRelationBDT @Integer @SafeRational Verify "" $
                           functionToShim "integerSafeRational" $ encode integerSafeRational
                         ] <>
                         plainFormattingDefs @Integer "Integer" "an integer" <>
                         [ valBDT "min" "Lesser of two Integers" $ min @Integer
                         , valBDT "max" "Greater of two Integers" $ max @Integer
                         , valBDT "+" "Add." $ (+) @Integer
                         , valBDT "-" "Subtract." $ (-) @Integer
                         , valBDT "*" "Multiply." $ (*) @Integer
                         , valBDT "negate" "Negate." $ negate @Integer
                         , valBDT "abs" "Absolute value." $ abs @Integer
                         , valBDT "signum" "Sign." $ signum @Integer
                         , valBDT "mod" "Modulus, leftover from `div`" $ mod' @Integer
                         , valBDT "even" "Is even?" $ even @Integer
                         , valBDT "odd" "Is odd?" $ odd @Integer
                         , valBDT "gcd" "Greatest common divisor." $ gcd @Integer
                         , valBDT "lcm" "Least common multiple." $ lcm @Integer
                         , valBDT "^" "Raise to non-negative power." $ (^) @Integer @Integer
                         , valBDT "sum" "Sum." $ sum @[] @Integer
                         , valBDT "product" "Product." $ product @[] @Integer
                         , valBDT
                               "range"
                               "`range a b` is an arithmetic sequence starting from `a`, with all numbers `<= b`. Step is +1." $
                           range @Integer
                         , valBDT
                               "arithList"
                               "`arithList step a (Just b)` is an arithmetic sequence starting from `a`, with all numbers `<= b` (for positive step) or `>= b` (for negative step).\n\n\
                                \`arithList step a Nothing` is an infinite arithmetic sequence starting from `a`." $
                           arithList @Integer
                         ]
                       , headingBDT "Rational" "" $
                         [ typeBDT "Rational" "" (MkSomeGroundType rationalGroundType) []
                         , hasSubtypeRelationBDT @SafeRational @Number Verify "" $
                           functionToShim "safeRationalNumber" $ encode safeRationalNumber
                         ] <>
                         plainFormattingDefs @SafeRational "Rational" "a rational" <>
                         [ valBDT "minR" "Lesser of two Rationals" $ min @SafeRational
                         , valBDT "maxR" "Greater of two Rationals" $ max @SafeRational
                         , valBDT ".+" "Add." $ (+) @SafeRational
                         , valBDT ".-" "Subtract." $ (-) @SafeRational
                         , valBDT ".*" "Multiply." $ (*) @SafeRational
                         , valBDT "/" "Divide." $ (/) @SafeRational
                         , valBDT "negateR" "Negate." $ negate @SafeRational
                         , valBDT "recip" "Reciprocal." $ recip @SafeRational
                         , valBDT "absR" "Absolute value." $ abs @SafeRational
                         , valBDT "signumR" "Sign." $ signum @SafeRational
                         , valBDT "modR" "Modulus, leftover from `div`" $ mod' @SafeRational
                         , valBDT "^^" "Raise to Integer power." $ ((^^) :: SafeRational -> Integer -> SafeRational)
                         , valBDT "sumR" "Sum." $ sum @[] @SafeRational
                         , valBDT "meanR" "Mean." $ \(vv :: [SafeRational]) -> sum vv / toSafeRational (length vv)
                         , valBDT "productR" "Product." $ product @[] @SafeRational
                         ]
                       , headingBDT "Number" "" $
                         plainFormattingDefs @Number "Number" "a number" <>
                         [ typeBDT "Number" "" (MkSomeGroundType numberGroundType) []
                         , literalSubtypeRelationEntry @Number
                         , showableSubtypeRelationEntry @Number
                         , valBDT "minN" "Lesser of two Numbers" $ min @Number
                         , valBDT "maxN" "Greater of two Numbers" $ max @Number
                         , valBDT "~+" "Add." $ (+) @Number
                         , valBDT "~-" "Subtract." $ (-) @Number
                         , valBDT "~*" "Multiply." $ (*) @Number
                         , valBDT "~/" "Divide." $ (/) @Number
                         , valBDT "negateN" "Negate." $ negate @Number
                         , valBDT "recipN" "Reciprocal." $ recip @Number
                         , valBDT "pi" "Half the radians in a circle." $ pi @Number
                         , valBDT "exp" "Exponent" $ exp @Number
                         , valBDT "log" "Natural logarithm" $ log @Number
                         , valBDT "sqrt" "Square root." $ sqrt @Number
                         , valBDT "**" "Raise to power." $ (**) @Number
                         , valBDT "logBase" "" $ logBase @Number
                         , valBDT "sin" "Sine of an angle in radians." $ sin @Number
                         , valBDT "cos" "Cosine of an angle in radians." $ cos @Number
                         , valBDT "tan" "Tangent of an angle in radians." $ tan @Number
                         , valBDT "asin" "Radian angle of a sine." $ asin @Number
                         , valBDT "acos" "Radian angle of a cosine." $ acos @Number
                         , valBDT "atan" "Radian angle of a tangent." $ atan @Number
                         , valBDT "sinh" "Hyperbolic sine." $ sinh @Number
                         , valBDT "cosh" "Hyperbolic cosine." $ cosh @Number
                         , valBDT "tanh" "Hyperbolic tangent." $ tanh @Number
                         , valBDT "asinh" "Inverse hyperbolic sine." $ asinh @Number
                         , valBDT "acosh" "Inverse hyperbolic cosine." $ acosh @Number
                         , valBDT "atanh" "Inverse hyperbolic tangent." $ atanh @Number
                         , valBDT "nabs" "Absolute value." $ abs @Number
                         , valBDT "signumN" "Sign. Note this will be the same exact or inexact as the number." $
                           signum @Number
                         , valBDT "floor" "Integer towards negative infinity." (floor :: Number -> Integer)
                         , valBDT "ceiling" "Integer towards positive infinity." (ceiling :: Number -> Integer)
                         , valBDT "round" "Closest Integer." (round :: Number -> Integer)
                         , valBDT "inexact" "Convert a number to inexact." numberToDouble
                         , valBDT
                               "approximate"
                               "`approximate d x` gives the exact number that's a multiple of `d` that's closest to `x`."
                               approximate
                         , valBDT
                               "div"
                               "Division to Integer, towards negative infinity."
                               (div' :: Number -> Number -> Integer)
                         , valBDT "modN" "Modulus, leftover from `div`" $ mod' @Number
                         , valBDT "isNaN" "Is not a number?" numberIsNaN
                         , valBDT "isInfinite" "Is infinite?" numberIsInfinite
                         , valBDT "isNegativeZero" "Is negative zero?" numberIsNegativeZero
                         , valBDT "isExact" "Is exact?" numberIsExact
                         , valBDT "sumN" "Sum." $ sum @[] @Number
                         , valBDT "meanN" "Mean." $ \(vv :: [Number]) -> sum vv / (ExactNumber $ toRational $ length vv)
                         , valBDT "productN" "Product." $ product @[] @Number
                         , valBDT "numberCheckSafeRational" "Get the exact value of a Number, if it is one." $
                           decode safeRationalNumber
                         , valBDT
                               "checkExactInteger"
                               "Get the exact Integer value of a Number, if it is one. Works as expected on Rationals." $
                           decode $ integerSafeRational . safeRationalNumber
                         ]
                       ]
          , headingBDT
                "Date & Time"
                ""
                [ headingBDT "Duration" "" $
                  [ typeBDT
                        "Duration"
                        ""
                        (MkSomeGroundType durationGroundType)
                        [ valPatBDT "Seconds" "Construct a `Duration` from seconds." secondsToNominalDiffTime $
                          PureFunction $ \d -> (nominalDiffTimeToSeconds d, ())
                        ]
                  , literalSubtypeRelationEntry @NominalDiffTime
                  , showableSubtypeRelationEntry @NominalDiffTime
                  ] <>
                  -- plainFormattingDefs @NominalDiffTime "Duration" "a duration" <>
                  [ valBDT "zeroDuration" "No duration." $ (0 :: NominalDiffTime)
                  , valBDT "dayDuration" "One day duration." nominalDay
                  , valBDT "addDuration" "Add durations." $ (+) @NominalDiffTime
                  , valBDT "subtractDuration" "Subtract durations." $ (-) @NominalDiffTime
                  , valBDT "negateDuration" "Negate duration." $ negate @NominalDiffTime
                  , valBDT "multiplyDuration" "Multiply a duration by a number." $ \(n :: Number) (d :: NominalDiffTime) ->
                        (realToFrac n) * d
                  , valBDT "divideDuration" "Divide durations." $ \(a :: NominalDiffTime) (b :: NominalDiffTime) ->
                        (realToFrac (a / b) :: Number)
                  ]
                , headingBDT "Time" "" $
                  [ typeBDT "Time" "Absolute time as measured by UTC." (MkSomeGroundType timeGroundType) []
                  , literalSubtypeRelationEntry @UTCTime
                  , showableSubtypeRelationEntry @UTCTime
                  ] <>
                  plainFormattingDefs @UTCTime "Time" "a time" <>
                  unixFormattingDefs @UTCTime "Time" "a time" <>
                  [ valBDT "addTime" "Add duration to time." addUTCTime
                  , valBDT "diffTime" "Difference of times." diffUTCTime
                  , valBDT "getTime" "Get the current time." $ getCurrentTime
                  , valBDT "newClock" "Make a model of the current time that updates per the given duration." newClock
                  ]
                , headingBDT "Calendar" "" $
                  [ typeBDT
                        "Date"
                        ""
                        (MkSomeGroundType dateGroundType)
                        [ valPatBDT "YearMonthDay" "Construct a `Date` from year, month, day." fromGregorian $
                          PureFunction $ \day -> let
                              (y, m, d) = toGregorian day
                              in (y, (m, (d, ())))
                        , valPatBDT "ModifiedJulianDay" "Construct a `Date` from its MJD." ModifiedJulianDay $
                          PureFunction $ \day -> (toModifiedJulianDay day, ())
                        ]
                  , literalSubtypeRelationEntry @Day
                  , showableSubtypeRelationEntry @Day
                  ] <>
                  plainFormattingDefs @Day "Date" "a date" <>
                  unixFormattingDefs @Day "Date" "a date" <>
                  [ valBDT "addDays" "Add count to days to date." addDays
                  , valBDT "diffDays" "Difference of days between dates." diffDays
                  , valBDT "getUTCDate" "Get the current UTC date." $ fmap utctDay getCurrentTime
                  , valBDT "getDate" "Get the current local date." $ fmap localDay getLocalTime
                  ]
                , headingBDT "Time of Day" "" $
                  [ typeBDT
                        "TimeOfDay"
                        ""
                        (MkSomeGroundType timeOfDayGroundType)
                        [ valPatBDT "HourMinuteSecond" "Construct a `TimeOfDay` from hour, minute, second." TimeOfDay $
                          PureFunction $ \TimeOfDay {..} -> (todHour, (todMin, (todSec, ())))
                        , valPatBDT
                              "SinceMidnight"
                              "Construct a `TimeOfDay` from duration since midnight (wrapping whole days)."
                              (snd . timeToDaysAndTimeOfDay)
                              (PureFunction $ \t -> (daysAndTimeOfDayToTime 0 t, ()))
                        ]
                  , literalSubtypeRelationEntry @TimeOfDay
                  , showableSubtypeRelationEntry @TimeOfDay
                  ] <>
                  plainFormattingDefs @TimeOfDay "TimeOfDay" "a time of day" <>
                  unixFormattingDefs @TimeOfDay "TimeOfDay" "a time of day" <>
                  [valBDT "midnight" "Midnight." midnight, valBDT "midday" "Midday." midday]
                , headingBDT "Local Time" "" $
                  [ typeBDT
                        "LocalTime"
                        ""
                        (MkSomeGroundType localTimeGroundType)
                        [ valPatBDT "DateAndTime" "Construct a `LocalTime` from day and time of day." LocalTime $
                          PureFunction $ \LocalTime {..} -> (localDay, (localTimeOfDay, ()))
                        ]
                  , literalSubtypeRelationEntry @LocalTime
                  , showableSubtypeRelationEntry @LocalTime
                  ] <>
                  plainFormattingDefs @LocalTime "LocalTime" "a local time" <>
                  unixFormattingDefs @LocalTime "LocalTime" "a local time" <>
                  [ valBDT "timeToLocal" "Convert a time to local time, given a time zone offset in minutes" $ \i ->
                        utcToLocalTime $ minutesToTimeZone i
                  , valBDT "localToTime" "Convert a local time to time, given a time zone offset in minutes" $ \i ->
                        localTimeToUTC $ minutesToTimeZone i
                  , valBDT "getTimeZone" "Get the offset for a time in the current time zone." $ \t ->
                        fmap timeZoneMinutes $ getTimeZone t
                  , valBDT "getCurrentTimeZone" "Get the current time zone offset in minutes." $
                    fmap timeZoneMinutes getCurrentTimeZone
                  , valBDT "getLocalTime" "Get the current local time." getLocalTime
                  , valBDT "newTimeZoneModel" "The current time zone offset in minutes." newTimeZoneModel
                  ]
                ]
          , headingBDT
                "Open Entity Types"
                ""
                [ specialFormBDT
                      "openEntity"
                      "An open entity for this anchor. `A` is an open entity type."
                      ["@A", "<anchor>"]
                      "A" $
                  MkSpecialForm (ConsListType AnnotPositiveType $ ConsListType AnnotAnchor NilListType) $ \(t, (anchor, ())) -> do
                      mtp <- getOpenEntityType t
                      return $
                          case mtp of
                              MkSome (tp :: OpenEntityType tid) -> let
                                  typef = openEntityShimWit tp
                                  pt :: OpenEntity tid
                                  pt = MkOpenEntity $ MkEntity anchor
                                  in MkSomeOf typef pt
                , specialFormBDT
                      "newOpenEntity"
                      "Generate an open entity. `A` is an open entity type."
                      ["@A"]
                      "Action A" $
                  MkSpecialForm (ConsListType AnnotPositiveType NilListType) $ \(t, ()) -> do
                      mtp <- getOpenEntityType t
                      return $
                          case mtp of
                              MkSome (tp :: OpenEntityType tid) -> let
                                  pt :: Action (OpenEntity tid)
                                  pt = liftIO $ newKeyContainerItem @(FiniteSet (OpenEntity tid))
                                  typef = actionShimWit $ openEntityShimWit tp
                                  in MkSomeOf typef pt
                ]
          , headingBDT
                "Dynamic Entity Types"
                ""
                [ typeBDT "DynamicEntity" "" (MkSomeGroundType dynamicEntityGroundType) []
                , hasSubtypeRelationBDT @DynamicEntity @Entity Verify "" $
                  functionToShim "dynamicEntityAdapter" $ entityAdapterConvert $ dynamicEntityAdapter Nothing
                , specialFormBDT
                      "dynamicEntity"
                      "A dynamic entity for this anchor. `A` is a concrete dynamic entity type."
                      ["@A", "<anchor>"]
                      "A" $
                  MkSpecialForm (ConsListType AnnotPositiveType $ ConsListType AnnotAnchor NilListType) $ \(t, (anchor, ())) -> do
                      (n, dt) <- getConcreteDynamicEntityType t
                      let
                          typef = dynamicEntityShimWit n dt
                          pt :: DynamicEntity
                          pt = MkDynamicEntity dt $ MkEntity anchor
                      return $ MkSomeOf typef pt
                , specialFormBDT
                      "newDynamicEntity"
                      "Generate a dynamic entity. `A` is a concrete dynamic entity type."
                      ["@A"]
                      "Action A" $
                  MkSpecialForm (ConsListType AnnotPositiveType NilListType) $ \(t, ()) -> do
                      (n, dt) <- getConcreteDynamicEntityType t
                      let
                          pt :: Action DynamicEntity
                          pt =
                              liftIO $ do
                                  e <- newKeyContainerItem @(FiniteSet Entity)
                                  return $ MkDynamicEntity dt e
                          typef = actionShimWit $ dynamicEntityShimWit n dt
                      return $ MkSomeOf typef pt
                ]
          ]
    , headingBDT
          "Maybe"
          ""
          [ typeBDT
                "Maybe"
                ""
                (MkSomeGroundType maybeGroundType)
                [ valPatBDT "Just" "Construct a Maybe from a value." (Just @A) $
                  ImpureFunction $ \(v :: Maybe A) ->
                      case v of
                          Just a -> Just (a, ())
                          _ -> Nothing
                , valPatBDT "Nothing" "Construct a Maybe without a value." (Nothing @BottomType) $
                  ImpureFunction $ \(v :: Maybe A) ->
                      case v of
                          Nothing -> Just ()
                          _ -> Nothing
                ]
          , hasSubtypeRelationBDT @(Maybe Entity) @Entity Verify "" $
            functionToShim "maybeEntityConvert" maybeEntityConvert
          , hasSubtypeRelationBDT @(Maybe Showable) @Showable Verify "" $ functionToShim "show" textShowable
          ]
    , headingBDT
          "*:"
          ""
          [ typeBDT "*:" "" (MkSomeGroundType pairGroundType) []
          , hasSubtypeRelationBDT @(Entity, Entity) @Entity Verify "" $
            functionToShim "pairEntityConvert" pairEntityConvert
          , hasSubtypeRelationBDT @(Showable, Showable) @Showable Verify "" $ functionToShim "show" textShowable
          , valBDT "fst" "Get the first member of a pair." $ fst @A @B
          , valBDT "snd" "Get the second member of a pair." $ snd @A @B
          , valBDT "toPair" "Construct a pair." $ (,) @A @B
          , valBDT "pair" "Construct a pair." $ \(a :: A) -> (a, a)
          ]
    , headingBDT
          "+:"
          ""
          [ typeBDT
                "+:"
                ""
                (MkSomeGroundType eitherGroundType)
                [ valPatBDT "Left" "Construct an Either from the left." (Left @A @B) $
                  ImpureFunction $ \(v :: Either A B) ->
                      case v of
                          Left a -> Just (a, ())
                          _ -> Nothing
                , valPatBDT "Right" "Construct an Either from the right." (Right @A @B) $
                  ImpureFunction $ \(v :: Either A B) ->
                      case v of
                          Right a -> Just (a, ())
                          _ -> Nothing
                ]
          , hasSubtypeRelationBDT @(Either Entity Entity) @Entity Verify "" $
            functionToShim "eitherEntityConvert" eitherEntityConvert
          , hasSubtypeRelationBDT @(Either Showable Showable) @Showable Verify "" $ functionToShim "show" textShowable
          , valBDT "fromEither" "Eliminate a sum" $ either @A @C @B
          , valBDT "either" "Eliminate a sum" $ \(v :: Either A A) ->
                case v of
                    Left a -> a
                    Right a -> a
          ]
    , headingBDT
          "Lists"
          ""
          [ typeBDT
                "List"
                "A list."
                (MkSomeGroundType listGroundType)
                [ mkTypeBDT "List1" "A list with at least one element." (MkSomeGroundType list1GroundType) []
                , hasSubtypeRelationBDT @(NonEmpty A) @[A] Verify "" $ functionToShim "NonEmpty.toList" toList
                , valPatBDT "[]" "Empty list" ([] @BottomType) $
                  ImpureFunction $ \(v :: [A]) ->
                      case v of
                          [] -> Just ()
                          _ -> Nothing
                , valPatBDT "::" "Construct a list" ((:|) @A) $
                  ImpureFunction $ \(v :: [A]) ->
                      case v of
                          a:b -> Just (a, (b, ()))
                          _ -> Nothing
                ]
          , hasSubtypeRelationBDT @[Entity] @Entity Verify "" $ functionToShim "listEntityConvert" listEntityConvert
          , hasSubtypeRelationBDT @[Showable] @Showable Verify "" $ functionToShim "show" textShowable
          , valBDT "list" "Eliminate a list" $ \(fnil :: B) fcons (l :: [A]) ->
                case l of
                    [] -> fnil
                    (a:aa) -> fcons a aa
          , valBDT "length" "Number of items in a list" (length :: [TopType] -> Int)
          , valBDT "index" "Get item from list by index." (index :: [A] -> Int -> Maybe A)
          , valBDT "mapList" "Map the items of a list." (fmap :: (A -> B) -> [A] -> [B])
          , valBDT "++" "Concatentate lists." ((++) :: [A] -> [A] -> [A])
          , valBDT "filter" "Filter a list." (filter :: (A -> Bool) -> [A] -> [A])
          , valBDT "maybeMapList" "Map and filter a list." (mapMaybe :: (A -> Maybe B) -> [A] -> [B])
          , valBDT "take" "Take the first n elements." (take :: Int -> [A] -> [A])
          , valBDT "drop" "Drop the first n elements." (drop :: Int -> [A] -> [A])
          , valBDT "takeWhile" "Take while the condition holds." (takeWhile :: (A -> Bool) -> [A] -> [A])
          , valBDT "dropWhile" "Drop while the condition holds." (dropWhile :: (A -> Bool) -> [A] -> [A])
          , valBDT "zip" "Zip two lists." $ zip @A @B
          ]
    , headingBDT
          "Functions"
          ""
          [ typeBDT "->" "A pure function." (MkSomeGroundType funcGroundType) []
          , valBDT "id" "The identity function." $ id @(->) @A
          , valBDT "$" "Apply a function to a value." $ id @(->) @(A -> B)
          , valBDT ">-" "Apply a value to a function." revap
          , valBDT "." "Compose functions." $ (.) @(->) @A @B @C
          , valBDT "error" "Error." $ ((\t -> error (unpack t)) :: Text -> BottomType)
          , valBDT
                "seq"
                "Evaluate the first argument, then if that's not \"bottom\" (error or non-termination), return the second argument."
                (seq :: TopType -> A -> A)
          , specialFormBDT "check" "Check from a dynamic supertype." ["@A"] "D(A) -> Maybe A" $
            MkSpecialForm (ConsListType AnnotNegativeType NilListType) $ \(MkSome tn, ()) -> do
                let dtw = getGreatestDynamicSupertype tn
                tpw <- invertType tn
                return $ MkSomeOf (funcShimWit dtw $ maybeShimWit tpw) id
          , specialFormBDT "coerce" "Coerce from a dynamic supertype." ["@A"] "D(A) -> A" $
            MkSpecialForm (ConsListType AnnotNegativeType NilListType) $ \(MkSome tn, ()) -> do
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
