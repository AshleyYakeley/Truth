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
import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.Convert.Types
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

asTextPrism ::
       forall a. (Read a, TextShow a)
    => LangPrism '( Text, Text) '( a, a)
asTextPrism = prism (readMaybe . unpack) textShow

plainFormattingDef ::
       forall t context. (HasQType 'Positive t, HasQType 'Negative t, Read t, TextShow t)
    => Text
    -> BindDocTree context
plainFormattingDef lname = valBDT "asText" ("Represent " <> plainText lname <> " as text.") $ asTextPrism @t

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
    -> BindDocTree context
unixFormattingDef lname =
    valBDT
        (UnqualifiedFullNameRef $ MkName $ "unixAsText")
        ("Interpret " <> plainText lname <> " model as text, interpreting deleted values as empty text.") $
    unixAsText @t

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

append :: NonEmpty A -> [A] -> NonEmpty A
append (a :| aa) bb = a :| (aa <> bb)

mconcat1 :: NonEmpty (NonEmpty A) -> NonEmpty A
mconcat1 (na :| lna) = append na $ mconcat $ fmap toList lna

baseLibSections :: [BindDocTree context]
baseLibSections =
    [ headingBDT "Literals & Entities" "" $
      [ typeBDT "Entity" "" (MkSomeGroundType entityGroundType) []
      , namespaceBDT "Entity" "" $
        fmap nameInRootBDT (eqEntries @_ @Entity) <> [valBDT "anchor" "The anchor of an entity, as text." entityAnchor]
      , typeBDT "Literal" "" (MkSomeGroundType literalGroundType) []
      , hasSubtypeRelationBDT @Literal @Entity Verify "" $ functionToShim "literalToEntity" literalToEntity
      , headingBDT
            "Showable"
            ""
            [ typeBDT
                  "Showable"
                  "Something that can be represented as `Text`."
                  (MkSomeGroundType showableGroundType)
                  [valPatBDT "MkShowable" "" MkShowable $ PureFunction $ \(MkShowable t) -> (t, ())]
            , namespaceBDT "Showable" "" [nameInRootBDT $ valBDT "show" "Show something as `Text`" $ textShow @Showable]
            ]
      , headingBDT
            "Unit"
            ""
            [ typeBDT "Unit" "" (MkSomeGroundType unitGroundType) []
            , literalSubtypeRelationEntry @()
            , showableSubtypeRelationEntry @()
            , namespaceBDT "Unit" "" $ monoidEntries @_ @()
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
            , namespaceBDT
                  "Boolean"
                  ""
                  [ nameInRootBDT $ valBDT "&&" "Boolean AND." (&&)
                  , nameInRootBDT $ valBDT "||" "Boolean OR." (||)
                  , nameInRootBDT $ valBDT "not" "Boolean NOT." not
                  ]
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
            , namespaceBDT "Ordering" "" $
              monoidEntries @_ @Ordering <>
              [ nameInRootBDT $ valBDT "eq" "Equal." $ (==) EQ
              , nameInRootBDT $ valBDT "ne" "Not equal." $ (/=) EQ
              , nameInRootBDT $ valBDT "lt" "Less than." $ (==) LT
              , nameInRootBDT $ valBDT "le" "Less than or equal to." $ (/=) GT
              , nameInRootBDT $ valBDT "gt" "Greater than." $ (==) GT
              , nameInRootBDT $ valBDT "ge" "Greater than or equal to." $ (/=) LT
              ]
            , namespaceBDT "Order" "" $
              monoidEntries @_ @(A -> A -> Ordering) <>
              [ valBDT "reverse" "Reverse an order." $ reverseOrder @A
              , nameInRootBDT $ valBDT "lesser" "The lesser of two items." lesser
              , nameInRootBDT $ valBDT "greater" "The greater of two items." greater
              , nameInRootBDT $
                valBDT "alphabetical" "Alphabetical first, then lower case before upper, per Unicode normalisation." $
                Text.Collate.collate Text.Collate.rootCollator
              , nameInRootBDT $ valBDT "numerical" "Numercal order." $ compare @Number
              , nameInRootBDT $ valBDT "chronological" "Chronological order." $ compare @UTCTime
              , nameInRootBDT $ valBDT "durational" "Durational order." $ compare @NominalDiffTime
              , nameInRootBDT $ valBDT "calendrical" "Date order." $ compare @Day
              , nameInRootBDT $ valBDT "horological" "Time of day order." $ compare @TimeOfDay
              , nameInRootBDT $ valBDT "localChronological" "Local time order." $ compare @LocalTime
              ]
            ]
      , headingBDT
            "Text"
            ""
            [ typeBDT "Text" "" (MkSomeGroundType textGroundType) []
            , literalSubtypeRelationEntry @Text
            , showableSubtypeRelationEntry @Text
            , namespaceBDT "Text" "" $
              monoidEntries @_ @Text <>
              [ valBDT "length" "The length of a text." $ olength @Text
              , valBDT
                    "section"
                    "`section start len text` is the section of `text` beginning at `start` of length `len`." $ \start len (text :: Text) ->
                    take len $ drop start text
              , valBDT "toUpperCase" "" Data.Text.toUpper
              , valBDT "toLowerCase" "" Data.Text.toLower
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
            in headingBDT
                   "Numeric"
                   ""
                   [ headingBDT "Integer" "" $
                     [ typeBDT "Integer" "" (MkSomeGroundType integerGroundType) []
                     , hasSubtypeRelationBDT @Integer @SafeRational Verify "" $
                       functionToShim "integerSafeRational" $ encode integerSafeRational
                     , namespaceBDT "Integer" "" $
                       pickNamesInRootBDT ["min", "max"] (ordEntries @_ @Integer) <>
                       pickNamesInRootBDT ["succ", "pred"] (enumEntries @_ @Integer) <>
                       [ plainFormattingDef @Integer "an integer"
                       , nameInRootBDT $ valBDT "+" "Add." $ (+) @Integer
                       , nameInRootBDT $ valBDT "-" "Subtract." $ (-) @Integer
                       , nameInRootBDT $ valBDT "*" "Multiply." $ (*) @Integer
                       , nameInRootBDT $ valBDT "negate" "Negate." $ negate @Integer
                       , nameInRootBDT $ valBDT "abs" "Absolute value." $ abs @Integer
                       , nameInRootBDT $ valBDT "signum" "Sign." $ signum @Integer
                       , nameInRootBDT $ valBDT "mod" "Modulus, leftover from `div`" $ mod' @Integer
                       , nameInRootBDT $ valBDT "even" "Is even?" $ even @Integer
                       , nameInRootBDT $ valBDT "odd" "Is odd?" $ odd @Integer
                       , nameInRootBDT $ valBDT "gcd" "Greatest common divisor." $ gcd @Integer
                       , nameInRootBDT $ valBDT "lcm" "Least common multiple." $ lcm @Integer
                       , nameInRootBDT $ valBDT "^" "Raise to non-negative power." $ (^) @Integer @Integer
                       , nameInRootBDT $ valBDT "sum" "Sum." $ sum @[] @Integer
                       , nameInRootBDT $ valBDT "product" "Product." $ product @[] @Integer
                       , nameInRootBDT $
                         valBDT
                             "range"
                             "`range a b` is an arithmetic sequence starting from `a`, with all numbers `<= b`. Step is +1." $
                         range @Integer
                       , nameInRootBDT $
                         valBDT
                             "arithList"
                             "`arithList step a (Just b)` is an arithmetic sequence starting from `a`, with all numbers `<= b` (for positive step) or `>= b` (for negative step).\n\n\
                                \`arithList step a Nothing` is an infinite arithmetic sequence starting from `a`." $
                         arithList @Integer
                       ]
                     ]
                   , headingBDT "Rational" "" $
                     [ typeBDT "Rational" "" (MkSomeGroundType rationalGroundType) []
                     , hasSubtypeRelationBDT @SafeRational @Number Verify "" $
                       functionToShim "safeRationalNumber" $ encode safeRationalNumber
                     , namespaceBDT
                           "Rational"
                           ""
                           [ plainFormattingDef @SafeRational "a rational"
                           , valBDT "min" "Lesser of two Rationals" $ min @SafeRational
                           , valBDT "max" "Greater of two Rationals" $ max @SafeRational
                           , valBDT "+" "Add." $ (+) @SafeRational
                           , valBDT "-" "Subtract." $ (-) @SafeRational
                           , valBDT "*" "Multiply." $ (*) @SafeRational
                           , nameInRootBDT $ valBDT "/" "Divide." $ (/) @SafeRational
                           , valBDT "negate" "Negate." $ negate @SafeRational
                           , valBDT "recip" "Reciprocal." $ recip @SafeRational
                           , valBDT "abs" "Absolute value." $ abs @SafeRational
                           , valBDT "signum" "Sign." $ signum @SafeRational
                           , valBDT "mod" "Modulus, leftover from `div`" $ mod' @SafeRational
                           , valBDT "^" "Raise to Integer power." $ ((^^) :: SafeRational -> Integer -> SafeRational)
                           , valBDT "sum" "Sum." $ sum @[] @SafeRational
                           , valBDT "mean" "Mean." $ \(vv :: [SafeRational]) -> sum vv / toSafeRational (length vv)
                           , valBDT "product" "Product." $ product @[] @SafeRational
                           ]
                     ]
                   , headingBDT "Number" "" $
                     [ typeBDT "Number" "" (MkSomeGroundType numberGroundType) []
                     , literalSubtypeRelationEntry @Number
                     , showableSubtypeRelationEntry @Number
                     , namespaceBDT "Number" "" $
                       pickNamesInRootBDT ["<", "<=", ">", ">="] (ordEntries @_ @Number) <>
                       [ plainFormattingDef @Number "a number"
                       , valBDT "+" "Add." $ (+) @Number
                       , valBDT "-" "Subtract." $ (-) @Number
                       , valBDT "*" "Multiply." $ (*) @Number
                       , valBDT "/" "Divide." $ (/) @Number
                       , valBDT "negate" "Negate." $ negate @Number
                       , valBDT "recip" "Reciprocal." $ recip @Number
                       , nameInRootBDT $ valBDT "pi" "Half the radians in a circle." $ pi @Number
                       , nameInRootBDT $ valBDT "exp" "Exponent" $ exp @Number
                       , nameInRootBDT $ valBDT "log" "Natural logarithm" $ log @Number
                       , nameInRootBDT $ valBDT "sqrt" "Square root." $ sqrt @Number
                       , valBDT "^" "Raise to power." $ (**) @Number
                       , nameInRootBDT $ valBDT "logBase" "" $ logBase @Number
                       , nameInRootBDT $ valBDT "sin" "Sine of an angle in radians." $ sin @Number
                       , nameInRootBDT $ valBDT "cos" "Cosine of an angle in radians." $ cos @Number
                       , nameInRootBDT $ valBDT "tan" "Tangent of an angle in radians." $ tan @Number
                       , nameInRootBDT $ valBDT "asin" "Radian angle of a sine." $ asin @Number
                       , nameInRootBDT $ valBDT "acos" "Radian angle of a cosine." $ acos @Number
                       , nameInRootBDT $ valBDT "atan" "Radian angle of a tangent." $ atan @Number
                       , nameInRootBDT $ valBDT "sinh" "Hyperbolic sine." $ sinh @Number
                       , nameInRootBDT $ valBDT "cosh" "Hyperbolic cosine." $ cosh @Number
                       , nameInRootBDT $ valBDT "tanh" "Hyperbolic tangent." $ tanh @Number
                       , nameInRootBDT $ valBDT "asinh" "Inverse hyperbolic sine." $ asinh @Number
                       , nameInRootBDT $ valBDT "acosh" "Inverse hyperbolic cosine." $ acosh @Number
                       , nameInRootBDT $ valBDT "atanh" "Inverse hyperbolic tangent." $ atanh @Number
                       , valBDT "abs" "Absolute value." $ abs @Number
                       , valBDT "signum" "Sign. Note this will be the same exact or inexact as the number." $
                         signum @Number
                       , nameInRootBDT $
                         valBDT "floor" "Integer towards negative infinity." (floor :: Number -> Integer)
                       , nameInRootBDT $
                         valBDT "ceiling" "Integer towards positive infinity." (ceiling :: Number -> Integer)
                       , nameInRootBDT $ valBDT "round" "Closest Integer." (round :: Number -> Integer)
                       , nameInRootBDT $ valBDT "inexact" "Convert a number to inexact." numberToDouble
                       , nameInRootBDT $
                         valBDT
                             "approximate"
                             "`approximate d x` gives the exact number that's a multiple of `d` that's closest to `x`."
                             approximate
                       , nameInRootBDT $
                         valBDT
                             "div"
                             "Division to Integer, towards negative infinity."
                             (div' :: Number -> Number -> Integer)
                       , nameInRootBDT $ valBDT "mod" "Modulus, leftover from `div`" $ mod' @Number
                       , nameInRootBDT $ valBDT "isNaN" "Is not a number?" numberIsNaN
                       , nameInRootBDT $ valBDT "isInfinite" "Is infinite?" numberIsInfinite
                       , nameInRootBDT $ valBDT "isNegativeZero" "Is negative zero?" numberIsNegativeZero
                       , nameInRootBDT $ valBDT "isExact" "Is exact?" numberIsExact
                       , valBDT "sum" "Sum." $ sum @[] @Number
                       , valBDT "mean" "Mean." $ \(vv :: [Number]) -> sum vv / (ExactNumber $ toRational $ length vv)
                       , valBDT "product" "Product." $ product @[] @Number
                       , nameInRootBDT $
                         valBDT "checkSafeRational" "Get the exact value of a Number, if it is one." $
                         decode safeRationalNumber
                       , nameInRootBDT $
                         valBDT
                             "checkExactInteger"
                             "Get the exact Integer value of a Number, if it is one. Works as expected on Rationals." $
                         decode $ integerSafeRational . safeRationalNumber
                       ]
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
              , namespaceBDT
                    "Duration"
                    ""
                  -- plainFormattingDef @NominalDiffTime  "a duration"
                    [ valBDT "zero" "No duration." $ (0 :: NominalDiffTime)
                    , valBDT "day" "One day duration." nominalDay
                    , valBDT "+" "Add durations." $ (+) @NominalDiffTime
                    , valBDT "-" "Subtract durations." $ (-) @NominalDiffTime
                    , valBDT "negate" "Negate duration." $ negate @NominalDiffTime
                    , valBDT "*" "Multiply a duration by a number." $ \(n :: Number) (d :: NominalDiffTime) ->
                          (realToFrac n) * d
                    , valBDT "/" "Divide durations." $ \(a :: NominalDiffTime) (b :: NominalDiffTime) ->
                          (realToFrac (a / b) :: Number)
                    ]
              ]
            , headingBDT "Time" "" $
              [ typeBDT "Time" "Absolute time as measured by UTC." (MkSomeGroundType timeGroundType) []
              , literalSubtypeRelationEntry @UTCTime
              , showableSubtypeRelationEntry @UTCTime
              , namespaceBDT
                    "Time"
                    ""
                    [ plainFormattingDef @UTCTime "a time"
                    , unixFormattingDef @UTCTime "a time"
                    , valBDT "+" "Add duration to time." addUTCTime
                    , valBDT "-" "Difference of times." diffUTCTime
                    , valBDT "getNow" "Get the current time." $ getCurrentTime
                    , nameInRootBDT $
                      valBDT "newClock" "Make a model of the current time that updates per the given duration." newClock
                    ]
              ]
            , headingBDT "Date" "" $
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
              , namespaceBDT "Date" "" $
                enumEntries @_ @Day <>
                [ plainFormattingDef @Day "a date"
                , unixFormattingDef @Day "a date"
                , valBDT "+" "Add count to days to date." addDays
                , valBDT "-" "Difference of days between dates." diffDays
                , valBDT "getNowUTC" "Get the current UTC date." $ fmap utctDay getCurrentTime
                , valBDT "getNowLocal" "Get the current local date." $ fmap localDay getLocalTime
                ]
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
              , namespaceBDT
                    "TimeOfDay"
                    ""
                    [ plainFormattingDef @TimeOfDay "a time of day"
                    , unixFormattingDef @TimeOfDay "a time of day"
                    , nameInRootBDT $ valBDT "midnight" "Midnight." midnight
                    , nameInRootBDT $ valBDT "midday" "Midday." midday
                    ]
              ]
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
              , namespaceBDT
                    "LocalTime"
                    ""
                    [ plainFormattingDef @LocalTime "a local time"
                    , unixFormattingDef @LocalTime "a local time"
                    , valBDT "fromTime" "Convert a time to local time, given a time zone offset in minutes" $ \i ->
                          utcToLocalTime $ minutesToTimeZone i
                    , valBDT "toTime" "Convert a local time to time, given a time zone offset in minutes" $ \i ->
                          localTimeToUTC $ minutesToTimeZone i
                    , valBDT "getTimeZone" "Get the offset for a time in the current time zone." $ \t ->
                          fmap timeZoneMinutes $ getTimeZone t
                    , valBDT "getCurrentTimeZone" "Get the current time zone offset in minutes." $
                      fmap timeZoneMinutes getCurrentTimeZone
                    , valBDT "getNow" "Get the current local time." getLocalTime
                    , valBDT "newTimeZoneModel" "The current time zone offset in minutes." newTimeZoneModel
                    ]
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
            , specialFormBDT "newOpenEntity" "Generate an open entity. `A` is an open entity type." ["@A"] "Action A" $
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
            [ typeBDT "DynamicEntity" "" (MkSomeGroundType dynamicStorableGroundType) []
            , hasSubtypeRelationBDT @DynamicEntity @Entity Verify "" $
              functionToShim "dynamicStoreAdapter" $ storeAdapterConvert $ dynamicStoreAdapter Nothing
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
          , namespaceBDT "Maybe" "" $ monadEntries @_ @Maybe
          ]
    , headingBDT
          "Type Product"
          ""
          [ typeBDT "*:" "" (MkSomeGroundType pairGroundType) []
          , hasSubtypeRelationBDT @(Entity, Entity) @Entity Verify "" $
            functionToShim "pairEntityConvert" pairEntityConvert
          , hasSubtypeRelationBDT @(Showable, Showable) @Showable Verify "" $ functionToShim "show" textShowable
          , namespaceBDT
                "Product"
                ""
                [ nameInRootBDT $ valBDT "fst" "Get the first member of a pair." $ fst @A @B
                , nameInRootBDT $ valBDT "snd" "Get the second member of a pair." $ snd @A @B
                , valBDT "to" "Construct a pair." $ (,) @A @B
                , nameInRootBDT $ valBDT "both" "Construct a pair." $ \(a :: A) -> (a, a)
                ]
          ]
    , headingBDT
          "Type Sum"
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
          , namespaceBDT "Sum" "" $
            monadEntries @_ @(Either P) <>
            [ valBDT "from" "Eliminate a sum" $ either @A @C @B
            , nameInRootBDT $
              valBDT "either" "Eliminate a sum" $ \(v :: Either A A) ->
                  case v of
                      Left a -> a
                      Right a -> a
            ]
          ]
    , headingBDT
          "List"
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
          , namespaceBDT "List" "" $
            monoidEntries @_ @[A] <>
            monadEntries @_ @[] <>
            [ valBDT "from" "Eliminate a list" $ \(fnil :: B) fcons (l :: [A]) ->
                  case l of
                      [] -> fnil
                      (a:aa) -> fcons a aa
            , nameInRootBDT $ valBDT "length" "Number of items in a list" (length :: [TopType] -> Int)
            , nameInRootBDT $ valBDT "index" "Get item from list by index." (index :: [A] -> Int -> Maybe A)
            , valBDT "map" "Map the items of a list." (fmap :: (A -> B) -> [A] -> [B])
            , nameInRootBDT $ valBDT "filter" "Filter a list." (filter :: (A -> Bool) -> [A] -> [A])
            , nameInRootBDT $ valBDT "maybeMap" "Map and filter a list." (mapMaybe :: (A -> Maybe B) -> [A] -> [B])
            , nameInRootBDT $ valBDT "take" "Take the first n elements." (take :: Int -> [A] -> [A])
            , nameInRootBDT $ valBDT "drop" "Drop the first n elements." (drop :: Int -> [A] -> [A])
            , nameInRootBDT $
              valBDT "takeWhile" "Take while the condition holds." (takeWhile :: (A -> Bool) -> [A] -> [A])
            , nameInRootBDT $
              valBDT "dropWhile" "Drop while the condition holds." (dropWhile :: (A -> Bool) -> [A] -> [A])
            , nameInRootBDT $ valBDT "zip" "Zip two lists." $ zip @A @B
            , nameInRootBDT $ valBDT "sort" "Sort list by an order." (sortBy :: (A -> A -> Ordering) -> [A] -> [A])
            ]
          , namespaceBDT "List1" "" $
            applicativeEntries @_ @NonEmpty <>
            [ valBDT "map" "Map the items of a non-empty list." (fmap :: (A -> B) -> NonEmpty A -> NonEmpty B)
            , nameInRootBDT $ valBDT "<>" "Concatenate a non-empty list with a list." append
            , nameInRootBDT $ valBDT "concat1" "Concatenate a non-empty list of non-empty lists." mconcat1
            , nameInRootBDT $
              valBDT
                  "sort"
                  "Sort non-empty list by an order."
                  (sortBy :: (A -> A -> Ordering) -> NonEmpty A -> NonEmpty A)
            ]
          ]
    , headingBDT
          "Function"
          ""
          [ typeBDT "->" "A pure function." (MkSomeGroundType funcGroundType) []
          , valBDT "$" "Apply a function to a value." $ id @(->) @(A -> B)
          , valBDT ">-" "Apply a value to a function." revap
          , nameInRootBDT $ valBDT "error" "Error." $ ((\t -> error (unpack t)) :: Text -> BottomType)
          , nameInRootBDT $
            valBDT
                "seq"
                "Evaluate the first argument, then if that's not \"bottom\" (error or non-termination), return the second argument."
                (seq :: TopType -> A -> A)
          , nameInRootBDT $
            specialFormBDT "check" "Check from a dynamic supertype." ["@A"] "D(A) -> Maybe A" $
            MkSpecialForm (ConsListType AnnotNegativeType NilListType) $ \(MkSome tn, ()) -> do
                let dtw = getGreatestDynamicSupertype tn
                tpw <- invertType tn
                return $ MkSomeOf (funcShimWit dtw $ maybeShimWit tpw) id
          , nameInRootBDT $
            specialFormBDT "coerce" "Coerce from a dynamic supertype." ["@A"] "D(A) -> A" $
            MkSpecialForm (ConsListType AnnotNegativeType NilListType) $ \(MkSome tn, ()) -> do
                let dtw = getGreatestDynamicSupertype tn
                tpw <- invertType tn
                return $
                    MkSomeOf (funcShimWit dtw tpw) $ \case
                        Just t -> t
                        Nothing ->
                            error $
                            unpack $ toText $ "coercion from " <> exprShow dtw <> " to " <> exprShow tn <> " failed"
          , namespaceBDT "Function" "" $
            monadEntries @_ @((->) P) <>
            [ nameInRootBDT $ valBDT "id" "The identity function." $ id @(->) @A
            , nameInRootBDT $ valBDT "." "Compose functions." $ (.) @(->) @A @B @C
            ]
          ]
    ]
