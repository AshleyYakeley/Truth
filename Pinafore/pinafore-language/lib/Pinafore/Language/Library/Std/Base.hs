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
import Pinafore.Language.DocTree
import Pinafore.Language.ExprShow
import Pinafore.Language.Interpreter
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

-- Showable
showableGroundType :: PinaforeGroundType '[] Showable
showableGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Showable)|]) "Showable"

instance HasPinaforeGroundType '[] Showable where
    pinaforeGroundType = showableGroundType

showableSubtypeRelationEntry ::
       forall a. (HasPinaforeType 'Negative a, TextShow a)
    => DocTreeEntry BindDoc
showableSubtypeRelationEntry = hasSubtypeRelationEntry @a @Showable "" $ functionToShim "textShowable" textShowable

literalSubtypeRelationEntry ::
       forall a. (HasPinaforeType 'Negative a, AsLiteral a)
    => DocTreeEntry BindDoc
literalSubtypeRelationEntry = hasSubtypeRelationEntry @a @Literal "" $ functionToShim "toLiteral" toLiteral

entityAnchor :: Entity -> Text
entityAnchor p = pack $ show p

zeroTime :: UTCTime
zeroTime = UTCTime (fromGregorian 2000 1 1) 0

newClock :: NominalDiffTime -> PinaforeAction (PinaforeImmutableWholeRef UTCTime)
newClock duration = do
    (clockOM, ()) <- actionLiftLifeCycle $ makeSharedModel $ clockPremodel zeroTime duration
    return $ functionImmutableRef $ MkWModel $ clockOM

newTimeZoneRef :: PinaforeImmutableWholeRef UTCTime -> PinaforeAction (PinaforeImmutableWholeRef Int)
newTimeZoneRef now = do
    ref <-
        pinaforeFloatMapReadOnly
            (floatLift (\mr ReadWhole -> fmap (fromKnow zeroTime) $ mr ReadWhole) liftROWChangeLens clockTimeZoneLens) $
        immutableRefToReadOnlyRef now
    return $ fmap timeZoneMinutes $ MkPinaforeImmutableWholeRef ref

interpretAsText ::
       forall a. (Read a, TextShow a)
    => LangWholeRef '( a, a)
    -> LangWholeRef '( Text, Text)
interpretAsText = let
    getter :: Maybe a -> Maybe Text
    getter Nothing = Just ""
    getter (Just a) = Just $ textShow a
    setter :: Maybe Text -> Maybe a -> Maybe (Maybe a)
    setter Nothing _ = Just Nothing
    setter (Just "") _ = Just Nothing
    setter (Just t) _ = fmap Just $ textReadMaybe t
    in maybeLensLangWholeRef getter setter

textReadMaybe :: Read t => Text -> Maybe t
textReadMaybe t = readMaybe $ unpack t

plainFormattingDefs ::
       forall t. (HasPinaforeType 'Positive t, HasPinaforeType 'Negative t, Read t, TextShow t)
    => Text
    -> Text
    -> [DocTreeEntry BindDoc]
plainFormattingDefs uname lname =
    [ mkValEntry (MkName $ "parse" <> uname) ("Parse text as " <> plainMarkdown lname <> ". Inverse of `show`.") $
      textReadMaybe @t
    , mkValEntry
          (MkName $ "interpret" <> uname <> "AsText")
          ("Interpret " <> plainMarkdown lname <> " reference as text, interpreting deleted values as empty text.") $
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
    -> LangWholeRef '( a, a)
    -> LangWholeRef '( Text, Text)
unixInterpretAsText fmt = let
    getter :: Maybe a -> Maybe Text
    getter Nothing = Just ""
    getter (Just a) = Just $ unixFormat fmt a
    setter :: Maybe Text -> Maybe a -> Maybe (Maybe a)
    setter Nothing _ = Just Nothing
    setter (Just "") _ = Just Nothing
    setter (Just t) _ = fmap Just $ unixParse fmt t
    in maybeLensLangWholeRef getter setter

unixFormattingDefs ::
       forall t. (HasPinaforeType 'Positive t, HasPinaforeType 'Negative t, FormatTime t, ParseTime t)
    => Text
    -> Text
    -> [DocTreeEntry BindDoc]
unixFormattingDefs uname lname =
    [ mkValEntry
          (MkName $ "unixFormat" <> uname)
          ("Format " <> plainMarkdown lname <> " as text, using a UNIX-style formatting string.") $
      unixFormat @t
    , mkValEntry
          (MkName $ "unixParse" <> uname)
          ("Parse text as " <> plainMarkdown lname <> ", using a UNIX-style formatting string.") $
      unixParse @t
    , mkValEntry
          (MkName $ "unixInterpret" <> uname <> "AsText")
          ("Interpret " <> plainMarkdown lname <> " reference as text, interpreting deleted values as empty text.") $
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

baseLibEntries :: [DocTreeEntry BindDoc]
baseLibEntries =
    [ docTreeEntry
          "Literals & Entities"
          ""
          [ mkTypeEntry "Entity" "" $ MkBoundType entityGroundType
          , mkValEntry "==" "Entity equality." $ (==) @Entity
          , mkValEntry "/=" "Entity non-equality." $ (/=) @Entity
          , mkValEntry "entityAnchor" "The anchor of an entity, as text." entityAnchor
          , mkTypeEntry "Literal" "" $ MkBoundType literalGroundType
          , hasSubtypeRelationEntry @Literal @Entity "" $ functionToShim "literalToEntity" literalToEntity
          , docTreeEntry
                "Showable"
                ""
                [ mkTypeEntry "Showable" "Something that can be represented as `Text`." $ MkBoundType showableGroundType
                , mkValEntry "show" "Show something as `Text`" $ textShow @Showable
                ]
          , docTreeEntry
                "Unit"
                ""
                [ mkTypeEntry "Unit" "" $ MkBoundType unitGroundType
                , literalSubtypeRelationEntry @()
                , showableSubtypeRelationEntry @()
                ]
          , docTreeEntry
                "Boolean"
                ""
                [ mkTypeEntry "Boolean" "" $ MkBoundType booleanGroundType
                , literalSubtypeRelationEntry @Bool
                , showableSubtypeRelationEntry @Bool
                , mkValPatEntry "True" "Boolean TRUE." True $ \v ->
                      if v
                          then Just ()
                          else Nothing
                , mkValPatEntry "False" "Boolean FALSE." False $ \v ->
                      if v
                          then Nothing
                          else Just ()
                , mkValEntry "&&" "Boolean AND." (&&)
                , mkValEntry "||" "Boolean OR." (||)
                , mkValEntry "not" "Boolean NOT." not
                ]
          , docTreeEntry
                "Ordering"
                ""
                [ mkTypeEntry "Ordering" "" $ MkBoundType orderingGroundType
                , literalSubtypeRelationEntry @Ordering
                , showableSubtypeRelationEntry @Ordering
                , mkValPatEntry "LT" "Less than." LT $ \v ->
                      case v of
                          LT -> Just ()
                          _ -> Nothing
                , mkValPatEntry "EQ" "Equal to." EQ $ \v ->
                      case v of
                          EQ -> Just ()
                          _ -> Nothing
                , mkValPatEntry "GT" "Greater than." GT $ \v ->
                      case v of
                          GT -> Just ()
                          _ -> Nothing
                , mkValEntry "eq" "Equal." $ (==) EQ
                , mkValEntry "ne" "Not equal." $ (/=) EQ
                , mkValEntry "lt" "Less than." $ (==) LT
                , mkValEntry "le" "Less than or equal to." $ (/=) GT
                , mkValEntry "gt" "Greater than." $ (==) GT
                , mkValEntry "ge" "Greater than or equal to." $ (/=) LT
                , mkValEntry "lesser" "The lesser of two weevils." lesser
                , mkValEntry "greater" "The greater of two weevils." greater
                , mkValEntry
                      "alphabetical"
                      "Alphabetical first, then lower case before upper, per Unicode normalisation." $
                  Text.Collate.collate Text.Collate.rootCollator
                , mkValEntry "numerical" "Numercal order." $ compare @Number
                , mkValEntry "chronological" "Chronological order." $ compare @UTCTime
                , mkValEntry "durational" "Durational order." $ compare @NominalDiffTime
                , mkValEntry "calendrical" "Date order." $ compare @Day
                , mkValEntry "horological" "Time of day order." $ compare @TimeOfDay
                , mkValEntry "localChronological" "Local time order." $ compare @LocalTime
                , mkValEntry "noOrder" "No order, everything EQ." $ noOrder @TopType
                , mkValEntry "orders" "Join orders by priority." $ concatOrders @A
                , mkValEntry "reverse" "Reverse an order." $ reverseOrder @A
                , mkValEntry "sort" "Sort by an order." (sortBy :: (A -> A -> Ordering) -> [A] -> [A])
                ]
          , docTreeEntry
                "Text"
                ""
                [ mkTypeEntry "Text" "" $ MkBoundType textGroundType
                , literalSubtypeRelationEntry @Text
                , showableSubtypeRelationEntry @Text
                , mkValEntry "<>" "Concatenate text." $ (<>) @Text
                , mkValEntry "textLength" "The length of a piece of text." $ olength @Text
                , mkValEntry
                      "textSection"
                      "`textSection start len text` is the section of `text` beginning at `start` of length `len`." $ \start len (text :: Text) ->
                      take len $ drop start text
                , mkValEntry "textConcat" "Concatenate texts." $ mconcat @Text
                , mkValEntry "toUpperCase" "" Data.Text.toUpper
                , mkValEntry "toLowerCase" "" Data.Text.toLower
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
                in docTreeEntry
                       "Numeric"
                       ""
                       [ mkValEntry "~==" "Numeric equality, folding exact and inexact numbers." $ (==) @Number
                       , mkValEntry "~/=" "Numeric non-equality." $ (/=) @Number
                       , mkValEntry "<" "Numeric strictly less." $ (<) @Number
                       , mkValEntry "<=" "Numeric less or equal." $ (<=) @Number
                       , mkValEntry ">" "Numeric strictly greater." $ (>) @Number
                       , mkValEntry ">=" "Numeric greater or equal." $ (>=) @Number
                       , docTreeEntry "Integer" "" $
                         [ mkTypeEntry "Integer" "" $ MkBoundType integerGroundType
                         , hasSubtypeRelationEntry @Integer @SafeRational "" $
                           functionToShim "integerSafeRational" $ encode integerSafeRational
                         ] <>
                         plainFormattingDefs @Integer "Integer" "an integer" <>
                         [ mkValEntry "min" "Lesser of two Integers" $ min @Integer
                         , mkValEntry "max" "Greater of two Integers" $ max @Integer
                         , mkValEntry "+" "Add." $ (+) @Integer
                         , mkValEntry "-" "Subtract." $ (-) @Integer
                         , mkValEntry "*" "Multiply." $ (*) @Integer
                         , mkValEntry "negate" "Negate." $ negate @Integer
                         , mkValEntry "abs" "Absolute value." $ abs @Integer
                         , mkValEntry "signum" "Sign." $ signum @Integer
                         , mkValEntry "mod" "Modulus, leftover from `div`" $ mod' @Integer
                         , mkValEntry "even" "Is even?" $ even @Integer
                         , mkValEntry "odd" "Is odd?" $ odd @Integer
                         , mkValEntry "gcd" "Greatest common divisor." $ gcd @Integer
                         , mkValEntry "lcm" "Least common multiple." $ lcm @Integer
                         , mkValEntry "^" "Raise to non-negative power." $ (^) @Integer @Integer
                         , mkValEntry "sum" "Sum." $ sum @[] @Integer
                         , mkValEntry "product" "Product." $ product @[] @Integer
                         , mkValEntry
                               "range"
                               "`range a b` is an arithmetic sequence starting from `a`, with all numbers `<= b`. Step is +1." $
                           range @Integer
                         , mkValEntry
                               "arithList"
                               "`arithList step a (Just b)` is an arithmetic sequence starting from `a`, with all numbers `<= b` (for positive step) or `>= b` (for negative step).\n\n\
                                \`arithList step a Nothing` is an infinite arithmetic sequence starting from `a`." $
                           arithList @Integer
                         ]
                       , docTreeEntry "Rational" "" $
                         [ mkTypeEntry "Rational" "" $ MkBoundType rationalGroundType
                         , hasSubtypeRelationEntry @SafeRational @Number "" $
                           functionToShim "safeRationalNumber" $ encode safeRationalNumber
                         ] <>
                         plainFormattingDefs @SafeRational "Rational" "a rational" <>
                         [ mkValEntry "minR" "Lesser of two Rationals" $ min @SafeRational
                         , mkValEntry "maxR" "Greater of two Rationals" $ max @SafeRational
                         , mkValEntry ".+" "Add." $ (+) @SafeRational
                         , mkValEntry ".-" "Subtract." $ (-) @SafeRational
                         , mkValEntry ".*" "Multiply." $ (*) @SafeRational
                         , mkValEntry "/" "Divide." $ (/) @SafeRational
                         , mkValEntry "negateR" "Negate." $ negate @SafeRational
                         , mkValEntry "recip" "Reciprocal." $ recip @SafeRational
                         , mkValEntry "absR" "Absolute value." $ abs @SafeRational
                         , mkValEntry "signumR" "Sign." $ signum @SafeRational
                         , mkValEntry "modR" "Modulus, leftover from `div`" $ mod' @SafeRational
                         , mkValEntry "^^" "Raise to Integer power." $ ((^^) :: SafeRational -> Integer -> SafeRational)
                         , mkValEntry "sumR" "Sum." $ sum @[] @SafeRational
                         , mkValEntry "meanR" "Mean." $ \(vv :: [SafeRational]) -> sum vv / toSafeRational (length vv)
                         , mkValEntry "productR" "Product." $ product @[] @SafeRational
                         ]
                       , docTreeEntry "Number" "" $
                         plainFormattingDefs @Number "Number" "a number" <>
                         [ mkTypeEntry "Number" "" $ MkBoundType numberGroundType
                         , literalSubtypeRelationEntry @Number
                         , showableSubtypeRelationEntry @Number
                         , mkValEntry "minN" "Lesser of two Numbers" $ min @Number
                         , mkValEntry "maxN" "Greater of two Numbers" $ max @Number
                         , mkValEntry "~+" "Add." $ (+) @Number
                         , mkValEntry "~-" "Subtract." $ (-) @Number
                         , mkValEntry "~*" "Multiply." $ (*) @Number
                         , mkValEntry "~/" "Divide." $ (/) @Number
                         , mkValEntry "negateN" "Negate." $ negate @Number
                         , mkValEntry "recipN" "Reciprocal." $ recip @Number
                         , mkValEntry "pi" "Half the radians in a circle." $ pi @Number
                         , mkValEntry "exp" "Exponent" $ exp @Number
                         , mkValEntry "log" "Natural logarithm" $ log @Number
                         , mkValEntry "sqrt" "Square root." $ sqrt @Number
                         , mkValEntry "**" "Raise to power." $ (**) @Number
                         , mkValEntry "logBase" "" $ logBase @Number
                         , mkValEntry "sin" "Sine of an angle in radians." $ sin @Number
                         , mkValEntry "cos" "Cosine of an angle in radians." $ cos @Number
                         , mkValEntry "tan" "Tangent of an angle in radians." $ tan @Number
                         , mkValEntry "asin" "Radian angle of a sine." $ asin @Number
                         , mkValEntry "acos" "Radian angle of a cosine." $ acos @Number
                         , mkValEntry "atan" "Radian angle of a tangent." $ atan @Number
                         , mkValEntry "sinh" "Hyperbolic sine." $ sinh @Number
                         , mkValEntry "cosh" "Hyperbolic cosine." $ cosh @Number
                         , mkValEntry "tanh" "Hyperbolic tangent." $ tanh @Number
                         , mkValEntry "asinh" "Inverse hyperbolic sine." $ asinh @Number
                         , mkValEntry "acosh" "Inverse hyperbolic cosine." $ acosh @Number
                         , mkValEntry "atanh" "Inverse hyperbolic tangent." $ atanh @Number
                         , mkValEntry "nabs" "Absolute value." $ abs @Number
                         , mkValEntry "signumN" "Sign. Note this will be the same exact or inexact as the number." $
                           signum @Number
                         , mkValEntry "floor" "Integer towards negative infinity." (floor :: Number -> Integer)
                         , mkValEntry "ceiling" "Integer towards positive infinity." (ceiling :: Number -> Integer)
                         , mkValEntry "round" "Closest Integer." (round :: Number -> Integer)
                         , mkValEntry "inexact" "Convert a number to inexact." numberToDouble
                         , mkValEntry
                               "approximate"
                               "`approximate d x` gives the exact number that's a multiple of `d` that's closest to `x`."
                               approximate
                         , mkValEntry
                               "div"
                               "Division to Integer, towards negative infinity."
                               (div' :: Number -> Number -> Integer)
                         , mkValEntry "modN" "Modulus, leftover from `div`" $ mod' @Number
                         , mkValEntry "isNaN" "Is not a number?" numberIsNaN
                         , mkValEntry "isInfinite" "Is infinite?" numberIsInfinite
                         , mkValEntry "isNegativeZero" "Is negative zero?" numberIsNegativeZero
                         , mkValEntry "isExact" "Is exact?" numberIsExact
                         , mkValEntry "sumN" "Sum." $ sum @[] @Number
                         , mkValEntry "meanN" "Mean." $ \(vv :: [Number]) ->
                               sum vv / (ExactNumber $ toRational $ length vv)
                         , mkValEntry "productN" "Product." $ product @[] @Number
                         , mkValEntry "numberCheckSafeRational" "Get the exact value of a Number, if it is one." $
                           decode safeRationalNumber
                         , mkValEntry
                               "checkExactInteger"
                               "Get the exact Integer value of a Number, if it is one. Works as expected on Rationals." $
                           decode $ integerSafeRational . safeRationalNumber
                         ]
                       ]
          , docTreeEntry
                "Date & Time"
                ""
                [ docTreeEntry "Duration" "" $
                  [ mkTypeEntry "Duration" "" $ MkBoundType durationGroundType
                  , literalSubtypeRelationEntry @NominalDiffTime
                  , showableSubtypeRelationEntry @NominalDiffTime
                  , mkValPatEntry "Seconds" "Construct a `Duration` from seconds." secondsToNominalDiffTime $ \d ->
                        Just (nominalDiffTimeToSeconds d, ())
                  ] <>
                  -- plainFormattingDefs @NominalDiffTime "Duration" "a duration" <>
                  [ mkValEntry "zeroDuration" "No duration." $ (0 :: NominalDiffTime)
                  , mkValEntry "dayDuration" "One day duration." nominalDay
                  , mkValEntry "addDuration" "Add durations." $ (+) @NominalDiffTime
                  , mkValEntry "subtractDuration" "Subtract durations." $ (-) @NominalDiffTime
                  , mkValEntry "negateDuration" "Negate duration." $ negate @NominalDiffTime
                  , mkValEntry "multiplyDuration" "Multiply a duration by a number." $ \(n :: Number) (d :: NominalDiffTime) ->
                        (realToFrac n) * d
                  , mkValEntry "divideDuration" "Divide durations." $ \(a :: NominalDiffTime) (b :: NominalDiffTime) ->
                        (realToFrac (a / b) :: Number)
                  ]
                , docTreeEntry "Time" "" $
                  [ mkTypeEntry "Time" "Absolute time as measured by UTC." $ MkBoundType timeGroundType
                  , literalSubtypeRelationEntry @UTCTime
                  , showableSubtypeRelationEntry @UTCTime
                  ] <>
                  plainFormattingDefs @UTCTime "Time" "a time" <>
                  unixFormattingDefs @UTCTime "Time" "a time" <>
                  [ mkValEntry "addTime" "Add duration to time." addUTCTime
                  , mkValEntry "diffTime" "Difference of times." diffUTCTime
                  , mkValEntry "getTime" "Get the current time." $ getCurrentTime
                  , mkValEntry
                        "newClock"
                        "Make a reference to the current time that updates per the given duration."
                        newClock
                  ]
                , docTreeEntry "Calendar" "" $
                  [ mkTypeEntry "Date" "" $ MkBoundType dateGroundType
                  , mkValPatEntry "YearMonthDay" "Construct a `Date` from year, month, day." fromGregorian $ \day -> let
                        (y, m, d) = toGregorian day
                        in Just (y, (m, (d, ())))
                  , mkValPatEntry "ModifiedJulianDay" "Construct a `Date` from its MJD." ModifiedJulianDay $ \day ->
                        Just (toModifiedJulianDay day, ())
                  , literalSubtypeRelationEntry @Day
                  , showableSubtypeRelationEntry @Day
                  ] <>
                  plainFormattingDefs @Day "Date" "a date" <>
                  unixFormattingDefs @Day "Date" "a date" <>
                  [ mkValEntry "addDays" "Add count to days to date." addDays
                  , mkValEntry "diffDays" "Difference of days between dates." diffDays
                  , mkValEntry "getUTCDate" "Get the current UTC date." $ fmap utctDay getCurrentTime
                  , mkValEntry "getDate" "Get the current local date." $ fmap localDay getLocalTime
                  ]
                , docTreeEntry "Time of Day" "" $
                  [ mkTypeEntry "TimeOfDay" "" $ MkBoundType timeOfDayGroundType
                  , mkValPatEntry "HourMinuteSecond" "Construct a `TimeOfDay` from hour, minute, second." TimeOfDay $ \TimeOfDay {..} ->
                        Just (todHour, (todMin, (todSec, ())))
                  , mkValPatEntry
                        "SinceMidnight"
                        "Construct a `TimeOfDay` from duration since midnight (wrapping whole days)."
                        (snd . timeToDaysAndTimeOfDay)
                        (\t -> Just (daysAndTimeOfDayToTime 0 t, ()))
                  , literalSubtypeRelationEntry @TimeOfDay
                  , showableSubtypeRelationEntry @TimeOfDay
                  ] <>
                  plainFormattingDefs @TimeOfDay "TimeOfDay" "a time of day" <>
                  unixFormattingDefs @TimeOfDay "TimeOfDay" "a time of day" <>
                  [mkValEntry "midnight" "Midnight." midnight, mkValEntry "midday" "Midday." midday]
                , docTreeEntry "Local Time" "" $
                  [ mkTypeEntry "LocalTime" "" $ MkBoundType localTimeGroundType
                  , mkValPatEntry "DateAndTime" "Construct a `LocalTime` from day and time of day." LocalTime $ \LocalTime {..} ->
                        Just (localDay, (localTimeOfDay, ()))
                  , literalSubtypeRelationEntry @LocalTime
                  , showableSubtypeRelationEntry @LocalTime
                  ] <>
                  plainFormattingDefs @LocalTime "LocalTime" "a local time" <>
                  unixFormattingDefs @LocalTime "LocalTime" "a local time" <>
                  [ mkValEntry "timeToLocal" "Convert a time to local time, given a time zone offset in minutes" $ \i ->
                        utcToLocalTime $ minutesToTimeZone i
                  , mkValEntry "localToTime" "Convert a local time to time, given a time zone offset in minutes" $ \i ->
                        localTimeToUTC $ minutesToTimeZone i
                  , mkValEntry "getTimeZone" "Get the offset for a time in the current time zone." $ \t ->
                        fmap timeZoneMinutes $ getTimeZone t
                  , mkValEntry "getCurrentTimeZone" "Get the current time zone offset in minutes." $
                    fmap timeZoneMinutes getCurrentTimeZone
                  , mkValEntry "getLocalTime" "Get the current local time." getLocalTime
                  , mkValEntry "newTimeZoneRef" "The current time zone offset in minutes." newTimeZoneRef
                  ]
                ]
          , docTreeEntry
                "Closed Entity Types"
                ""
                [ mkSubtypeRelationEntry "(any closed entity type)" (fst $ pgtShowType entityGroundType) "" $
                  MkSubtypeConversionEntry entityGroundType $ \gta@MkPinaforeGroundType {..} -> do
                      MkClosedEntityFamily _ (MkSealedEntityProperties eprops) <-
                          matchFamilyType closedEntityFamilyWitness pgtFamilyType
                      Refl <- testEquality (covaryToDolanVarianceType $ epKind eprops) pgtVarianceType
                      return $
                          entityPropertiesSaturatedAdapter
                              (groundedDolanShimWit entityGroundType nilDolanArgumentsShimWit)
                              plainEntityAdapter
                              eprops $ \args eat ->
                              subtypeConversion gta args entityGroundType nilDolanArgumentsShimWit $
                              pure $ functionToShim "ClosedEntity" $ entityAdapterConvert eat
                ]
          , docTreeEntry
                "Open Entity Types"
                ""
                [ mkSubtypeRelationEntry "(any open entity type)" (fst $ pgtShowType entityGroundType) "" $
                  MkSubtypeConversionEntry entityGroundType $ \MkPinaforeGroundType {..} -> do
                      Refl <- testEquality pgtVarianceType NilListType
                      MkLiftedFamily _ <- matchFamilyType openEntityFamilyWitness pgtFamilyType
                      return $ nilSubtypeConversion $ coerceShim "OpenEntity"
                , mkSpecialFormEntry
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
                , mkSpecialFormEntry
                      "newOpenEntity"
                      "Generate an open entity. `A` is an open entity type."
                      ["@A"]
                      "Action A" $
                  MkSpecialForm (ConsListType AnnotPositiveType NilListType) $ \(t, ()) -> do
                      mtp <- getOpenEntityType t
                      return $
                          case mtp of
                              MkSome (tp :: OpenEntityType tid) -> let
                                  pt :: PinaforeAction (OpenEntity tid)
                                  pt = liftIO $ newKeyContainerItem @(FiniteSet (OpenEntity tid))
                                  typef = actionShimWit $ openEntityShimWit tp
                                  in MkSomeOf typef pt
                ]
          , docTreeEntry
                "Dynamic Entity Types"
                ""
                [ mkTypeEntry "DynamicEntity" "" $ MkBoundType dynamicEntityGroundType
                , mkSubtypeRelationEntry "(any dynamic entity type)" (fst $ pgtShowType dynamicEntityGroundType) "" $
                  MkSubtypeConversionEntry dynamicEntityGroundType $ \MkPinaforeGroundType {..} -> do
                      Refl <- testEquality pgtVarianceType NilListType
                      MkADynamicEntityFamily _ _ <- matchFamilyType aDynamicEntityFamilyWitness pgtFamilyType
                      return $ nilSubtypeConversion id
                , hasSubtypeRelationEntry @DynamicEntity @Entity "" $
                  functionToShim "dynamicEntityAdapter" $ entityAdapterConvert $ dynamicEntityAdapter Nothing
                , mkSpecialFormEntry
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
                , mkSpecialFormEntry
                      "newDynamicEntity"
                      "Generate a dynamic entity. `A` is a concrete dynamic entity type."
                      ["@A"]
                      "Action A" $
                  MkSpecialForm (ConsListType AnnotPositiveType NilListType) $ \(t, ()) -> do
                      (n, dt) <- getConcreteDynamicEntityType t
                      let
                          pt :: PinaforeAction DynamicEntity
                          pt =
                              liftIO $ do
                                  e <- newKeyContainerItem @(FiniteSet Entity)
                                  return $ MkDynamicEntity dt e
                          typef = actionShimWit $ dynamicEntityShimWit n dt
                      return $ MkSomeOf typef pt
                ]
          ]
    , docTreeEntry
          "Maybe"
          ""
          [ mkTypeEntry "Maybe" "" $ MkBoundType maybeGroundType
          , mkValPatEntry "Just" "Construct a Maybe from a value." (Just @A) $ \(v :: Maybe A) ->
                case v of
                    Just a -> Just (a, ())
                    _ -> Nothing
          , mkValPatEntry "Nothing" "Construct a Maybe without a value." (Nothing @BottomType) $ \(v :: Maybe A) ->
                case v of
                    Nothing -> Just ()
                    _ -> Nothing
          , hasSubtypeRelationEntry @(Maybe Entity) @Entity "" $ functionToShim "maybeEntityConvert" maybeEntityConvert
          , hasSubtypeRelationEntry @(Maybe Showable) @Showable "" $ functionToShim "show" textShowable
          ]
    , docTreeEntry
          "*:"
          ""
          [ mkTypeEntry "*:" "" $ MkBoundType pairGroundType
          , hasSubtypeRelationEntry @(Entity, Entity) @Entity "" $ functionToShim "pairEntityConvert" pairEntityConvert
          , hasSubtypeRelationEntry @(Showable, Showable) @Showable "" $ functionToShim "show" textShowable
          , mkValEntry "fst" "Get the first member of a pair." $ fst @A @B
          , mkValEntry "snd" "Get the second member of a pair." $ snd @A @B
          , mkValEntry "toPair" "Construct a pair." $ (,) @A @B
          , mkValEntry "pair" "Construct a pair." $ \(a :: A) -> (a, a)
          ]
    , docTreeEntry
          "+:"
          ""
          [ mkTypeEntry "+:" "" $ MkBoundType eitherGroundType
          , mkValPatEntry "Left" "Construct an Either from the left." (Left @A @B) $ \(v :: Either A B) ->
                case v of
                    Left a -> Just (a, ())
                    _ -> Nothing
          , mkValPatEntry "Right" "Construct an Either from the right." (Right @A @B) $ \(v :: Either A B) ->
                case v of
                    Right a -> Just (a, ())
                    _ -> Nothing
          , hasSubtypeRelationEntry @(Either Entity Entity) @Entity "" $
            functionToShim "eitherEntityConvert" eitherEntityConvert
          , hasSubtypeRelationEntry @(Either Showable Showable) @Showable "" $ functionToShim "show" textShowable
          , mkValEntry "fromEither" "Eliminate a sum" $ either @A @C @B
          , mkValEntry "either" "Eliminate a sum" $ \(v :: Either A A) ->
                case v of
                    Left a -> a
                    Right a -> a
          ]
    , docTreeEntry
          "Lists"
          ""
          [ mkTypeEntry "List" "A list." $ MkBoundType listGroundType
          , mkTypeEntry "List1" "A list with at least one element." $ MkBoundType list1GroundType
          , hasSubtypeRelationEntry @(NonEmpty A) @[A] "" $ functionToShim "NonEmpty.toList" toList
          , mkValPatEntry "[]" "Empty list" ([] @BottomType) $ \(v :: [A]) ->
                case v of
                    [] -> Just ()
                    _ -> Nothing
          , mkValPatEntry "::" "Construct a list" ((:|) @A) $ \(v :: [A]) ->
                case v of
                    a:b -> Just (a, (b, ()))
                    _ -> Nothing
          , hasSubtypeRelationEntry @[Entity] @Entity "" $ functionToShim "listEntityConvert" listEntityConvert
          , hasSubtypeRelationEntry @[Showable] @Showable "" $ functionToShim "show" textShowable
          , mkValEntry "list" "Eliminate a list" $ \(fnil :: B) fcons (l :: [A]) ->
                case l of
                    [] -> fnil
                    (a:aa) -> fcons a aa
          , mkValEntry "length" "Number of items in a list" (length :: [TopType] -> Int)
          , mkValEntry "index" "Get item from list by index." (index :: [A] -> Int -> Maybe A)
          , mkValEntry "mapList" "Map the items of a list." (fmap :: (A -> B) -> [A] -> [B])
          , mkValEntry "++" "Concatentate lists." ((++) :: [A] -> [A] -> [A])
          , mkValEntry "filter" "Filter a list." (filter :: (A -> Bool) -> [A] -> [A])
          , mkValEntry "maybeMapList" "Map and filter a list." (mapMaybe :: (A -> Maybe B) -> [A] -> [B])
          , mkValEntry "take" "Take the first n elements." (take :: Int -> [A] -> [A])
          , mkValEntry "drop" "Drop the first n elements." (drop :: Int -> [A] -> [A])
          , mkValEntry "takeWhile" "Take while the condition holds." (takeWhile :: (A -> Bool) -> [A] -> [A])
          , mkValEntry "dropWhile" "Drop while the condition holds." (dropWhile :: (A -> Bool) -> [A] -> [A])
          , mkValEntry "zip" "Zip two lists." $ zip @A @B
          ]
    , docTreeEntry
          "Functions"
          ""
          [ mkTypeEntry "->" "A pure function." $ MkBoundType funcGroundType
          , mkValEntry "id" "The identity function." $ id @(->) @A
          , mkValEntry "$" "Apply a function to a value." $ id @(->) @(A -> B)
          , mkValEntry "." "Compose functions." $ (.) @(->) @A @B @C
          , mkValEntry "error" "Error." $ ((\t -> error (unpack t)) :: Text -> BottomType)
          , mkValEntry
                "seq"
                "Evaluate the first argument, then if that's not \"bottom\" (error or non-termination), return the second argument."
                (seq :: TopType -> A -> A)
          , mkSpecialFormEntry "check" "Check from a dynamic supertype." ["@A"] "D(A) -> Maybe A" $
            MkSpecialForm (ConsListType AnnotNegativeType NilListType) $ \(MkSome tn, ()) -> do
                let dtw = getGreatestDynamicSupertype tn
                tpw <- invertType tn
                return $ MkSomeOf (funcShimWit dtw $ maybeShimWit tpw) id
          , mkSpecialFormEntry "coerce" "Coerce from a dynamic supertype." ["@A"] "D(A) -> A" $
            MkSpecialForm (ConsListType AnnotNegativeType NilListType) $ \(MkSome tn, ()) -> do
                let dtw = getGreatestDynamicSupertype tn
                tpw <- invertType tn
                return $
                    MkSomeOf (funcShimWit dtw tpw) $ \case
                        Just t -> t
                        Nothing ->
                            error $ unpack $ "coercion from " <> exprShow dtw <> " to " <> exprShow tn <> " failed"
          ]
    ]
