{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Library.Std.Base
    ( baseLibEntries
    , outputLn
    ) where

import Changes.Core
import Changes.World.Clock
import Data.Time
import Data.Time.Clock.System
import Pinafore.Base
import Pinafore.Context
import Pinafore.Language.Convert
import Pinafore.Language.DocTree
import Pinafore.Language.ExprShow
import Pinafore.Language.If
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var
import Shapes
import Shapes.Numeric

openEntityShimWit :: forall tid. OpenEntityType tid -> PinaforeShimWit 'Positive (OpenEntity tid)
openEntityShimWit tp =
    singleDolanShimWit $
    mkShimWit $
    GroundDolanSingularType (EntityPinaforeGroundType NilListType $ OpenEntityGroundType tp) NilDolanArguments

dynamicEntityShimWit :: Name -> DynamicType -> PinaforeShimWit 'Positive DynamicEntity
dynamicEntityShimWit n dt =
    singleDolanShimWit $
    mkShimWit $
    GroundDolanSingularType
        (EntityPinaforeGroundType NilListType $ ADynamicEntityGroundType n $ singletonSet dt)
        NilDolanArguments

textShimWit ::
       forall polarity. Is PolarityType polarity
    => PinaforeShimWit polarity Text
textShimWit =
    singleDolanShimWit $
    mkShimWit $
    GroundDolanSingularType
        (EntityPinaforeGroundType NilListType $ LiteralEntityGroundType TextLiteralType)
        NilDolanArguments

maybeShimWit :: forall a. PinaforeShimWit 'Positive a -> PinaforeShimWit 'Positive (Maybe a)
maybeShimWit swa =
    unPosShimWit swa $ \ta conva ->
        mapPosShimWit (applyCoPolyShim cid conva) $
        singleDolanShimWit $
        mkShimWit $
        GroundDolanSingularType (EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType) $
        ConsDolanArguments ta NilDolanArguments

eitherShimWit ::
       forall a b. PinaforeShimWit 'Positive a -> PinaforeShimWit 'Positive b -> PinaforeShimWit 'Positive (Either a b)
eitherShimWit swa swb =
    unPosShimWit swa $ \ta conva ->
        unPosShimWit swb $ \tb convb ->
            mapPosShimWit (applyCoPolyShim (cfmap conva) convb) $
            singleDolanShimWit $
            mkShimWit $
            GroundDolanSingularType
                (EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) EitherEntityGroundType) $
            ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

funcShimWit ::
       forall a b. PinaforeShimWit 'Negative a -> PinaforeShimWit 'Positive b -> PinaforeShimWit 'Positive (a -> b)
funcShimWit swa swb =
    unNegShimWit swa $ \ta conva ->
        unPosShimWit swb $ \tb convb ->
            mapPosShimWit (applyCoPolyShim (ccontramap conva) convb) $
            singleDolanShimWit $
            mkShimWit $
            GroundDolanSingularType funcGroundType $ ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

actionShimWit :: forall a. PinaforeShimWit 'Positive a -> PinaforeShimWit 'Positive (PinaforeAction a)
actionShimWit swa =
    unPosShimWit swa $ \ta conva ->
        mapPosShimWit (cfmap conva) $
        singleDolanShimWit $
        mkShimWit $ GroundDolanSingularType actionGroundType $ ConsDolanArguments ta NilDolanArguments

getTimeMS :: IO Integer
getTimeMS = do
    MkSystemTime s ns <- getSystemTime
    return $ (toInteger s) * 1000 + div (toInteger ns) 1000000

output :: (?pinafore :: PinaforeContext) => Text -> PinaforeAction ()
output text = liftIO $ hPutStrLn pinaforeStdOut $ unpack text

outputLn :: (?pinafore :: PinaforeContext) => Text -> PinaforeAction ()
outputLn text = liftIO $ hPutStrLn pinaforeStdOut $ unpack text

qfail :: Text -> PinaforeAction BottomType
qfail t = fail $ unpack t

entityAnchor :: Entity -> Text
entityAnchor p = pack $ show p

onStop :: PinaforeAction A -> PinaforeAction A -> PinaforeAction A
onStop p q = p <|> q

zeroTime :: UTCTime
zeroTime = UTCTime (fromGregorian 2000 1 1) 0

newClock :: NominalDiffTime -> PinaforeAction (PinaforeImmutableWholeRef UTCTime)
newClock duration = do
    (clockOM, ()) <- liftLifeCycle $ makeSharedModel $ clockPremodel zeroTime duration
    return $ functionImmutableRef $ MkWModel $ clockOM

newTimeZoneRef :: PinaforeImmutableWholeRef UTCTime -> PinaforeAction (PinaforeImmutableWholeRef Int)
newTimeZoneRef now = do
    ref <-
        pinaforeFloatMapReadOnly
            (floatLift (\mr ReadWhole -> fmap (fromKnow zeroTime) $ mr ReadWhole) liftROWChangeLens clockTimeZoneLens) $
        immutableRefToReadOnlyRef now
    return $ fmap timeZoneMinutes $ MkPinaforeImmutableWholeRef ref

interpretAsText ::
       forall a. AsLiteral a
    => LangWholeRef '( a, a)
    -> LangWholeRef '( Text, Text)
interpretAsText = let
    getter :: Maybe a -> Maybe Text
    getter Nothing = Just ""
    getter (Just a) = Just $ unLiteral $ toLiteral a
    setter :: Maybe Text -> Maybe a -> Maybe (Maybe a)
    setter Nothing _ = Just Nothing
    setter (Just "") _ = Just Nothing
    setter (Just t) _ = fmap Just $ parseLiteral t
    in maybeLensLangWholeRef getter setter

parseLiteral :: AsLiteral t => Text -> Maybe t
parseLiteral = knowToMaybe . fromLiteral . MkLiteral

plainFormattingDefs ::
       forall t. (ToPinaforeType t, FromPinaforeType t, AsLiteral t)
    => Text
    -> Text
    -> [DocTreeEntry BindDoc]
plainFormattingDefs uname lname =
    [ mkValEntry (MkName $ "parse" <> uname) ("Parse text as " <> lname <> ". Inverse of `toText`.") $ parseLiteral @t
    , mkValEntry
          (MkName $ "interpret" <> uname <> "AsText")
          ("Interpret " <> lname <> " reference as text, interpreting deleted values as empty text.") $
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
       forall t. (ToPinaforeType t, FromPinaforeType t, FormatTime t, ParseTime t)
    => Text
    -> Text
    -> [DocTreeEntry BindDoc]
unixFormattingDefs uname lname =
    [ mkValEntry
          (MkName $ "unixFormat" <> uname)
          ("Format " <> lname <> " as text, using a UNIX-style formatting string.") $
      unixFormat @t
    , mkValEntry
          (MkName $ "unixParse" <> uname)
          ("Parse text as " <> lname <> ", using a UNIX-style formatting string.") $
      unixParse @t
    , mkValEntry
          (MkName $ "unixInterpret" <> uname <> "AsText")
          ("Interpret " <> lname <> " reference as text, interpreting deleted values as empty text.") $
      unixInterpretAsText @t
    ]

getLocalTime :: IO LocalTime
getLocalTime = fmap zonedTimeToLocalTime getZonedTime

getEnv :: (?pinafore :: PinaforeContext) => Text -> Maybe Text
getEnv n = fmap pack $ lookup (unpack n) $ iiEnvironment pinaforeInvocationInfo

topEntityType :: forall pol. PinaforeType pol (JoinMeetType pol Entity (LimitType pol))
topEntityType =
    ConsDolanType
        (GroundDolanSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments)
        NilDolanType

literalSubtypeConversionEntry ::
       LiteralType a -> LiteralType b -> PinaforePolyShim Type a b -> SubypeConversionEntry PinaforeGroundType
literalSubtypeConversionEntry ta tb conv =
    simpleSubtypeConversionEntry
        (EntityPinaforeGroundType NilListType (LiteralEntityGroundType ta))
        (EntityPinaforeGroundType NilListType (LiteralEntityGroundType tb)) $
    nilSubtypeConversion conv

baseLibEntries :: [DocTreeEntry BindDoc]
baseLibEntries =
    [ docTreeEntry
          "Literals & Entities"
          ""
          [ mkTypeEntry "Entity" "" $ MkBoundType $ EntityPinaforeGroundType NilListType TopEntityGroundType
          , mkValEntry "==" "Entity equality." $ (==) @Entity
          , mkValEntry "/=" "Entity non-equality." $ (/=) @Entity
          , mkValEntry "entityAnchor" "The anchor of an entity, as text." entityAnchor
          , mkTypeEntry "Literal" "" $
            MkBoundType $ EntityPinaforeGroundType NilListType $ LiteralEntityGroundType LiteralLiteralType
          , mkSubtypeRelationEntry "Literal" "Entity" "" $
            pure $
            MkSubypeConversionEntry (EntityPinaforeGroundType NilListType TopEntityGroundType) $ \case
                EntityPinaforeGroundType NilListType t -> Just $ nilSubtypeConversion $ entitySubtypeShim t
                _ -> Nothing
          , mkValEntry "toText" "The text of a literal." unLiteral
          , docTreeEntry
                "Boolean"
                ""
                [ mkTypeEntry "Boolean" "" $
                  MkBoundType $ EntityPinaforeGroundType NilListType $ LiteralEntityGroundType BooleanLiteralType
                , mkSubtypeRelationEntry "Boolean" "Literal" "" $
                  pure $
                  MkSubypeConversionEntry
                      (EntityPinaforeGroundType NilListType (LiteralEntityGroundType LiteralLiteralType)) $ \case
                      EntityPinaforeGroundType NilListType (LiteralEntityGroundType t) ->
                          case literalTypeAsLiteral t of
                              Dict -> Just $ nilSubtypeConversion $ functionToShim "literal to Literal" toLiteral
                      _ -> Nothing
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
                [ mkTypeEntry "Ordering" "" $
                  MkBoundType $ EntityPinaforeGroundType NilListType $ LiteralEntityGroundType OrderingLiteralType
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
                , mkValEntry "alphabetical" "Alphabetical order." $ compare @Text
                , mkValEntry "numerical" "Numercal order." $ compare @Number
                , mkValEntry "chronological" "Chronological order." $ compare @UTCTime
                , mkValEntry "durational" "Durational order." $ compare @NominalDiffTime
                , mkValEntry "calendrical" "Date order." $ compare @Day
                , mkValEntry "horological" "Time of day order." $ compare @TimeOfDay
                , mkValEntry "localChronological" "Local time order." $ compare @LocalTime
                , mkValEntry "noOrder" "No order, everything EQ." $ noOrder @TopType
                , mkValEntry "orders" "Join orders by priority." $ concatOrders @A
                , mkValEntry "reverse" "Reverse an order." $ reverseOrder @A
                ]
          , docTreeEntry
                "Text"
                ""
                [ mkTypeEntry "Text" "" $
                  MkBoundType $ EntityPinaforeGroundType NilListType $ LiteralEntityGroundType TextLiteralType
                , mkSubtypeRelationEntry "Text" "Literal" "" []
                , mkValEntry "<>" "Concatenate text." $ (<>) @Text
                , mkValEntry "textLength" "The length of a piece of text." $ olength @Text
                , mkValEntry
                      "textSection"
                      "`textSection start len text` is the section of `text` beginning at `start` of length `len`." $ \start len (text :: Text) ->
                      take len $ drop start text
                , mkValEntry "textConcat" "Concatenate texts." $ mconcat @Text
                ]
          , docTreeEntry
                "Numeric"
                ""
                [ mkValEntry "~==" "Numeric equality, folding exact and inexact numbers." $ (==) @Number
                , mkValEntry "~/=" "Numeric non-equality." $ (/=) @Number
                , mkValEntry "<" "Numeric strictly less." $ (<) @Number
                , mkValEntry "<=" "Numeric less or equal." $ (<=) @Number
                , mkValEntry ">" "Numeric strictly greater." $ (>) @Number
                , mkValEntry ">=" "Numeric greater or equal." $ (>=) @Number
                , docTreeEntry "Integer" "" $
                  [ mkTypeEntry "Integer" "" $
                    MkBoundType $ EntityPinaforeGroundType NilListType $ LiteralEntityGroundType IntegerLiteralType
                  , mkSubtypeRelationEntry "Integer" "Rational" "" $
                    pure $
                    literalSubtypeConversionEntry IntegerLiteralType RationalLiteralType $
                    functionToShim "Integer to Rational" integerToSafeRational
                  ] <>
                  plainFormattingDefs @Integer "Integer" "an integer" <>
                  [ mkValEntry "+" "Add." $ (+) @Integer
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
                  ]
                , docTreeEntry "Rational" "" $
                  [ mkTypeEntry "Rational" "" $
                    MkBoundType $ EntityPinaforeGroundType NilListType $ LiteralEntityGroundType RationalLiteralType
                  , mkSubtypeRelationEntry "Rational" "Number" "" $
                    pure $
                    literalSubtypeConversionEntry RationalLiteralType NumberLiteralType $
                    functionToShim "Rational to Number" safeRationalToNumber
                  ] <>
                  plainFormattingDefs @SafeRational "Rational" "a rational" <>
                  [ mkValEntry ".+" "Add." $ (+) @SafeRational
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
                  [mkSubtypeRelationEntry "Number" "Literal" "" []] <>
                  plainFormattingDefs @Number "Number" "a number" <>
                  [ mkTypeEntry "Number" "" $
                    MkBoundType $ EntityPinaforeGroundType NilListType $ LiteralEntityGroundType NumberLiteralType
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
                  , mkValEntry "meanN" "Mean." $ \(vv :: [Number]) -> sum vv / (ExactNumber $ toRational $ length vv)
                  , mkValEntry "productN" "Product." $ product @[] @Number
                  , mkValEntry
                        "numberCheckSafeRational"
                        "Get the exact value of a Number, if it is one."
                        numberCheckSafeRational
                  , mkValEntry
                        "checkExactInteger"
                        "Get the exact Integer value of a Number, if it is one. Works as expected on Rationals." $ \n ->
                        numberCheckSafeRational n >>= safeRationalCheckInteger
                  ]
                ]
          , docTreeEntry
                "Date & Time"
                ""
                [ docTreeEntry "Duration" "" $
                  [ mkTypeEntry "Duration" "" $
                    MkBoundType $ EntityPinaforeGroundType NilListType $ LiteralEntityGroundType DurationLiteralType
                  , mkSubtypeRelationEntry "Duration" "Literal" "" []
                  ] <>
                  plainFormattingDefs @NominalDiffTime "Duration" "a duration" <>
                  [ mkValEntry "zeroDuration" "No duration." $ (0 :: NominalDiffTime)
                  , mkValEntry "secondsToDuration" "Convert seconds to duration." secondsToNominalDiffTime
                  , mkValEntry "durationToSeconds" "Convert duration to seconds." nominalDiffTimeToSeconds
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
                  [ mkTypeEntry "Time" "Absolute time as measured by UTC." $
                    MkBoundType $ EntityPinaforeGroundType NilListType $ LiteralEntityGroundType TimeLiteralType
                  , mkSubtypeRelationEntry "Time" "Literal" "" []
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
                  [ mkTypeEntry "Date" "" $
                    MkBoundType $ EntityPinaforeGroundType NilListType $ LiteralEntityGroundType DateLiteralType
                  , mkValPatEntry "MkDate" "Construct a Date from year, month, day." fromGregorian $ \day -> let
                        (y, m, d) = toGregorian day
                        in Just (y, (m, (d, ())))
                  , mkSubtypeRelationEntry "Date" "Literal" "" []
                  ] <>
                  plainFormattingDefs @Day "Date" "a date" <>
                  unixFormattingDefs @Day "Date" "a date" <>
                  [ mkValEntry "dateToModifiedJulian" "Convert to MJD." toModifiedJulianDay
                  , mkValEntry "modifiedJulianToDate" "Convert from MJD." ModifiedJulianDay
                  , mkValEntry "addDays" "Add count to days to date." addDays
                  , mkValEntry "diffDays" "Difference of days between dates." diffDays
                  , mkValEntry "getUTCDate" "Get the current UTC date." $ fmap utctDay getCurrentTime
                  , mkValEntry "getDate" "Get the current local date." $ fmap localDay getLocalTime
                  ]
                , docTreeEntry "Time of Day" "" $
                  [ mkTypeEntry "TimeOfDay" "" $
                    MkBoundType $ EntityPinaforeGroundType NilListType $ LiteralEntityGroundType TimeOfDayLiteralType
                  , mkValPatEntry "MkTimeOfDay" "Construct a TimeOfDay from hour, minute, second." TimeOfDay $ \TimeOfDay {..} ->
                        Just (todHour, (todMin, (todSec, ())))
                  , mkSubtypeRelationEntry "TimeOfDay" "Literal" "" []
                  ] <>
                  plainFormattingDefs @TimeOfDay "TimeOfDay" "a time of day" <>
                  unixFormattingDefs @TimeOfDay "TimeOfDay" "a time of day" <>
                  [mkValEntry "midnight" "Midnight." midnight, mkValEntry "midday" "Midday." midday]
                , docTreeEntry "Local Time" "" $
                  [ mkTypeEntry "LocalTime" "" $
                    MkBoundType $ EntityPinaforeGroundType NilListType $ LiteralEntityGroundType LocalTimeLiteralType
                  , mkValPatEntry "MkLocalTime" "Construct a LocalTime from day and time of day." LocalTime $ \LocalTime {..} ->
                        Just (localDay, (localTimeOfDay, ()))
                  , mkSubtypeRelationEntry "LocalTime" "Literal" "" []
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
                "Open Entity Types"
                ""
                [ mkSpecialFormEntry
                      "openEntity"
                      "An open entity for this anchor. `A` is an open entity type."
                      "@A <anchor>"
                      "A" $
                  MkSpecialForm (ConsListType AnnotOpenEntityType $ ConsListType AnnotAnchor NilListType) $ \(MkAnyW (tp :: OpenEntityType tid), (anchor, ())) -> do
                      let
                          typef = openEntityShimWit tp
                          pt :: OpenEntity tid
                          pt = MkOpenEntity $ MkEntity anchor
                      return $ MkAnyValue typef pt
                , mkSpecialFormEntry
                      "newOpenEntity"
                      "Generate an open entity. `A` is an open entity type."
                      "@A"
                      "Action A" $
                  MkSpecialForm (ConsListType AnnotOpenEntityType NilListType) $ \(MkAnyW (tp :: OpenEntityType tid), ()) -> do
                      let
                          pt :: PinaforeAction (OpenEntity tid)
                          pt = liftIO $ newKeyContainerItem @(FiniteSet (OpenEntity tid))
                          typef = actionShimWit $ openEntityShimWit tp
                      return $ MkAnyValue typef pt
                ]
          , docTreeEntry
                "Dynamic Entity Types"
                ""
                [ mkTypeEntry "DynamicEntity" "" $
                  MkBoundType $ EntityPinaforeGroundType NilListType TopDynamicEntityGroundType
                , mkSubtypeRelationEntry "(any dynamic entity type)" "Entity" "" $
                  pure $
                  MkSubypeConversionEntry (EntityPinaforeGroundType NilListType TopDynamicEntityGroundType) $ \case
                      EntityPinaforeGroundType NilListType (ADynamicEntityGroundType _ _) ->
                          Just $ nilSubtypeConversion id
                      _ -> Nothing
                , mkSpecialFormEntry
                      "dynamicEntity"
                      "A dynamic entity for this anchor. `A` is a concrete dynamic entity type."
                      "@A <anchor>"
                      "A" $
                  MkSpecialForm (ConsListType AnnotConcreteDynamicEntityType $ ConsListType AnnotAnchor NilListType) $ \((n, dt), (anchor, ())) -> do
                      let
                          typef = dynamicEntityShimWit n dt
                          pt :: DynamicEntity
                          pt = MkDynamicEntity dt $ MkEntity anchor
                      return $ MkAnyValue typef pt
                , mkSpecialFormEntry
                      "newDynamicEntity"
                      "Generate a dynamic entity. `A` is a concrete dynamic entity type."
                      "@A"
                      "Action A" $
                  MkSpecialForm (ConsListType AnnotConcreteDynamicEntityType NilListType) $ \((n, dt), ()) -> do
                      let
                          pt :: PinaforeAction DynamicEntity
                          pt =
                              liftIO $ do
                                  e <- newKeyContainerItem @(FiniteSet Entity)
                                  return $ MkDynamicEntity dt e
                          typef = actionShimWit $ dynamicEntityShimWit n dt
                      return $ MkAnyValue typef pt
                ]
          ]
    , docTreeEntry
          "Maybe"
          ""
          [ mkTypeEntry "Maybe" "" $
            MkBoundType $ EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType
          , mkValPatEntry "Just" "Construct a Maybe from a value." (Just @A) $ \(v :: Maybe A) ->
                case v of
                    Just a -> Just (a, ())
                    _ -> Nothing
          , mkValPatEntry "Nothing" "Construct a Maybe without a value." (Nothing @BottomType) $ \(v :: Maybe A) ->
                case v of
                    Nothing -> Just ()
                    _ -> Nothing
          , mkSubtypeRelationEntry "Maybe Entity" "Entity" "" $
            pure $
            simpleSubtypeConversionEntry
                (EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType)
                (EntityPinaforeGroundType NilListType TopEntityGroundType) $
            MkSubtypeConversion $ \sc (ConsDolanArguments t NilDolanArguments :: _ pola _) ->
                return $
                MkSubtypeArguments NilDolanArguments $ do
                    let
                        convE =
                            monoToEntityShim $
                            MkMonoType MaybeEntityGroundType $
                            ConsArguments (MkMonoType TopEntityGroundType NilArguments) NilArguments
                    conv <- subtypeConvert sc t $ topEntityType @'Negative
                    pure $ convE . cfmap (iJoinMeetL1 @'Negative . conv)
                {-
                -}
          ]
    , docTreeEntry
          "Pairs"
          ""
          [ mkSubtypeRelationEntry "(Entity,Entity)" "Entity" "" $
            pure $
            simpleSubtypeConversionEntry
                (EntityPinaforeGroundType (ConsListType Refl (ConsListType Refl NilListType)) PairEntityGroundType)
                (EntityPinaforeGroundType NilListType TopEntityGroundType) $
            MkSubtypeConversion $ \sc (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments) :: _ pola _) ->
                return $
                MkSubtypeArguments NilDolanArguments $ do
                    let
                        convE =
                            monoToEntityShim $
                            MkMonoType PairEntityGroundType $
                            ConsArguments (MkMonoType TopEntityGroundType NilArguments) $
                            ConsArguments (MkMonoType TopEntityGroundType NilArguments) NilArguments
                    convA <- subtypeConvert sc ta $ topEntityType @'Negative
                    convB <- subtypeConvert sc tb $ topEntityType @'Negative
                    pure $
                        convE .
                        applyCoPolyShim (cfmap (iJoinMeetL1 @'Negative . convA)) (iJoinMeetL1 @'Negative . convB)
          , mkValEntry "fst" "Get the first member of a pair." $ fst @A @B
          , mkValEntry "snd" "Get the second member of a pair." $ snd @A @B
          , mkValEntry "toPair" "Construct a pair." $ (,) @A @B
          , mkValEntry "pair" "Construct a pair." $ \(a :: A) -> (a, a)
          ]
    , docTreeEntry
          "Either"
          ""
          [ mkTypeEntry "Either" "" $
            MkBoundType $
            EntityPinaforeGroundType (ConsListType Refl $ ConsListType Refl NilListType) EitherEntityGroundType
          , mkValPatEntry "Left" "Construct an Either from the left." (Left @A @B) $ \(v :: Either A B) ->
                case v of
                    Left a -> Just (a, ())
                    _ -> Nothing
          , mkValPatEntry "Right" "Construct an Either from the right." (Right @A @B) $ \(v :: Either A B) ->
                case v of
                    Right a -> Just (a, ())
                    _ -> Nothing
          , mkSubtypeRelationEntry "Either Entity Entity" "Entity" "" $
            pure $
            simpleSubtypeConversionEntry
                (EntityPinaforeGroundType (ConsListType Refl (ConsListType Refl NilListType)) EitherEntityGroundType)
                (EntityPinaforeGroundType NilListType TopEntityGroundType) $
            MkSubtypeConversion $ \sc (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments) :: _ pola _) ->
                return $
                MkSubtypeArguments NilDolanArguments $ do
                    let
                        convE =
                            monoToEntityShim $
                            MkMonoType EitherEntityGroundType $
                            ConsArguments (MkMonoType TopEntityGroundType NilArguments) $
                            ConsArguments (MkMonoType TopEntityGroundType NilArguments) NilArguments
                    convA <- subtypeConvert sc ta $ topEntityType @'Negative
                    convB <- subtypeConvert sc tb $ topEntityType @'Negative
                    pure $
                        convE .
                        applyCoPolyShim (cfmap (iJoinMeetL1 @'Negative . convA)) (iJoinMeetL1 @'Negative . convB)
          , mkValEntry "fromEither" "Eliminate an Either" $ either @A @C @B
          , mkValEntry "either" "Eliminate an Either" $ \(v :: Either A A) ->
                case v of
                    Left a -> a
                    Right a -> a
          ]
    , docTreeEntry
          "Lists"
          ""
          [ mkValPatEntry "[]" "Empty list" ([] @BottomType) $ \(v :: [A]) ->
                case v of
                    [] -> Just ()
                    _ -> Nothing
          , mkValPatEntry "::" "Construct a list" ((:) @A) $ \(v :: [A]) ->
                case v of
                    a:b -> Just (a, (b, ()))
                    _ -> Nothing
          , mkSubtypeRelationEntry "[Entity]" "Entity" "" $
            pure $
            simpleSubtypeConversionEntry
                (EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType)
                (EntityPinaforeGroundType NilListType TopEntityGroundType) $
            MkSubtypeConversion $ \sc (ConsDolanArguments t NilDolanArguments :: _ pola _) ->
                return $
                MkSubtypeArguments NilDolanArguments $ do
                    let
                        convE =
                            monoToEntityShim $
                            MkMonoType ListEntityGroundType $
                            ConsArguments (MkMonoType TopEntityGroundType NilArguments) NilArguments
                    conv <- subtypeConvert sc t $ topEntityType @'Negative
                    pure $ convE . cfmap (iJoinMeetL1 @'Negative . conv)
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
          , mkValEntry "zip" "Zip two lists." $ zip @A @B
          ]
    , docTreeEntry
          "Functions"
          ""
          [ mkValEntry "id" "The identity function." $ id @(->) @A
          , mkValEntry "$" "Apply a function to a value." $ id @(->) @(A -> B)
          , mkValEntry "." "Compose functions." $ (.) @(->) @A @B @C
          , mkValEntry "error" "Error." $ ((\t -> error (unpack t)) :: Text -> BottomType)
          , mkValEntry
                "seq"
                "Evaluate the first argument, then if that's not \"bottom\" (error or non-termination), return the second argument."
                (seq :: TopType -> A -> A)
          , mkSpecialFormEntry "check" "Check from a dynamic supertype." "@A" "D(A) -> Maybe A" $
            MkSpecialForm (ConsListType AnnotPositiveType NilListType) $ \(MkAnyW tp, ()) -> do
                MkGreatestDynamicSupertype dtw _ convm <- getGreatestDynamicSupertype tp
                return $ MkAnyValue (funcShimWit dtw $ maybeShimWit $ mkShimWit tp) $ shimToFunction convm
          , mkSpecialFormEntry "coerce" "Coerce from a dynamic supertype." "@A" "D(A) -> A" $
            MkSpecialForm (ConsListType AnnotPositiveType NilListType) $ \(MkAnyW tp, ()) -> do
                MkGreatestDynamicSupertype dtw@(MkShimWit dtp _) _ convm <- getGreatestDynamicSupertype tp
                return $
                    MkAnyValue (funcShimWit dtw $ mkShimWit tp) $ \d ->
                        case shimToFunction convm d of
                            Just t -> t
                            Nothing ->
                                error $ unpack $ "coercion from " <> exprShow dtp <> " to " <> exprShow tp <> " failed"
          ]
    , docTreeEntry
          "Actions"
          ""
          [ mkTypeEntry "Action" "" $ MkBoundType actionGroundType
          , mkValEntry "return" "A value as an Action." $ return @PinaforeAction @A
          , mkValEntry ">>=" "Bind the result of an Action to an Action." $ qbind
          , mkValEntry ">>" "Do actions in sequence." $ qbind_
          , mkValEntry
                "mapAction"
                "Map a function on an action."
                (fmap :: (A -> B) -> PinaforeAction A -> PinaforeAction B)
          , mkValEntry "fixAction" "The fixed point of an Action." $ mfix @PinaforeAction @A
          , mkValEntry "fail" "Fail, causing the program to terminate with error." $ qfail
          , mkValEntry
                "stop"
                "Stop. This is similar to an exception that can be caught with `onStop`. The default handler (for the main program, button presses, etc.), is to catch and ignore it."
                (empty :: PinaforeAction BottomType)
          , mkValEntry "onStop" "`onStop p q` does `p` first, and if it stops, then does `q`." $ onStop
          , mkValEntry
                "for_"
                "Perform an action on each value of a list."
                (for_ :: [A] -> (A -> PinaforeAction ()) -> PinaforeAction ())
          , mkValEntry
                "for"
                "Perform an action on each value of a list, returning a list."
                (for :: [A] -> (A -> PinaforeAction B) -> PinaforeAction [B])
          , mkValEntry "output" "Output text to standard output." $ output
          , mkValEntry "outputLn" "Output text and a newline to standard output." $ outputLn
          , mkValEntry
                "getTimeMS"
                "Get the time as a whole number of milliseconds."
                (liftIO getTimeMS :: PinaforeAction Integer)
          , mkValEntry "sleep" "Do nothing for this number of milliseconds." (\t -> threadDelay $ t * 1000)
          , mkValEntry "lifecycle" "Close everything that gets opened in the given action." $
            subLifeCycle @PinaforeAction @A
          , mkValEntry "onClose" "Add this action as to be done when closing." pinaforeOnClose
          , mkValEntry "closer" "Get an (idempotent) action that closes what gets opened in the given action." $
            pinaforeEarlyCloser @A
          , mkSpecialFormEntry
                "evaluate"
                "A function that evaluates text as a Pinafore expression to be subsumed to positive type `A`.\n\
                \The result of the action is either the value (`Right`), or an error message (`Left`).\n\
                \The local scope is not in any way transmitted to the evaluation."
                "@A"
                "Text -> Action (Either Text A)" $
            MkSpecialForm (ConsListType AnnotPositiveType NilListType) $ \(MkAnyW tp, ()) -> do
                spvals <- getSpecialVals
                let
                    valShimWit ::
                           forall t.
                           PinaforeShimWit 'Positive t
                        -> PinaforeShimWit 'Positive (Text -> PinaforeAction (Either Text t))
                    valShimWit t' = funcShimWit textShimWit $ actionShimWit $ eitherShimWit textShimWit t'
                return $ MkAnyValue (valShimWit $ mkShimWit tp) $ specialEvaluate spvals tp
          ]
    , docTreeEntry
          "Invocation"
          "How the script was invoked."
          [ mkValEntry "scriptName" "The name of the script." (pack $ iiScriptName pinaforeInvocationInfo :: Text)
          , mkValEntry
                "scriptArguments"
                "Arguments passed to the script."
                (fmap pack $ iiScriptArguments pinaforeInvocationInfo :: [Text])
          , mkValEntry
                "environment"
                "Environment variables."
                (fmap (\(n, v) -> (pack n, pack v)) $ iiEnvironment pinaforeInvocationInfo :: [(Text, Text)])
          , mkValEntry "getEnv" "Get environment variable." getEnv
          ]
    , docTreeEntry
          "Undo"
          "Undo and redo changes."
          [ mkValEntry "queueUndo" "Undo an action." $ do
                uh <- pinaforeUndoHandler
                rc <- pinaforeResourceContext
                liftIO $ undoHandlerUndo uh rc noEditSource
          , mkValEntry "queueRedo" "Redo an action." $ do
                uh <- pinaforeUndoHandler
                rc <- pinaforeResourceContext
                liftIO $ undoHandlerRedo uh rc noEditSource
          ]
    ]
