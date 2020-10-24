module Pinafore.Language.Predefined.Base
    ( base_predefinitions
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
import Pinafore.Language.If
import Pinafore.Language.Name
import Pinafore.Language.Predefined.Defs
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var
import Shapes
import Shapes.Numeric

getTimeMS :: IO Integer
getTimeMS = do
    MkSystemTime s ns <- getSystemTime
    return $ (toInteger s) * 1000 + div (toInteger ns) 1000000

output :: (?pinafore :: PinaforeContext) => Text -> PinaforeAction ()
output text = liftIO $ hPutStrLn pinaforeStdOut $ unpack text

outputLn :: (?pinafore :: PinaforeContext) => Text -> PinaforeAction ()
outputLn text = liftIO $ hPutStrLn pinaforeStdOut $ unpack text

setentity :: LangWholeRef '( A, TopType) -> A -> PinaforeAction ()
setentity ref val = langWholeRefSet ref (Known val)

deleteentity :: LangWholeRef '( BottomType, TopType) -> PinaforeAction ()
deleteentity ref = langWholeRefSet ref Unknown

qfail :: Text -> PinaforeAction BottomType
qfail t = fail $ unpack t

entityAnchor :: Entity -> Text
entityAnchor p = pack $ show p

onStop :: PinaforeAction A -> PinaforeAction A -> PinaforeAction A
onStop p q = p <|> q

newMemWhole :: PinaforeAction (LangWholeRef '( A, A))
newMemWhole = do
    r <- liftIO $ makeMemoryReference Unknown $ \_ -> True
    model <- liftLifeCycle $ makeReflectingModel r
    uh <- pinaforeUndoHandler
    return $ pinaforeRefToWholeRef $ MkWModel $ undoHandlerModel uh model

newMemFiniteSet :: PinaforeAction (LangFiniteSetRef '( MeetType Entity A, A))
newMemFiniteSet = do
    r <- liftIO $ makeMemoryReference mempty $ \_ -> True
    model <- liftLifeCycle $ makeReflectingModel $ convertReference r
    uh <- pinaforeUndoHandler
    return $ meetValueLangFiniteSetRef $ MkWModel $ undoHandlerModel uh model

zeroTime :: UTCTime
zeroTime = UTCTime (fromGregorian 2000 1 1) 0

newClock :: NominalDiffTime -> PinaforeAction (PinaforeImmutableWholeRef UTCTime)
newClock duration = do
    (clockOM, ()) <- liftLifeCycle $ makeSharedModel $ clockPremodel zeroTime duration
    return $ functionImmutableRef $ MkWModel $ clockOM

newTimeZoneRef :: PinaforeImmutableWholeRef UTCTime -> PinaforeAction (PinaforeImmutableWholeRef Int)
newTimeZoneRef now = do
    rc <- pinaforeResourceContext
    ref <-
        liftLifeCycle $
        eaFloatMapReadOnly
            rc
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

base_predefinitions :: [DocTreeEntry BindDoc]
base_predefinitions =
    [ docTreeEntry
          "Literals & Entities"
          ""
          [ mkValEntry "==" "Entity equality." $ (==) @Entity
          , mkValEntry "/=" "Entity non-equality." $ (/=) @Entity
          , mkValEntry "entityAnchor" "The anchor of an entity, as text." entityAnchor
          , mkSupertypeEntry "id" "Every literal is an entity." $ literalToEntity @Literal
          , mkValEntry "toText" "The text of a literal." unLiteral
          , docTreeEntry
                "Boolean"
                ""
                [ mkSupertypeEntry "id" "Every boolean is a literal." $ toLiteral @Bool
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
                [ mkValPatEntry "LT" "Less than." LT $ \v ->
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
                [ mkSupertypeEntry "id" "Every text is a literal." $ toLiteral @Text
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
                  [mkSupertypeEntry "id" "Every integer is a rational." integerToSafeRational] <>
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
                  [mkSupertypeEntry "id" "Every rational is a number." safeRationalToNumber] <>
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
                  [mkSupertypeEntry "id" "Every number is a literal." $ toLiteral @Number] <>
                  plainFormattingDefs @Number "Number" "a number" <>
                  [ mkValEntry "~+" "Add." $ (+) @Number
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
                  [mkSupertypeEntry "id" "Every duration is a literal." $ toLiteral @NominalDiffTime] <>
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
                , docTreeEntry "Time" "Absolute time as measured by UTC." $
                  [mkSupertypeEntry "id" "Every time is a literal." $ toLiteral @UTCTime] <>
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
                  [ mkValPatEntry "Date" "Construct a Date from year, month, day." fromGregorian $ \day -> let
                        (y, m, d) = toGregorian day
                        in Just (y, (m, (d, ())))
                  , mkSupertypeEntry "id" "Every day is a literal." $ toLiteral @Day
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
                  [ mkValPatEntry "TimeOfDay" "Construct a TimeOfDay from hour, minute, second." TimeOfDay $ \TimeOfDay {..} ->
                        Just (todHour, (todMin, (todSec, ())))
                  , mkSupertypeEntry "id" "Every time of day is a literal." $ toLiteral @TimeOfDay
                  ] <>
                  plainFormattingDefs @TimeOfDay "TimeOfDay" "a time of day" <>
                  unixFormattingDefs @TimeOfDay "TimeOfDay" "a time of day" <>
                  [mkValEntry "midnight" "Midnight." midnight, mkValEntry "midday" "Midday." midday]
                , docTreeEntry "Local Time" "" $
                  [ mkValPatEntry "LocalTime" "Construct a LocalTime from day and time of day." LocalTime $ \LocalTime {..} ->
                        Just (localDay, (localTimeOfDay, ()))
                  , mkSupertypeEntry "id" "Every local time is a literal." $ toLiteral @LocalTime
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
          ]
    , docTreeEntry
          "Maybe"
          ""
          [ mkValPatEntry "Just" "Construct a Maybe from a value." (Just @A) $ \(v :: Maybe A) ->
                case v of
                    Just a -> Just (a, ())
                    _ -> Nothing
          , mkValPatEntry "Nothing" "Construct a Maybe without a value." (Nothing @BottomType) $ \(v :: Maybe A) ->
                case v of
                    Nothing -> Just ()
                    _ -> Nothing
          , mkSupertypeEntry "id" "Entity conversion." $
            entityAdapterConvert $
            concreteEntityAdapter $
            MkConcreteType MaybeEntityGroundType $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
          ]
    , docTreeEntry
          "Pairs"
          ""
          [ mkSupertypeEntry "id" "Entity conversion." $
            entityAdapterConvert $
            concreteEntityAdapter $
            MkConcreteType PairEntityGroundType $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
          , mkValEntry "fst" "Get the first member of a pair." $ fst @A @B
          , mkValEntry "snd" "Get the second member of a pair." $ snd @A @B
          , mkValEntry "toPair" "Construct a pair." $ (,) @A @B
          , mkValEntry "pair" "Construct a pair." $ \(a :: A) -> (a, a)
          ]
    , docTreeEntry
          "Either"
          ""
          [ mkValPatEntry "Left" "Construct an Either from the left." (Left @A @B) $ \(v :: Either A B) ->
                case v of
                    Left a -> Just (a, ())
                    _ -> Nothing
          , mkValPatEntry "Right" "Construct an Either from the right." (Right @A @B) $ \(v :: Either A B) ->
                case v of
                    Right a -> Just (a, ())
                    _ -> Nothing
          , mkSupertypeEntry "id" "Entity conversion." $
            entityAdapterConvert $
            concreteEntityAdapter $
            MkConcreteType EitherEntityGroundType $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
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
          , mkSupertypeEntry "id" "Entity conversion." $
            entityAdapterConvert $
            concreteEntityAdapter $
            MkConcreteType ListEntityGroundType $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
          , mkValEntry "list" "Eliminate a list" $ \(fnil :: B) fcons (l :: [A]) ->
                case l of
                    [] -> fnil
                    (a:aa) -> fcons a aa
          , mkValEntry "length" "Number of items in a list" (length :: [TopType] -> Int)
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
          ]
    , docTreeEntry
          "Actions"
          ""
          [ mkValEntry "return" "A value as an Action." $ return @PinaforeAction @A
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
          , mkValEntry "lifecycle" "Close everything that gets opened in the given action." $
            subLifeCycle @PinaforeAction @A
          , mkValEntry "onClose" "Add this action as to be done when closing." pinaforeOnClose
          , mkValEntry "closer" "Get an (idempotent) action that closes what gets opened in the given action." $
            pinaforeEarlyCloser @A
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
    , docTreeEntry
          "References"
          "References keep track of updates, and will update user interfaces constructed from them when their value changes."
          [ docTreeEntry
                "Whole References"
                "A whole reference of type `WholeRef {-p,+q}` has a setting type of `p` and a getting type of `q`."
                [ mkValEntry
                      "pureWhole"
                      "A constant whole reference for a value."
                      (pure :: A -> PinaforeImmutableWholeRef A)
                , mkValEntry
                      "immutWhole"
                      "Convert a whole reference to immutable.\n`immutWhole r = {%r}`"
                      (id :: PinaforeImmutableWholeRef A -> PinaforeImmutableWholeRef A)
                , mkValEntry
                      "coMapWhole"
                      "Map a function on getting a whole reference."
                      (coRangeLift :: (A -> B) -> LangWholeRef '( C, A) -> LangWholeRef '( C, B))
                , mkValEntry
                      "contraMapWhole"
                      "Map a function on setting a whole reference."
                      (contraRangeLift :: (B -> A) -> LangWholeRef '( A, C) -> LangWholeRef '( B, C))
                , mkValEntry "maybeLensMapWhole" "Map getter & pushback functions on a whole reference." $
                  maybeLensLangWholeRef @AP @AQ @BP @BQ
                , mkValEntry "lensMapWhole" "Map getter & pushback functions on a whole reference." $
                  fLensLangWholeRef @AP @AQ @B
                , mkValEntry "maybeWhole" "Map known/unknown to `Maybe` for a whole reference." $
                  langMaybeWholeRef @A @B
                , mkValEntry "pairWhole" "Combine whole references." $ langPairWholeRefs @AP @AQ @BP @BQ
                , mkValEntry
                      "applyWhole"
                      "Combine getting of whole references. `applyWhole f x` = `{%f %x}`"
                      ((<*>) :: PinaforeImmutableWholeRef (A -> B) -> PinaforeImmutableWholeRef A -> PinaforeImmutableWholeRef B)
                , mkValEntry
                      "unknown"
                      "The unknown whole reference, representing missing information."
                      (empty :: PinaforeImmutableWholeRef BottomType)
                , mkValEntry "known" "True if the whole reference is known." $ \(val :: PinaforeROWRef (Know TopType)) ->
                      (eaMapReadOnlyWhole (Known . isKnown) val :: PinaforeROWRef (Know Bool))
                , mkValEntry
                      "??"
                      "`p ?? q` = `p` if it is known, else `q`."
                      ((<|>) :: PinaforeImmutableWholeRef A -> PinaforeImmutableWholeRef A -> PinaforeImmutableWholeRef A)
                , mkValEntry "get" "Get a whole reference, or `stop` if the whole reference is unknown." $
                  langWholeRefGet @A
                , mkValEntry ":=" "Set a whole reference to a value. Stop if failed." setentity
                , mkValEntry "delete" "Delete a whole reference (i.e., make unknown). Stop if failed." deleteentity
                , mkValEntry "newMemWhole" "Create a new whole reference to memory, initially unknown." newMemWhole
                ]
          , docTreeEntry
                "Set References"
                ""
                [ mkValEntry "mapSet" "Map a function on a set." (contramap :: (A -> B) -> LangSetRef B -> LangSetRef A)
                , mkValEntry "pureSet" "Convert a predicate to a set." $ predicateToLangSetRef @A
                , mkValEntry "refSet" "Convert a predicate reference to a set." $ predicateRefToLangSetRef @A
                , mkValEntry "immutSet" "Convert a set to immutable." $ langSetRefImmutable @A
                , mkValEntry "+=" "Add an entity to a set." $ langSetRefAdd @A
                , mkValEntry "-=" "Remove an entity from a set." $ langSetRefRemove @A
                , mkValEntry "member" "A reference to the membership of a value in a set." $ langSetRefMember @A
                , mkValEntry
                      "notSet"
                      "Complement of a set. The resulting set can be added to (deleting from the original set) and deleted from (adding to the original set)." $
                  langSetRefComplement @A
                , mkValEntry
                      "<&>"
                      "Intersection of sets. The resulting set can be added to (adding to both sets), but not deleted from." $
                  langSetRefIntersect @A
                , mkValEntry
                      "<|>"
                      "Union of sets. The resulting set can be deleted from (deleting from both sets), but not added to." $
                  langSetRefUnion @A
                , mkValEntry
                      "<\\>"
                      "Difference of sets, everything in the first set but not the second. The resulting set can be added to (adding to the first and deleting from the second), but not deleted from." $
                  langSetRefDifference @A
                , mkValEntry
                      "<^>"
                      "Symmetric difference of sets, everything in exactly one of the sets. The resulting set will be read-only." $
                  langSetRefSymmetricDifference @A
                , mkValEntry "<+>" "Cartesian sum of sets." $ langSetRefCartesianSum @A @B
                , mkValEntry "<*>" "Cartesian product of sets. The resulting set will be read-only." $
                  langSetRefCartesianProduct @A @B
                ]
          , docTreeEntry
                "Finite Set References"
                ""
                [ mkSupertypeEntry "id" "Every finite set is a set." $ langFiniteSetRefToSetRef @A @TopType
                , mkValEntry
                      "coMapFiniteSet"
                      "Map a function on getting from a finite set."
                      (coRangeLift :: (A -> B) -> LangFiniteSetRef '( C, A) -> LangFiniteSetRef '( C, B))
                , mkValEntry
                      "contraMapFiniteSet"
                      "Map a function on setting to and testing a finite set."
                      (contraRangeLift :: (B -> A) -> LangFiniteSetRef '( A, C) -> LangFiniteSetRef '( B, C))
                , mkValEntry "<:&>" "Intersect a finite set with any set. The resulting finite set will be read-only." $
                  langFiniteSetRefSetIntersect @A @B
                , mkValEntry
                      "<:\\>"
                      "Difference of a finite set and any set. The resulting finite set will be read-only." $
                  langFiniteSetRefSetDifference @A @B
                , mkValEntry
                      "<:&:>"
                      "Intersection of finite sets. The resulting finite set can be added to, but not deleted from." $
                  langFiniteSetRefMeet @A
                , mkValEntry
                      "<:|:>"
                      "Union of finite sets. The resulting finite set can be deleted from, but not added to." $
                  langFiniteSetRefJoin @A
                , mkValEntry "<:+:>" "Cartesian sum of finite sets." $ langFiniteSetRefCartesianSum @AP @AQ @BP @BQ
                , mkSupertypeEntry "<:+:>" "Cartesian sum of finite sets." $ langFiniteSetRefCartesianSum @A @A @B @B
                , mkValEntry "<:*:>" "Cartesian product of finite sets. The resulting finite set will be read-only." $
                  langFiniteSetRefCartesianProduct @AP @AQ @BP @BQ
                , mkSupertypeEntry
                      "<:*:>"
                      "Cartesian product of finite sets. The resulting finite set will be read-only." $
                  langFiniteSetRefCartesianProduct @A @A @B @B
                , mkValEntry "members" "Get all members of a finite set, by an order." $ pinaforeSetGetOrdered @A
                , mkValEntry
                      "listFiniteSet"
                      "Represent a reference to a list as a finite set. Changing the set may scramble the order of the list." $
                  langListRefToFiniteSetRef @A
                , mkValEntry "single" "The member of a single-member finite set, or unknown." $
                  langFiniteSetRefSingle @A
                , mkValEntry "count" "Count of members in a finite set." $ langFiniteSetRefFunc @TopType @Int olength
                , mkValEntry
                      "removeAll"
                      "Remove all entities from a finite set."
                      (langFiniteSetRefRemoveAll :: LangFiniteSetRef '( BottomType, TopType) -> PinaforeAction ())
                , mkValEntry
                      "newMemFiniteSet"
                      "Create a new finite set reference to memory, initially empty."
                      newMemFiniteSet
                ]
          ]
    , docTreeEntry
          "Morphisms"
          "Morphisms relate entities."
          [ mkValEntry "identity" "The identity morphism." $ identityLangMorphism @X @Y
          , mkValEntry "!." "Compose morphisms." $ composeLangMorphism @AP @AQ @BX @BY @CP @CQ
          , mkSupertypeEntry "!." "Compose morphisms." $ composeLangMorphism @A @A @B @B @C @C
          , mkValEntry "!**" "Pair morphisms. References from these morphisms are undeleteable." $
            pairLangMorphism @AP @AQ @BP @BQ @CP @CQ
          , mkSupertypeEntry "!**" "Pair morphisms. References from these morphisms are undeleteable." $
            pairLangMorphism @A @A @B @B @C @C
          , mkValEntry "!++" "Either morphisms. References from these morphisms are undeleteable." $
            eitherLangMorphism @AP @AQ @BP @BQ @CP @CQ
          , mkSupertypeEntry "!++" "Either morphisms. References from these morphisms are undeleteable." $
            eitherLangMorphism @A @A @B @B @C @C
          , mkValEntry "!$" "Apply a morphism to a reference." $ applyLangMorphismRef @AP @AQ @BP @BQ
          , mkSupertypeEntry "!$" "Apply a morphism to a reference." $ applyLangMorphismRef @A @A @B @B
          , mkValEntry "!$%" "Apply a morphism to an immutable reference. `m !$% r = m !$ immutWhole r`" $
            applyLangMorphismImmutRef @A @BP @BQ
          , mkSupertypeEntry "!$%" "Apply a morphism to an immutable reference. `m !$% r = m !$ immutWhole r`" $
            applyLangMorphismImmutRef @A @B @B
          , mkValEntry "!$$" "Apply a morphism to a set." $ applyLangMorphismSet @A @B
          , mkValEntry "!@" "Co-apply a morphism to a reference." $ inverseApplyLangMorphismRef @A @BX @BY
          , mkSupertypeEntry "!@" "Co-apply a morphism to a reference." $ inverseApplyLangMorphismRef @A @B @B
          , mkValEntry "!@%" "Co-apply a morphism to an immutable reference. `m !@% r = m !@ immutWhole r`" $
            inverseApplyLangMorphismImmutRef @A @B
          , mkValEntry "!@@" "Co-apply a morphism to a set." $ inverseApplyLangMorphismSet @A @BX @BY
          , mkSupertypeEntry "!@@" "Co-apply a morphism to a set." $ inverseApplyLangMorphismSet @A @B @B
          ]
    , docTreeEntry
          "RefOrders"
          ""
          [ mkSupertypeEntry "id" "Every order is a RefOrder." $ pureRefOrder @A
          , mkValEntry "refOrders" "Join RefOrders by priority." $ refOrders @A
          , mkValEntry
                "mapRefOrder"
                "Map a function on a RefOrder."
                (contramap :: (B -> A) -> LangRefOrder A -> LangRefOrder B)
          , mkValEntry "refOrderOn" "Order by a RefOrder on a particular morphism." $ refOrderOn @B @A
          , mkValEntry "reverseRef" "Reverse a RefOrder." $ reverseRefOrder @A
          , mkValEntry "orderWholeRef" "Order two whole references." $ langRefOrderCompare @A
          ]
    ]
