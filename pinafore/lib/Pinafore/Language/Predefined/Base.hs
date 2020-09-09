module Pinafore.Language.Predefined.Base
    ( base_predefinitions
    , outputLn
    ) where

import Changes.Core
import Changes.World.Clock
import Data.Time
import Data.Time.Clock.System
import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.If
import Pinafore.Language.Predefined.Defs
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var
import Shapes
import Shapes.Numeric
import Changes.Debug

getTimeMS :: IO Integer
getTimeMS = do
    MkSystemTime s ns <- getSystemTime
    return $ (toInteger s) * 1000 + div (toInteger ns) 1000000

output :: Text -> PinaforeAction ()
output text = liftIO $ putStr $ unpack text

outputLn :: Text -> PinaforeAction ()
outputLn text = liftIO $ putStrLn $ unpack text

setentity :: LangRef '( A, TopType) -> A -> PinaforeAction ()
setentity ref val = langRefSet ref (Known val)

deleteentity :: LangRef '( BottomType, TopType) -> PinaforeAction ()
deleteentity ref = langRefSet ref Unknown

qfail :: Text -> PinaforeAction BottomType
qfail t = fail $ unpack t

entityAnchor :: Entity -> Text
entityAnchor p = pack $ show p

onStop :: PinaforeAction A -> PinaforeAction A -> PinaforeAction A
onStop p q = p <|> q

newMemRef :: PinaforeAction (LangRef '( A, A))
newMemRef = do
    r <- liftIO $ makeMemoryReference Unknown $ \_ -> True
    model <- pinaforeLiftLifeCycleIO $ makeReflectingModel r
    uh <- pinaforeUndoHandler
    return $ pinaforeRefToRef $ MkPinaforeRef $ undoHandlerModel uh model

newMemFiniteSet :: PinaforeAction (LangFiniteSetRef '( MeetType Entity A, A))
newMemFiniteSet = do
    r <- liftIO $ makeMemoryReference mempty $ \_ -> True
    model <- pinaforeLiftLifeCycleIO $ makeReflectingModel $ convertReference r
    uh <- pinaforeUndoHandler
    return $ meetValueLangFiniteSetRef $ MkPinaforeRef $ undoHandlerModel uh model

zeroTime :: UTCTime
zeroTime = UTCTime (fromGregorian 2000 1 1) 0

newClock :: NominalDiffTime -> PinaforeAction (PinaforeImmutableRef UTCTime)
newClock duration = do
    (clockOM, ()) <- pinaforeLiftLifeCycleIO $ makeSharedModel $ clockPremodel zeroTime duration
    return $ functionImmutableRef $ MkPinaforeRef $ clockOM

newTimeZoneRef :: PinaforeImmutableRef UTCTime -> PinaforeAction (PinaforeImmutableRef Int)
newTimeZoneRef now = do
    rc <- pinaforeResourceContext
    ref <-
        pinaforeLiftLifeCycleIO $
        eaFloatMapReadOnly
            rc
            (floatLift (\mr ReadWhole -> fmap (fromKnow zeroTime) $ mr ReadWhole) liftROWChangeLens clockTimeZoneLens) $
        immutableRefToReadOnlyRef now
    return $ fmap timeZoneMinutes $ MkPinaforeImmutableRef ref

interpretAsText ::
       forall a. AsLiteral a
    => LangRef '( a, a)
    -> LangRef '( Text, Text)
interpretAsText = let
    getter :: Maybe a -> Maybe Text
    getter Nothing = Just ""
    getter (Just a) = Just $ unLiteral $ toLiteral a
    setter :: Maybe Text -> Maybe a -> Maybe (Maybe a)
    setter Nothing _ = Just Nothing
    setter (Just "") _ = Just Nothing
    setter (Just t) _ = fmap Just $ parseLiteral t
    in maybeLensLangRef getter setter

parseLiteral :: AsLiteral t => Text -> Maybe t
parseLiteral = knowToMaybe . fromLiteral . MkLiteral

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

getLocalTime :: IO LocalTime
getLocalTime = fmap zonedTimeToLocalTime getZonedTime

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
                , docTreeEntry
                      "Integer"
                      ""
                      [ mkSupertypeEntry "id" "Every integer is a rational." integerToSafeRational
                      , mkValEntry "parseInteger" "Parse text as an integer." $ parseLiteral @Integer
                      , mkValEntry
                            "interpretIntegerAsText"
                            "Interpret an integer reference as text, interpreting deleted values as empty text" $
                        interpretAsText @Integer
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
                      ]
                , docTreeEntry
                      "Rational"
                      ""
                      [ mkSupertypeEntry "id" "Every rational is a number." safeRationalToNumber
                      , mkValEntry "parseRational" "Parse text as a rational." $ parseLiteral @SafeRational
                      , mkValEntry
                            "interpretRationalAsText"
                            "Interpret a rational reference as text, interpreting deleted values as empty text." $
                        interpretAsText @SafeRational
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
                , docTreeEntry
                      "Number"
                      ""
                      [ mkSupertypeEntry "id" "Every number is a literal." $ toLiteral @Number
                      , mkValEntry "parseNumber" "Parse text as a number." $ parseLiteral @Number
                      , mkValEntry
                            "interpretNumberAsText"
                            "Interpret a number reference as text, interpreting deleted values as empty text." $
                        interpretAsText @Number
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
                      , mkValEntry
                            "checkExactSafeRational"
                            "Get the exact value of a Number, if it is one."
                            checkExactSafeRational
                      , mkValEntry
                            "checkExactInteger"
                            "Get the exact Integer value of a Number, if it is one. Works as expected on Rationals." $ \n ->
                            checkExactSafeRational n >>= safeRationalInteger
                      ]
                ]
          , docTreeEntry
                "Date & Time"
                ""
                [ docTreeEntry
                      "Duration"
                      ""
                      [ mkSupertypeEntry "id" "Every duration is a literal." $ toLiteral @NominalDiffTime
                      , mkValEntry "parseDuration" "Parse text as a duration. Inverse of `toText`." $
                        parseLiteral @NominalDiffTime
                      , mkValEntry
                            "interpretDurationAsText"
                            "Interpret a duration reference as text, interpreting deleted values as empty text." $
                        interpretAsText @NominalDiffTime
                      , mkValEntry "zeroDuration" "No duration." $ (0 :: NominalDiffTime)
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
                , docTreeEntry
                      "Time"
                      "Absolute time as measured by UTC."
                      [ mkSupertypeEntry "id" "Every time is a literal." $ toLiteral @UTCTime
                      , mkValEntry "parseTime" "Parse text as a time. Inverse of `toText`." $ parseLiteral @UTCTime
                      , mkValEntry
                            "interpretTimeAsText"
                            "Interpret a time reference as text, interpreting deleted values as empty text." $
                        interpretAsText @UTCTime
                      , mkValEntry "addTime" "Add duration to time." addUTCTime
                      , mkValEntry "diffTime" "Difference of times." diffUTCTime
                      , mkValEntry "unixFormatTime" "Format a time as text, using a UNIX-style formatting string." $
                        unixFormat @UTCTime
                      , mkValEntry "unixParseTime" "Parse text as a time, using a UNIX-style formatting string." $
                        unixParse @UTCTime
                      , mkValEntry "getTime" "Get the current time." $ getCurrentTime
                      , mkValEntry
                            "newClock"
                            "Make a reference to the current time that updates per the given duration."
                            newClock
                      ]
                , docTreeEntry
                      "Calendar"
                      ""
                      [ mkValPatEntry "Date" "Construct a Date from year, month, day." fromGregorian $ \day -> let
                            (y, m, d) = toGregorian day
                            in Just (y, (m, (d, ())))
                      , mkSupertypeEntry "id" "Every day is a literal." $ toLiteral @Day
                      , mkValEntry "parseDate" "Parse text as a day." $ parseLiteral @Day
                      , mkValEntry "interpretDateAsText" "Interpret a date reference as text." $ interpretAsText @Day
                      , mkValEntry "dateToModifiedJulian" "Convert to MJD." toModifiedJulianDay
                      , mkValEntry "modifiedJulianToDate" "Convert from MJD." ModifiedJulianDay
                      , mkValEntry "addDays" "Add count to days to date." addDays
                      , mkValEntry "diffDays" "Difference of days between dates." diffDays
                      , mkValEntry "unixFormatDate" "Format a date as text, using a UNIX-style formatting string." $
                        unixFormat @Day
                      , mkValEntry "unixParseDate" "Parse text as a date, using a UNIX-style formatting string." $
                        unixParse @Day
                      , mkValEntry "getUTCDate" "Get the current UTC date." $ fmap utctDay getCurrentTime
                      , mkValEntry "getDate" "Get the current local date." $ fmap localDay getLocalTime
                      ]
                , docTreeEntry
                      "Time of Day"
                      ""
                      [ mkValPatEntry "TimeOfDay" "Construct a TimeOfDay from hour, minute, second." TimeOfDay $ \TimeOfDay {..} ->
                            Just (todHour, (todMin, (todSec, ())))
                      , mkSupertypeEntry "id" "Every time of day is a literal." $ toLiteral @TimeOfDay
                      , mkValEntry "parseTimeOfDay" "Parse text as a time of day." $ parseLiteral @TimeOfDay
                      , mkValEntry
                            "interpretTimeOfDayAsText"
                            "Interpret a time of day reference as text, interpreting deleted values as empty text." $
                        interpretAsText @TimeOfDay
                      , mkValEntry "midnight" "Midnight." midnight
                      , mkValEntry "midday" "Midday." midday
                      , mkValEntry
                            "unixFormatTimeOfDay"
                            "Format a time of day as text, using a UNIX-style formatting string." $
                        unixFormat @TimeOfDay
                      , mkValEntry
                            "unixParseTimeOfDay"
                            "Parse text as a time of day, using a UNIX-style formatting string." $
                        unixParse @TimeOfDay
                      ]
                , docTreeEntry
                      "Local Time"
                      ""
                      [ mkValPatEntry "LocalTime" "Construct a LocalTime from day and time of day." LocalTime $ \LocalTime {..} ->
                            Just (localDay, (localTimeOfDay, ()))
                      , mkSupertypeEntry "id" "Every local time is a literal." $ toLiteral @LocalTime
                      , mkValEntry "parseLocalTime" "Parse text as a local time." $ parseLiteral @LocalTime
                      , mkValEntry
                            "interpretLocalTimeAsText"
                            "Interpret a local time reference as text, interpreting deleted values as empty text." $
                        interpretAsText @LocalTime
                      , mkValEntry "timeToLocal" "Convert a time to local time, given a time zone offset in minutes" $ \i ->
                            utcToLocalTime $ minutesToTimeZone i
                      , mkValEntry "localToTime" "Convert a local time to time, given a time zone offset in minutes" $ \i ->
                            localTimeToUTC $ minutesToTimeZone i
                      , mkValEntry
                            "unixFormatLocalTime"
                            "Format a local time as text, using a UNIX-style formatting string." $
                        unixFormat @LocalTime
                      , mkValEntry
                            "unixParseLocalTime"
                            "Parse text as a local time, using a UNIX-style formatting string." $
                        unixParse @LocalTime
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
          [ mkPatEntry "[]" "Empty list" "[None]" $ \(v :: [A]) ->
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
          , mkValEntry "debugmsg" "Debug message (debug only)." (traceIOM . unpack :: Text -> PinaforeAction ())
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
          "A reference of type `Ref {-p,+q}` has a setting type of `p` and a getting type of `q`. References keep track of updates, and will update user interfaces constructed from them when their value changes."
          [ mkValEntry "pureRef" "A constant reference for a value." (pure :: A -> PinaforeImmutableRef A)
          , mkValEntry
                "immutRef"
                "Convert a reference to immutable.\n`immutRef r = {%r}`"
                (id :: PinaforeImmutableRef A -> PinaforeImmutableRef A)
          , mkValEntry
                "coMapRef"
                "Map a function on getting a reference."
                (coRangeLift :: (A -> B) -> LangRef '( C, A) -> LangRef '( C, B))
          , mkValEntry
                "contraMapRef"
                "Map a function on setting a reference."
                (contraRangeLift :: (B -> A) -> LangRef '( A, C) -> LangRef '( B, C))
          , mkValEntry "maybeLensMapRef" "Map getter & pushback functions on a reference." $
            maybeLensLangRef @AP @AQ @BP @BQ
          , mkValEntry "lensMapRef" "Map getter & pushback functions on a reference." $ fLensLangRef @AP @AQ @B
          , mkValEntry "maybeRef" "Map getter & pushback functions on a reference." $ maybeRef @A @B
          , mkValEntry "pairRef" "Combine references." $ langPairRefs @AP @AQ @BP @BQ
          , mkValEntry
                "applyRef"
                "Combine getting of references. `applyRef f x` = `{?f ?x}`"
                ((<*>) :: PinaforeImmutableRef (A -> B) -> PinaforeImmutableRef A -> PinaforeImmutableRef B)
          , mkValEntry
                "unknown"
                "The unknown reference, representing missing information."
                (empty :: PinaforeImmutableRef BottomType)
          , mkValEntry "known" "True if the reference is known." $ \(val :: PinaforeROWRef (Know TopType)) ->
                (eaMapReadOnlyWhole (Known . isKnown) val :: PinaforeROWRef (Know Bool))
          , mkValEntry
                "??"
                "`p ?? q` = `p` if it is known, else `q`."
                ((<|>) :: PinaforeImmutableRef A -> PinaforeImmutableRef A -> PinaforeImmutableRef A)
          , mkValEntry "get" "Get a reference, or `stop` if the reference is unknown." $ langRefGet @A
          , mkValEntry "runRef" "Run an action from a reference." $ runLangRef
          , mkValEntry ":=" "Set a reference to a value. Stop if failed." setentity
          , mkValEntry "delete" "Delete an entity reference. Stop if failed." deleteentity
          , mkValEntry "newMemRef" "Create a new reference to memory, initially unknown." newMemRef
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
          , mkValEntry "newEntity" "Create a new entity in a set and act on it." $ langSetRefAddNew
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
          , mkValEntry "<:\\>" "Difference of a finite set and any set. The resulting finite set will be read-only." $
            langFiniteSetRefSetDifference @A @B
          , mkValEntry
                "<:&:>"
                "Intersection of finite sets. The resulting finite set can be added to, but not deleted from." $
            langFiniteSetRefMeet @A
          , mkValEntry "<:|:>" "Union of finite sets. The resulting finite set can be deleted from, but not added to." $
            langFiniteSetRefJoin @A
          , mkValEntry "<:+:>" "Cartesian sum of finite sets." $ langFiniteSetRefCartesianSum @AP @AQ @BP @BQ
          , mkSupertypeEntry "<:+:>" "Cartesian sum of finite sets." $ langFiniteSetRefCartesianSum @A @A @B @B
          , mkValEntry "<:*:>" "Cartesian product of finite sets. The resulting finite set will be read-only." $
            langFiniteSetRefCartesianProduct @AP @AQ @BP @BQ
          , mkSupertypeEntry "<:*:>" "Cartesian product of finite sets. The resulting finite set will be read-only." $
            langFiniteSetRefCartesianProduct @A @A @B @B
          , mkValEntry "members" "Get all members of a finite set, by an order." $ pinaforeSetGetOrdered @A
          , mkValEntry
                "listFiniteSet"
                "Represent a reference to a list as a finite set. Changing the set may scramble the order of the list." $
            langListRefToFiniteSetRef @A
          , mkValEntry "single" "The member of a single-member finite set, or unknown." $ langFiniteSetRefSingle @A
          , mkValEntry "count" "Count of members in a finite set." $ langFiniteSetRefFunc @TopType @Int olength
          , mkValEntry
                "removeAll"
                "Remove all entities from a finite set."
                (langFiniteSetRefRemoveAll :: LangFiniteSetRef '( BottomType, TopType) -> PinaforeAction ())
          , mkValEntry "newMemFiniteSet" "Create a new finite set reference to memory, initially empty." newMemFiniteSet
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
          , mkValEntry "!$%" "Apply a morphism to an immutable reference. `m !$% r = m !$ immutRef r`" $
            applyLangMorphismImmutRef @A @BP @BQ
          , mkSupertypeEntry "!$%" "Apply a morphism to an immutable reference. `m !$% r = m !$ immutRef r`" $
            applyLangMorphismImmutRef @A @B @B
          , mkValEntry "!$$" "Apply a morphism to a set." $ applyLangMorphismSet @A @B
          , mkValEntry "!@" "Co-apply a morphism to a reference." $ inverseApplyLangMorphismRef @A @BX @BY
          , mkSupertypeEntry "!@" "Co-apply a morphism to a reference." $ inverseApplyLangMorphismRef @A @B @B
          , mkValEntry "!@%" "Co-apply a morphism to an immutable reference. `m !@% r = m !@ immutRef r`" $
            inverseApplyLangMorphismImmutRef @A @B
          , mkValEntry "!@@" "Co-apply a morphism to a set." $ inverseApplyLangMorphismSet @A @BX @BY
          , mkSupertypeEntry "!@@" "Co-apply a morphism to a set." $ inverseApplyLangMorphismSet @A @B @B
          ]
    , docTreeEntry
          "Orders"
          ""
          [ mkValEntry "alphabetical" "Alphabetical order." $ ordOrder @Text
          , mkValEntry "numerical" "Numercal order." $ ordOrder @Number
          , mkValEntry "chronological" "Chronological order." $ ordOrder @UTCTime
          , mkValEntry "durational" "Durational order." $ ordOrder @NominalDiffTime
          , mkValEntry "calendrical" "Date order." $ ordOrder @Day
          , mkValEntry "horological" "Time of day order." $ ordOrder @TimeOfDay
          , mkValEntry "localChronological" "Local time order." $ ordOrder @LocalTime
          , mkValEntry "noOrder" "No order, same as `orders []`." $ noOrder
          , mkValEntry "orders" "Join orders by priority." $ orders @A
          , mkValEntry "mapOrder" "Map a function on an order." (contramap :: (B -> A) -> LangOrder A -> LangOrder B)
          , mkValEntry "orderOn" "Order by an order on a particular morphism." $ orderOn @B @A
          , mkValEntry "rev" "Reverse an order." $ rev @A
          , mkValEntry "orderEQ" "Equal by an order." $ langOrderCompare @A $ (==) EQ
          , mkValEntry "orderLT" "Less than by an order." $ langOrderCompare @A $ (==) LT
          , mkValEntry "orderLE" "Less than or equal to by an order." $ langOrderCompare @A $ (/=) GT
          , mkValEntry "orderGT" "Greater than by an order." $ langOrderCompare @A $ (==) GT
          , mkValEntry "orderGE" "Greater than or equal to by an order." $ langOrderCompare @A $ (/=) LT
          ]
    ]
