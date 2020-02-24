module Pinafore.Language.Predefined.Base
    ( base_predefinitions
    , outputLn
    ) where

import Data.Fixed (div', mod')
import Data.Shim
import Data.Time
import Data.Time.Clock.System
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.If
import Pinafore.Language.Predefined.Defs
import Pinafore.Language.Type.Entity
import Pinafore.Language.Value
import Pinafore.Storage
import Shapes
import Shapes.Numeric
import Truth.Core

getTimeMS :: IO Integer
getTimeMS = do
    MkSystemTime s ns <- getSystemTime
    return $ (toInteger s) * 1000 + div (toInteger ns) 1000000

output :: Text -> PinaforeAction ()
output text = liftIO $ putStr $ unpack text

outputLn :: Text -> PinaforeAction ()
outputLn text = liftIO $ putStrLn $ unpack text

setentity :: PinaforeRef '( A, TopType) -> A -> PinaforeAction ()
setentity ref val = pinaforeRefSet ref (Known val)

deleteentity :: PinaforeRef '( BottomType, TopType) -> PinaforeAction ()
deleteentity ref = pinaforeRefSet ref Unknown

qfail :: Text -> PinaforeAction BottomType
qfail t = fail $ unpack t

entityUUID :: Entity -> Text
entityUUID p = pack $ show p

onStop :: PinaforeAction A -> PinaforeAction A -> PinaforeAction A
onStop p q = p <|> q

newMemRef ::
       forall baseupdate. (?pinafore :: PinaforeContext baseupdate, BaseEditLens MemoryCellUpdate baseupdate)
    => IO (PinaforeRef '( A, A))
newMemRef = do
    lens <- makeMemoryCellEditLens Unknown
    return $ pinaforeValueToRef $ MkPinaforeValue $ mapSubscriber lens $ pinaforeBaseSubscriber @baseupdate

newMemFiniteSet ::
       forall baseupdate. (?pinafore :: PinaforeContext baseupdate, BaseEditLens MemoryCellUpdate baseupdate)
    => IO (PinaforeFiniteSetRef '( MeetType Entity A, A))
newMemFiniteSet = do
    lens <- makeMemoryCellEditLens mempty
    return $
        meetValuePinaforeFiniteSetRef $
        MkPinaforeValue $ mapSubscriber (convertEditLens . lens) $ pinaforeBaseSubscriber @baseupdate

now :: forall baseupdate. (?pinafore :: PinaforeContext baseupdate, BaseEditLens (ROWUpdate UTCTime) baseupdate)
    => PinaforeImmutableReference UTCTime
now = functionImmutableReference $ MkPinaforeValue $ pinaforeBaseSubscriber @baseupdate

timeZone ::
       forall baseupdate. (?pinafore :: PinaforeContext baseupdate, BaseEditLens (ROWUpdate TimeZone) baseupdate)
    => PinaforeImmutableReference TimeZone
timeZone = functionImmutableReference $ MkPinaforeValue $ pinaforeBaseSubscriber @baseupdate

localNow ::
       forall baseupdate.
       ( ?pinafore :: PinaforeContext baseupdate
       , BaseEditLens (ROWUpdate UTCTime) baseupdate
       , BaseEditLens (ROWUpdate TimeZone) baseupdate
       )
    => PinaforeImmutableReference LocalTime
localNow = utcToLocalTime <$> timeZone <*> now

today ::
       forall baseupdate.
       ( ?pinafore :: PinaforeContext baseupdate
       , BaseEditLens (ROWUpdate UTCTime) baseupdate
       , BaseEditLens (ROWUpdate TimeZone) baseupdate
       )
    => PinaforeImmutableReference Day
today = localDay <$> localNow

interpretAsText ::
       forall a. AsLiteral a
    => PinaforeRef '( a, a)
    -> PinaforeRef '( Text, Text)
interpretAsText = pinaforeFLensRef (unLiteral . toLiteral) (\t _ -> parseLiteral t)

parseLiteral :: AsLiteral t => Text -> Maybe t
parseLiteral = knowToMaybe . fromLiteral . MkLiteral

base_predefinitions ::
       forall baseupdate.
       ( HasPinaforeEntityUpdate baseupdate
       , HasPinaforeFileUpdate baseupdate
       , BaseEditLens MemoryCellUpdate baseupdate
       , BaseEditLens (ROWUpdate UTCTime) baseupdate
       , BaseEditLens (ROWUpdate TimeZone) baseupdate
       )
    => [DocTreeEntry (BindDoc baseupdate)]
base_predefinitions =
    [ docTreeEntry
          "Literals & Entities"
          ""
          [ mkValEntry "==" "Entity equality." $ (==) @Entity
          , mkValEntry "/=" "Entity non-equality." $ (/=) @Entity
          , mkValEntry "entityUUID" "UUID of an entity." entityUUID
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
                      , mkValEntry "interpretIntegerAsText" "Interpret an integer reference as text." $
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
                      , mkValEntry "interpretRationalAsText" "Interpret a rational reference as text." $
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
                      , mkValEntry "interpretNumberAsText" "Interpret a number reference as text." $
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
                      , mkValEntry "parseDuration" "Parse text as a duration." $ parseLiteral @NominalDiffTime
                      , mkValEntry "interpretDurationAsText" "Interpret a duration reference as text." $
                        interpretAsText @NominalDiffTime
                      , mkValEntry "zeroDurtaion" "No duration." $ (0 :: NominalDiffTime)
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
                      , mkValEntry "parseTime" "Parse text as a time." $ parseLiteral @UTCTime
                      , mkValEntry "interpretTimeAsText" "Interpret a time reference as text." $
                        interpretAsText @UTCTime
                      , mkValEntry "addTime" "Add duration to time." addUTCTime
                      , mkValEntry "diffTime" "Difference of times." diffUTCTime
                      , mkValEntry "now" "The current time truncated to the second." $ now @baseupdate
                      ]
                , docTreeEntry
                      "Calendar"
                      ""
                      [ mkValPatEntry "Day" "Construct a Day from year, month, day." fromGregorian $ \day -> let
                            (y, m, d) = toGregorian day
                            in Just (y, (m, (d, ())))
                      , mkSupertypeEntry "id" "Every day is a literal." $ toLiteral @Day
                      , mkValEntry "parseDay" "Parse text as a day." $ parseLiteral @Day
                      , mkValEntry "interpretDayAsText" "Interpret a day reference as text." $ interpretAsText @Day
                      , mkValEntry "dayToModifiedJulian" "Convert to MJD." toModifiedJulianDay
                      , mkValEntry "modifiedJulianToDay" "Convert from MJD." ModifiedJulianDay
                      , mkValEntry "addDays" "Add count to days." addDays
                      , mkValEntry "diffDays" "Difference of days." diffDays
                      , mkValEntry "utcDay" "The current UTC day." $ fmap utctDay $ now @baseupdate
                      , mkValEntry "today" "The current local day." $ today @baseupdate
                      ]
                , docTreeEntry
                      "Time of Day"
                      ""
                      [ mkValPatEntry "TimeOfDay" "Construct a TimeOfDay from hour, minute, second." TimeOfDay $ \TimeOfDay {..} ->
                            Just (todHour, (todMin, (todSec, ())))
                      , mkSupertypeEntry "id" "Every time of day is a literal." $ toLiteral @TimeOfDay
                      , mkValEntry "parseTimeOfDay" "Parse text as a time of day." $ parseLiteral @TimeOfDay
                      , mkValEntry "interpretTimeOfDayAsText" "Interpret a time of day reference as text." $
                        interpretAsText @TimeOfDay
                      , mkValEntry "midnight" "Midnight." midnight
                      , mkValEntry "midday" "Midday." midday
                      ]
                , docTreeEntry
                      "Local Time"
                      ""
                      [ mkValPatEntry "LocalTime" "Construct a LocalTime from day and time of day." LocalTime $ \LocalTime {..} ->
                            Just (localDay, (localTimeOfDay, ()))
                      , mkSupertypeEntry "id" "Every local time is a literal." $ toLiteral @LocalTime
                      , mkValEntry "parseLocalTime" "Parse text as a local time." $ parseLiteral @LocalTime
                      , mkValEntry "interpretLocalTimeAsText" "Interpret a local time reference as text." $
                        interpretAsText @LocalTime
                      , mkValEntry "timeToLocal" "Convert a time to local time, given a time zone offset in minutes" $ \i ->
                            utcToLocalTime $ minutesToTimeZone i
                      , mkValEntry "localToTime" "Convert a local time to time, given a time zone offset in minutes" $ \i ->
                            localTimeToUTC $ minutesToTimeZone i
                      , mkValEntry "getTimeZone" "Get the offset for a time in the current time zone." $ \t ->
                            fmap timeZoneMinutes $ getTimeZone t
                      , mkValEntry "timeZone" "The current time zone offset in minutes." $
                        fmap timeZoneMinutes $ timeZone @baseupdate
                      , mkValEntry "localNow" "The current local time." $ localNow @baseupdate
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
            entityAdapter $
            MkEntityType MaybeEntityGroundType $
            ConsArguments (MkEntityType TopEntityGroundType NilArguments) NilArguments
          ]
    , docTreeEntry
          "Pairs"
          ""
          [ mkSupertypeEntry "id" "Entity conversion." $
            entityAdapterConvert $
            entityAdapter $
            MkEntityType PairEntityGroundType $
            ConsArguments (MkEntityType TopEntityGroundType NilArguments) $
            ConsArguments (MkEntityType TopEntityGroundType NilArguments) NilArguments
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
            entityAdapter $
            MkEntityType EitherEntityGroundType $
            ConsArguments (MkEntityType TopEntityGroundType NilArguments) $
            ConsArguments (MkEntityType TopEntityGroundType NilArguments) NilArguments
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
          , mkValPatEntry ":" "Construct a list" ((:) @A) $ \(v :: [A]) ->
                case v of
                    a:b -> Just (a, (b, ()))
                    _ -> Nothing
          , mkSupertypeEntry "id" "Entity conversion." $
            entityAdapterConvert $
            entityAdapter $
            MkEntityType ListEntityGroundType $
            ConsArguments (MkEntityType TopEntityGroundType NilArguments) NilArguments
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
          ]
    , docTreeEntry
          "Undo"
          "Undo and redo changes."
          [ mkValEntry "queueUndo" "Undo an action." $ do
                ua <- pinaforeUndoActions
                rc <- pinaforeResourceContext
                liftIO $ uaUndo ua rc noEditSource
          , mkValEntry "queueRedo" "Redo an action." $ do
                ua <- pinaforeUndoActions
                rc <- pinaforeResourceContext
                liftIO $ uaRedo ua rc noEditSource
          ]
    , docTreeEntry
          "References"
          "A reference of type `Ref {-p,+q}` has a setting type of `p` and a getting type of `q`. References keep track of updates, and will update user interfaces constructed from them when their value changes."
          [ mkValEntry "pureRef" "A constant reference for a value." (pure :: A -> PinaforeImmutableReference A)
          , mkValEntry
                "immutRef"
                "Convert a reference to immutable.\n`immutRef r = {%r}`"
                (id :: PinaforeImmutableReference A -> PinaforeImmutableReference A)
          , mkValEntry
                "coMapRef"
                "Map a function on getting a reference."
                (coRangeLift :: (A -> B) -> PinaforeRef '( C, A) -> PinaforeRef '( C, B))
          , mkValEntry
                "contraMapRef"
                "Map a function on setting a reference."
                (contraRangeLift :: (B -> A) -> PinaforeRef '( A, C) -> PinaforeRef '( B, C))
          , mkValEntry "lensMapRef" "Map getter & pushback functions on a reference." $ pinaforeFLensRef @AP @AQ @B
          , mkValEntry
                "applyRef"
                "Combine references."
                ((<*>) :: PinaforeImmutableReference (A -> B) -> PinaforeImmutableReference A -> PinaforeImmutableReference B)
          , mkValEntry
                "unknown"
                "The unknown reference, representing missing information."
                (empty :: PinaforeImmutableReference BottomType)
          , mkValEntry "known" "True if the reference is known." $ \(val :: PinaforeReadOnlyValue (Know TopType)) ->
                (eaMapReadOnlyWhole (Known . isKnown) val :: PinaforeReadOnlyValue (Know Bool))
          , mkValEntry
                "??"
                "`p ?? q` = `p` if it is known, else `q`."
                ((<|>) :: PinaforeImmutableReference A -> PinaforeImmutableReference A -> PinaforeImmutableReference A)
          , mkValEntry "get" "Get a reference, or `stop` if the reference is unknown." $ pinaforeRefGet @A
          , mkValEntry "runRef" "Run an action from a reference." $ runPinaforeRef
          , mkValEntry ":=" "Set a reference to a value. Stop if failed." setentity
          , mkValEntry "delete" "Delete an entity reference. Stop if failed." deleteentity
          , mkValEntry "newMemRef" "Create a new reference to memory, initially unknown." $ newMemRef @baseupdate
          ]
    , docTreeEntry
          "Set References"
          ""
          [ mkValEntry
                "mapSet"
                "Map a function on a set."
                (contramap :: (A -> B) -> PinaforeSetRef B -> PinaforeSetRef A)
          , mkValEntry "pureSet" "Convert a predicate to a set." $ pinaforePredicateToSetRef @A
          , mkValEntry "refSet" "Convert a predicate reference to a set." $ pinaforePredicateRefToSetRef @A
          , mkValEntry "immutSet" "Convert a set to immutable." $ pinaforeSetRefImmutable @A
          , mkValEntry "+=" "Add an entity to a set." $ pinaforeSetRefAdd @A
          , mkValEntry "-=" "Remove an entity from a set." $ pinaforeSetRefRemove @A
          , mkValEntry "newEntity" "Create a new entity in a set and act on it." $ pinaforeSetRefAddNew
          , mkValEntry "member" "A reference to the membership of a value in a set." $ pinaforeSetRefMember @A
          , mkValEntry
                "notSet"
                "Complement of a set. The resulting set can be added to (deleting from the original set) and deleted from (adding to the original set)." $
            pinaforeSetRefComplement @A
          , mkValEntry
                "<&>"
                "Intersection of sets. The resulting set can be added to (adding to both sets), but not deleted from." $
            pinaforeSetRefIntersect @A
          , mkValEntry
                "<|>"
                "Union of sets. The resulting set can be deleted from (deleting from both sets), but not added to." $
            pinaforeSetRefUnion @A
          , mkValEntry
                "<\\>"
                "Difference of sets, everything in the first set but not the second. The resulting set can be added to (adding to the first and deleting from the second), but not deleted from." $
            pinaforeSetRefDifference @A
          , mkValEntry
                "<^>"
                "Symmetric difference of sets, everything in exactly one of the sets. The resulting set will be read-only." $
            pinaforeSetRefSymmetricDifference @A
          , mkValEntry "<+>" "Cartesian sum of sets." $ pinaforeSetRefCartesianSum @A @B
          , mkValEntry "<*>" "Cartesian product of sets. The resulting set will be read-only." $
            pinaforeSetRefCartesianProduct @A @B
          ]
    , docTreeEntry
          "Finite Set References"
          ""
          [ mkSupertypeEntry "id" "Every finite set is a set." $ pinaforeFiniteSetRefToSetRef @A @TopType
          , mkValEntry
                "coMapFiniteSet"
                "Map a function on getting from a finite set."
                (coRangeLift :: (A -> B) -> PinaforeFiniteSetRef '( C, A) -> PinaforeFiniteSetRef '( C, B))
          , mkValEntry
                "contraMapFiniteSet"
                "Map a function on setting to and testing a finite set."
                (contraRangeLift :: (B -> A) -> PinaforeFiniteSetRef '( A, C) -> PinaforeFiniteSetRef '( B, C))
          , mkValEntry "<:&>" "Intersect a finite set with any set. The resulting finite set will be read-only." $
            pinaforeFiniteSetRefSetIntersect @A @B
          , mkValEntry "<:\\>" "Difference of a finite set and any set. The resulting finite set will be read-only." $
            pinaforeFiniteSetRefSetDifference @A @B
          , mkValEntry
                "<:&:>"
                "Intersection of finite sets. The resulting finite set can be added to, but not deleted from." $
            pinaforeFiniteSetRefMeet @A
          , mkValEntry "<:|:>" "Union of finite sets. The resulting finite set can be deleted from, but not added to." $
            pinaforeFiniteSetRefJoin @A
          , mkValEntry "<:+:>" "Cartesian sum of finite sets." $ pinaforeFiniteSetRefCartesianSum @AP @AQ @BP @BQ
          , mkSupertypeEntry "<:+:>" "Cartesian sum of finite sets." $ pinaforeFiniteSetRefCartesianSum @A @A @B @B
          , mkValEntry "<:*:>" "Cartesian product of finite sets. The resulting finite set will be read-only." $
            pinaforeFiniteSetRefCartesianProduct @AP @AQ @BP @BQ
          , mkSupertypeEntry "<:*:>" "Cartesian product of finite sets. The resulting finite set will be read-only." $
            pinaforeFiniteSetRefCartesianProduct @A @A @B @B
          , mkValEntry "members" "Get all members of a finite set, by an order." $ pinaforeSetGetOrdered @baseupdate @A
          , mkValEntry "single" "The member of a single-member finite set, or unknown." $ pinaforeFiniteSetRefSingle @A
          , mkValEntry "count" "Count of members in a finite set." $ pinaforeFiniteSetRefFunc @TopType @Int olength
          , mkValEntry
                "removeAll"
                "Remove all entities from a finite set."
                (pinaforeFiniteSetRefRemoveAll :: PinaforeFiniteSetRef '( BottomType, TopType) -> PinaforeAction ())
          , mkValEntry "newMemFiniteSet" "Create a new finite set reference to memory, initially empty." $
            newMemFiniteSet @baseupdate
          ]
    , docTreeEntry
          "Morphisms"
          "Morphisms relate entities."
          [ mkValEntry "identity" "The identity morphism." $ identityPinaforeMorphism @baseupdate @A
          , mkValEntry "!." "Compose morphisms." $ composePinaforeMorphism @baseupdate @AP @AQ @BP @BQ @CP @CQ
          , mkSupertypeEntry "!." "Compose morphisms." $ composePinaforeMorphism @baseupdate @A @A @B @B @C @C
          , mkValEntry "!**" "Pair morphisms. References from these morphisms are undeleteable." $
            pairPinaforeMorphism @baseupdate @AP @AQ @BP @BQ @CP @CQ
          , mkSupertypeEntry "!**" "Pair morphisms. References from these morphisms are undeleteable." $
            pairPinaforeMorphism @baseupdate @A @A @B @B @C @C
          , mkValEntry "!++" "Either morphisms. References from these morphisms are undeleteable." $
            eitherPinaforeMorphism @baseupdate @AP @AQ @BP @BQ @CP @CQ
          , mkSupertypeEntry "!++" "Either morphisms. References from these morphisms are undeleteable." $
            eitherPinaforeMorphism @baseupdate @A @A @B @B @C @C
          , mkValEntry "!$" "Apply a morphism to a reference." $ pinaforeApplyMorphismRef @baseupdate @AP @AQ @BP @BQ
          , mkSupertypeEntry "!$" "Apply a morphism to a reference." $ pinaforeApplyMorphismRef @baseupdate @A @A @B @B
          , mkValEntry "!$%" "Apply a morphism to an immutable reference. `m !$% r = m !$ immutRef r`" $
            pinaforeApplyMorphismImmutRef @baseupdate @A @BP @BQ
          , mkSupertypeEntry "!$%" "Apply a morphism to an immutable reference. `m !$% r = m !$ immutRef r`" $
            pinaforeApplyMorphismImmutRef @baseupdate @A @B @B
          , mkValEntry "!$$" "Apply a morphism to a set." $ pinaforeApplyMorphismSet @baseupdate @A @BP @BQ
          , mkSupertypeEntry "!$$" "Apply a morphism to a set." $ pinaforeApplyMorphismSet @baseupdate @A @B @B
          , mkValEntry "!@" "Co-apply a morphism to a reference." $
            pinaforeApplyInverseMorphismRef @baseupdate @AP @AQ @BP @BQ
          , mkSupertypeEntry "!@" "Co-apply a morphism to a reference." $
            pinaforeApplyInverseMorphismRef @baseupdate @A @A @B @B
          , mkValEntry "!@%" "Co-apply a morphism to an immutable reference. `m !@% r = m !@ immutRef r`" $
            pinaforeApplyInverseMorphismImmutRef @baseupdate @A @BP @BQ
          , mkSupertypeEntry "!@%" "Co-apply a morphism to a reference. `m !@% r = m !@ immutRef r`" $
            pinaforeApplyInverseMorphismImmutRef @baseupdate @A @B @B
          , mkValEntry "!@@" "Co-apply a morphism to a set." $
            pinaforeApplyInverseMorphismSet @baseupdate @AP @AQ @BP @BQ
          , mkSupertypeEntry "!@@" "Co-apply a morphism to a set." $
            pinaforeApplyInverseMorphismSet @baseupdate @A @A @B @B
          ]
    , docTreeEntry
          "Orders"
          ""
          [ mkValEntry "alphabetical" "Alphabetical order." $ ordOrder @baseupdate @Text
          , mkValEntry "numerical" "Numercal order." $ ordOrder @baseupdate @Number
          , mkValEntry "chronological" "Chronological order." $ ordOrder @baseupdate @UTCTime
          , mkValEntry "durational" "Durational order." $ ordOrder @baseupdate @NominalDiffTime
          , mkValEntry "calendrical" "Day order." $ ordOrder @baseupdate @Day
          , mkValEntry "horological" "Time of day order." $ ordOrder @baseupdate @TimeOfDay
          , mkValEntry "localChronological" "Local time order." $ ordOrder @baseupdate @LocalTime
          , mkValEntry "noOrder" "No order, same as `orders []`." $ noOrder @baseupdate
          , mkValEntry "orders" "Join orders by priority." $ orders @baseupdate @A
          , mkValEntry
                "mapOrder"
                "Map a function on an order."
                (contramap :: (B -> A) -> PinaforeOrder baseupdate A -> PinaforeOrder baseupdate B)
          , mkValEntry "orderOn" "Order by an order on a particular morphism." $ orderOn @baseupdate @B @A
          , mkValEntry "rev" "Reverse an order." $ rev @baseupdate @A
          , mkValEntry "orderEQ" "Equal by an order." $ pinaforeOrderCompare @baseupdate @A $ (==) EQ
          , mkValEntry "orderLT" "Less than by an order." $ pinaforeOrderCompare @baseupdate @A $ (==) LT
          , mkValEntry "orderLE" "Less than or equal to by an order." $ pinaforeOrderCompare @baseupdate @A $ (/=) GT
          , mkValEntry "orderGT" "Greater than by an order." $ pinaforeOrderCompare @baseupdate @A $ (==) GT
          , mkValEntry "orderGE" "Greater than or equal to by an order." $ pinaforeOrderCompare @baseupdate @A $ (/=) LT
          ]
    ]
