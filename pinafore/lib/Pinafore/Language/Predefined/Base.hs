module Pinafore.Language.Predefined.Base
    ( base_predefinitions
    , outputLn
    ) where

import Data.Fixed (div', mod')
import Data.Shim
import Data.Time
import Data.Time.Clock.System
import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.If
import Pinafore.Language.Predefined.Defs
import Pinafore.Language.Value
import Pinafore.Storage
import Shapes
import Shapes.Numeric
import Truth.Core

getTimeMS :: IO Integer
getTimeMS = do
    MkSystemTime s ns <- getSystemTime
    return $ (toInteger s) * 1000 + div (toInteger ns) 1000000

output :: forall baseupdate. Text -> PinaforeAction baseupdate ()
output text = liftIO $ putStr $ unpack text

outputLn :: forall baseupdate. Text -> PinaforeAction baseupdate ()
outputLn text = liftIO $ putStrLn $ unpack text

setentity :: forall baseupdate. PinaforeRef baseupdate '( A, TopType) -> A -> PinaforeAction baseupdate ()
setentity ref val = pinaforeRefSet ref (Known val)

deleteentity :: forall baseupdate. PinaforeRef baseupdate '( BottomType, TopType) -> PinaforeAction baseupdate ()
deleteentity ref = pinaforeRefSet ref Unknown

qfail :: forall baseupdate. Text -> PinaforeAction baseupdate BottomType
qfail t = fail $ unpack t

entityUUID :: Entity -> Text
entityUUID p = pack $ show p

onStop :: forall baseupdate. PinaforeAction baseupdate A -> PinaforeAction baseupdate A -> PinaforeAction baseupdate A
onStop p q = q <|> p

newMemRef ::
       forall baseupdate. BaseEditLens MemoryCellUpdate baseupdate
    => IO (PinaforeRef baseupdate '( A, A))
newMemRef = do
    lens <- makeMemoryCellEditLens Unknown
    return $ pinaforeLensToRef $ lens . baseEditLens

newMemFiniteSet ::
       forall baseupdate. BaseEditLens MemoryCellUpdate baseupdate
    => IO (PinaforeFiniteSetRef baseupdate '( MeetType Entity A, A))
newMemFiniteSet = do
    lens <- makeMemoryCellEditLens mempty
    return $ meetValuePinaforeFiniteSetRef $ convertEditLens . lens . baseEditLens

now :: forall baseupdate. (BaseEditLens (WholeUpdate UTCTime) baseupdate)
    => PinaforeImmutableReference baseupdate UTCTime
now = functionImmutableReference $ editLensFunction $ baseEditLens @(WholeUpdate UTCTime) @baseupdate

timeZone ::
       forall baseupdate. (BaseEditLens (WholeUpdate TimeZone) baseupdate)
    => PinaforeImmutableReference baseupdate TimeZone
timeZone = functionImmutableReference $ editLensFunction $ baseEditLens @(WholeUpdate TimeZone) @baseupdate

localNow ::
       forall baseupdate.
       (BaseEditLens (WholeUpdate UTCTime) baseupdate, BaseEditLens (WholeUpdate TimeZone) baseupdate)
    => PinaforeImmutableReference baseupdate LocalTime
localNow = utcToLocalTime <$> timeZone <*> now

today ::
       forall baseupdate.
       (BaseEditLens (WholeUpdate UTCTime) baseupdate, BaseEditLens (WholeUpdate TimeZone) baseupdate)
    => PinaforeImmutableReference baseupdate Day
today = localDay <$> localNow

base_predefinitions ::
       forall baseupdate.
       ( HasPinaforeEntityUpdate baseupdate
       , HasPinaforeFileUpdate baseupdate
       , BaseEditLens MemoryCellUpdate baseupdate
       , BaseEditLens (WholeUpdate UTCTime) baseupdate
       , BaseEditLens (WholeUpdate TimeZone) baseupdate
       )
    => [DocTreeEntry (BindDoc baseupdate)]
base_predefinitions =
    [ docTreeEntry
          "Literals & Entities"
          ""
          [ mkValEntry "is" "Entity equality." $ (==) @Entity
          , mkValEntry "==" "Literal equality. Same as Entity equality restricted to Literal, but faster." $
            (==) @Literal
          , mkValEntry "/=" "Literal non-equality." $ (/=) @Literal
          , mkValEntry "entityUUID" "UUID of an entity." entityUUID
          , mkValEntry "toText" "The text of a literal." unLiteral
          , docTreeEntry
                "Boolean"
                ""
                [ mkValPatEntry "True" "Boolean TRUE." True $ \v ->
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
                [ mkValEntry "<>" "Concatenate text." $ (<>) @Text
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
                      ]
                , docTreeEntry
                      "Rational"
                      ""
                      [ mkValEntry ".+" "Add." $ (+) @Rational
                      , mkValEntry ".-" "Subtract." $ (-) @Rational
                      , mkValEntry ".*" "Multiply." $ (*) @Rational
                      , mkValEntry "/" "Divide." $ (/) @Rational
                      , mkValEntry "negateR" "Negate." $ negate @Rational
                      , mkValEntry "recip" "Reciprocal." $ recip @Rational
                      , mkValEntry "absR" "Absolute value." $ abs @Rational
                      , mkValEntry "signumR" "Sign." $ signum @Rational
                      , mkValEntry "modR" "Modulus, leftover from `div`" $ mod' @Rational
                      , mkValEntry "^^" "Raise to Integer power." $ ((^^) :: Rational -> Integer -> Rational)
                      ]
                , docTreeEntry
                      "Number"
                      ""
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
                      , mkValEntry
                            "checkExactRational"
                            "Get the exact value of a Number, if it is one."
                            checkExactRational
                      , mkValEntry
                            "checkExactInteger"
                            "Get the exact Integer value of a Number, if it is one. Works as expected on Rationals." $ \n ->
                            checkExactRational n >>= rationalInteger
                      ]
                ]
          , docTreeEntry
                "Date & Time"
                ""
                [ docTreeEntry
                      "Time & Duration"
                      ""
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
                      , mkValEntry "midnight" "Midnight." midnight
                      , mkValEntry "midday" "Midday." midday
                      ]
                , docTreeEntry
                      "Local Time"
                      ""
                      [ mkValPatEntry "LocalTime" "Construct a LocalTime from day and time of day." LocalTime $ \LocalTime {..} ->
                            Just (localDay, (localTimeOfDay, ()))
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
          ]
    , docTreeEntry
          "Pairs"
          ""
          [ mkValEntry "fst" "Get the first member of a pair." $ fst @A @B
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
          [ mkValEntry "return" "A value as an Action." $ return @(PinaforeAction baseupdate) @A
          , mkValEntry ">>=" "Bind the result of an Action to an Action." $ qbind @baseupdate
          , mkValEntry ">>" "Do actions in sequence." $ qbind_ @baseupdate
          , mkValEntry "fixAction" "The fixed point of an Action." $ mfix @(PinaforeAction baseupdate) @A
          , mkValEntry "fail" "Fail, causing the program to terminate with error." $ qfail @baseupdate
          , mkValEntry
                "stop"
                "Stop. This is similar to an exception that can be caught with `onStop`. The default handler (for the main program, button presses, etc.), is to catch and ignore it."
                (empty :: PinaforeAction baseupdate BottomType)
          , mkValEntry "onStop" "`onStop p q` does `q` first, and if it stops, then does `p`." $ onStop @baseupdate
          , mkValEntry
                "for_"
                "Perform an action on each value of a list."
                (for_ :: [A] -> (A -> PinaforeAction baseupdate ()) -> PinaforeAction baseupdate ())
          , mkValEntry
                "for"
                "Perform an action on each value of a list, returning a list."
                (for :: [A] -> (A -> PinaforeAction baseupdate B) -> PinaforeAction baseupdate [B])
          , mkValEntry "output" "Output text to standard output." $ output @baseupdate
          , mkValEntry "outputLn" "Output text and a newline to standard output." $ outputLn @baseupdate
          , mkValEntry
                "getTimeMS"
                "Get the time as a whole number of milliseconds."
                (liftIO getTimeMS :: PinaforeAction baseupdate Integer)
          ]
    , docTreeEntry
          "Undo"
          "Undo and redo changes."
          [ mkValEntry "queueUndo" "Undo an action." $ do
                ua <- pinaforeUndoActions
                liftIO $ uaUndo ua noEditSource
          , mkValEntry "queueRedo" "Redo an action." $ do
                ua <- pinaforeUndoActions
                liftIO $ uaRedo ua noEditSource
          ]
    , docTreeEntry
          "References"
          "A reference of type `Ref {-p,+q}` has a setting type of `p` and a getting type of `q`. References keep track of updates, and will update user interfaces constructed from them when their value changes."
          [ mkValEntry
                "pureRef"
                "A constant reference for a value."
                (pure :: A -> PinaforeImmutableReference baseupdate A)
          , mkValEntry
                "immutRef"
                "Convert a reference to immutable.\n`immutRef r = {%r}`"
                (id :: PinaforeImmutableReference baseupdate A -> PinaforeImmutableReference baseupdate A)
          , mkValEntry
                "coMapRef"
                "Map a function on getting a reference."
                (coRangeLift :: (A -> B) -> PinaforeRef baseupdate '( C, A) -> PinaforeRef baseupdate '( C, B))
          , mkValEntry
                "contraMapRef"
                "Map a function on setting a reference."
                (contraRangeLift :: (B -> A) -> PinaforeRef baseupdate '( A, C) -> PinaforeRef baseupdate '( B, C))
          , mkValEntry "lensMapRef" "Map getter & pushback functions on a reference." $
            pinaforeFLensRef @baseupdate @AP @AQ @B
          , mkValEntry
                "applyRef"
                "Combine references."
                ((<*>) :: PinaforeImmutableReference baseupdate (A -> B) -> PinaforeImmutableReference baseupdate A -> PinaforeImmutableReference baseupdate B)
          , mkValEntry
                "unknown"
                "The unknown reference, representing missing information."
                (empty :: PinaforeImmutableReference baseupdate BottomType)
          , mkValEntry "known" "True if the reference is known." $ \(val :: PinaforeFunctionValue baseupdate (Know TopType)) ->
                (funcUpdateFunction (Known . isKnown) . val :: PinaforeFunctionValue baseupdate (Know Bool))
          , mkValEntry
                "??"
                "`p ?? q` = `p` if it is known, else `q`."
                ((<|>) :: PinaforeImmutableReference baseupdate A -> PinaforeImmutableReference baseupdate A -> PinaforeImmutableReference baseupdate A)
          , mkValEntry "get" "Get a reference, or `stop` if the reference is unknown." $ pinaforeRefGet @baseupdate @A
          , mkValEntry "runRef" "Run an action from a reference." $ runPinaforeRef @baseupdate
          , mkValEntry ":=" "Set a reference to a value. Stop if failed." $ setentity @baseupdate
          , mkValEntry "delete" "Delete an entity reference. Stop if failed." $ deleteentity @baseupdate
          , mkValEntry "newMemRef" "Create a new reference to memory, initially unknown." $ newMemRef @baseupdate
          ]
    , docTreeEntry
          "Set References"
          ""
          [ mkValEntry
                "mapSet"
                "Map a function on a set."
                (contramap :: (A -> B) -> PinaforeSetRef baseupdate B -> PinaforeSetRef baseupdate A)
          , mkValEntry "pureSet" "Convert a predicate to a set." $ pinaforePredicateToSetRef @baseupdate @A
          , mkValEntry "refSet" "Convert a predicate reference to a set." $ pinaforePredicateRefToSetRef @baseupdate @A
          , mkValEntry "immutSet" "Convert a set to immutable." $ pinaforeSetRefImmutable @baseupdate @A
          , mkValEntry "+=" "Add an entity to a set." $ pinaforeSetRefAdd @baseupdate @A
          , mkValEntry "-=" "Remove an entity from a set." $ pinaforeSetRefRemove @baseupdate @A
          , mkValEntry "newEntity" "Create a new entity in a set and act on it." $ pinaforeSetRefAddNew @baseupdate
          , mkValEntry "member" "A reference to the membership of a value in a set." $
            pinaforeSetRefMember @baseupdate @A
          , mkValEntry
                "notSet"
                "Complement of a set. The resulting set can be added to (deleting from the original set) and deleted from (adding to the original set)." $
            pinaforeSetRefComplement @baseupdate @A
          , mkValEntry
                "<&>"
                "Intersection of sets. The resulting set can be added to (adding to both sets), but not deleted from." $
            pinaforeSetRefIntersect @baseupdate @A
          , mkValEntry
                "<|>"
                "Union of sets. The resulting set can be deleted from (deleting from both sets), but not added to." $
            pinaforeSetRefUnion @baseupdate @A
          , mkValEntry
                "<\\>"
                "Difference of sets, everything in the first set but not the second. The resulting set can be added to (adding to the first and deleting from the second), but not deleted from." $
            pinaforeSetRefDifference @baseupdate @A
          , mkValEntry
                "<^>"
                "Symmetric difference of sets, everything in exactly one of the sets. The resulting set will be read-only." $
            pinaforeSetRefSymmetricDifference @baseupdate @A
          , mkValEntry "<+>" "Cartesian sum of sets." $ pinaforeSetRefCartesianSum @baseupdate @A @B
          , mkValEntry "<*>" "Cartesian product of sets. The resulting set will be read-only." $
            pinaforeSetRefCartesianProduct @baseupdate @A @B
          ]
    , docTreeEntry
          "Finite Set References"
          ""
          [ mkValEntry
                "coMapFiniteSet"
                "Map a function on getting from a finite set."
                (coRangeLift :: (A -> B) -> PinaforeFiniteSetRef baseupdate '( C, A) -> PinaforeFiniteSetRef baseupdate '( C, B))
          , mkValEntry
                "contraMapFiniteSet"
                "Map a function on setting to and testing a finite set."
                (contraRangeLift :: (B -> A) -> PinaforeFiniteSetRef baseupdate '( A, C) -> PinaforeFiniteSetRef baseupdate '( B, C))
          , mkValEntry "<:&>" "Intersect a finite set with any set. The resulting finite set will be read-only." $
            pinaforeFiniteSetRefSetIntersect @baseupdate @A @B
          , mkValEntry "<:\\>" "Difference of a finite set and any set. The resulting finite set will be read-only." $
            pinaforeFiniteSetRefSetDifference @baseupdate @A @B
          , mkValEntry
                "<:&:>"
                "Intersection of finite sets. The resulting finite set can be added to, but not deleted from." $
            pinaforeFiniteSetRefMeet @baseupdate @A
          , mkValEntry "<:|:>" "Union of finite sets. The resulting finite set can be deleted from, but not added to." $
            pinaforeFiniteSetRefJoin @baseupdate @A
          , mkValEntry "<:+:>" "Cartesian sum of finite sets." $
            pinaforeFiniteSetRefCartesianSum @baseupdate @AP @AQ @BP @BQ
          , mkValEntry "<:*:>" "Cartesian product of finite sets. The resulting finite set will be read-only." $
            pinaforeFiniteSetRefCartesianProduct @baseupdate @AP @AQ @BP @BQ
          , mkValEntry "members" "Get all members of a finite set, by an order." $ pinaforeSetGetOrdered @baseupdate @A
          , mkValEntry "single" "The member of a single-member finite set, or unknown." $
            pinaforeFiniteSetRefSingle @baseupdate @A
          , mkValEntry "count" "Count of members in a finite set." $
            pinaforeFiniteSetRefFunc @baseupdate @TopType @Int olength
          , mkValEntry
                "removeAll"
                "Remove all entities from a finite set."
                (pinaforeFiniteSetRefRemoveAll :: PinaforeFiniteSetRef baseupdate '( BottomType, TopType) -> PinaforeAction baseupdate ())
          , mkValEntry "newMemFiniteSet" "Create a new finite set reference to memory, initially empty." $
            newMemFiniteSet @baseupdate
          ]
    , docTreeEntry
          "Morphisms"
          "Morphisms relate entities."
          [ mkValEntry "identity" "The identity morphism." $ identityPinaforeMorphism @baseupdate @A
          , mkValEntry "!." "Compose morphisms." $ composePinaforeMorphism @baseupdate @AP @AQ @BP @BQ @CP @CQ
          , mkValEntry "!**" "Pair morphisms. References from these morphisms are undeleteable." $
            pairPinaforeMorphism @baseupdate @AP @AQ @BP @BQ @CP @CQ
          , mkValEntry "!++" "Either morphisms. References from these morphisms are undeleteable." $
            eitherPinaforeMorphism @baseupdate @AP @AQ @BP @BQ @CP @CQ
          , mkValEntry "!$" "Apply a morphism to a reference." $ pinaforeApplyMorphismRef @baseupdate @AP @AQ @BP @BQ
          , mkValEntry "!$$" "Apply a morphism to a set." $ pinaforeApplyMorphismSet @baseupdate @A @BP @BQ
          , mkValEntry "!@" "Co-apply a morphism to a reference." $
            pinaforeApplyInverseMorphismRef @baseupdate @AP @AQ @BP @BQ
          , mkValEntry "!@@" "Co-apply a morphism to a set." $
            pinaforeApplyInverseMorphismSet @baseupdate @AP @AQ @BP @BQ
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
