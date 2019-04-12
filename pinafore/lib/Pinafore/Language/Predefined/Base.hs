module Pinafore.Language.Predefined.Base
    ( base_predefinitions
    , outputln
    ) where

import Data.Fixed (div', mod')
import Data.Time.Clock.System
import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.If
import Pinafore.Language.Morphism
import Pinafore.Language.Order
import Pinafore.Language.Predefined.Defs
import Pinafore.Language.Reference
import Pinafore.Language.Set
import Pinafore.Language.Type
import Pinafore.Storage.File
import Shapes
import Shapes.Numeric
import Truth.Core
import Truth.Debug

gettimems :: IO Integer
gettimems = do
    MkSystemTime s ns <- getSystemTime
    return $ (toInteger s) * 1000 + div (toInteger ns) 1000000

output :: forall baseedit. Text -> PinaforeAction baseedit ()
output text = liftIO $ putStr $ unpack text

outputln :: forall baseedit. Text -> PinaforeAction baseedit ()
outputln text = liftIO $ putStrLn $ unpack text

setentity :: forall baseedit. PinaforeReference baseedit '( A, TopType) -> A -> PinaforeAction baseedit ()
setentity ref val = pinaforeReferenceSet ref (Known val)

deleteentity :: forall baseedit. PinaforeReference baseedit '( BottomType, TopType) -> PinaforeAction baseedit ()
deleteentity ref = pinaforeReferenceSet ref Unknown

qfail :: forall baseedit. Text -> PinaforeAction baseedit BottomType
qfail t = fail $ unpack t

entityuuid :: Entity -> Text
entityuuid p = pack $ show p

onstop :: forall baseedit. PinaforeAction baseedit A -> PinaforeAction baseedit A -> PinaforeAction baseedit A
onstop p q = q <|> p

newmemref ::
       forall baseedit. BaseEditLens MemoryCellEdit baseedit
    => IO (PinaforeReference baseedit '( A, A))
newmemref = do
    lens <- makeMemoryCellEditLens Unknown
    return $ pinaforeLensToReference $ lens . baseEditLens

newmemset ::
       forall baseedit. BaseEditLens MemoryCellEdit baseedit
    => IO (PinaforeSet baseedit '( MeetType Entity A, A))
newmemset = do
    lens <- makeMemoryCellEditLens mempty
    return $ meetValuePinaforeSet $ convertEditLens . lens . baseEditLens

base_predefinitions ::
       forall baseedit.
       (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit, BaseEditLens MemoryCellEdit baseedit)
    => [DocTreeEntry (BindDoc baseedit)]
base_predefinitions =
    [ docTreeEntry
          "Literals & Entities"
          ""
          [ mkValEntry "is" "Entity equality." $ (==) @Entity
          , mkValEntry "==" "Literal equality. Same as Entity equality restricted to Literal, but faster." $
            (==) @Literal
          , mkValEntry "/=" "Literal non-equality." $ (/=) @Literal
          , mkValEntry "entityuuid" "UUID of an entity." entityuuid
          , mkValEntry "totext" "The text of a literal." unLiteral
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
          , docTreeEntry "Text" "" [mkValEntry "<>" "Concatenate text." $ (<>) @Text]
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
                      , mkValEntry "rnegate" "Negate." $ negate @Rational
                      , mkValEntry "recip" "Reciprocal." $ recip @Rational
                      , mkValEntry "rabs" "Absolute value." $ abs @Rational
                      , mkValEntry "rsignum" "Sign." $ signum @Rational
                      , mkValEntry "rmod" "Modulus, leftover from `div`" $ mod' @Rational
                      , mkValEntry "^^" "Raise to Integer power." $ ((^^) :: Rational -> Integer -> Rational)
                      ]
                , docTreeEntry
                      "Number"
                      ""
                      [ mkValEntry "~+" "Add." $ (+) @Number
                      , mkValEntry "~-" "Subtract." $ (-) @Number
                      , mkValEntry "~*" "Multiply." $ (*) @Number
                      , mkValEntry "~/" "Divide." $ (/) @Number
                      , mkValEntry "nnegate" "Negate." $ negate @Number
                      , mkValEntry "nrecip" "Reciprocal." $ recip @Number
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
                      , mkValEntry "nsignum" "Sign. Note this will be the same exact or inexact as the number." $
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
                      , mkValEntry "nmod" "Modulus, leftover from `div`" $ mod' @Number
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
          , mkValEntry "either" "Eliminate an Either" $ either @A @C @B
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
          , mkValEntry "maplist" "Map the items of a list." (fmap :: (A -> B) -> [A] -> [B])
          , mkValEntry "++" "Concatentate lists." ((++) :: [A] -> [A] -> [A])
          , mkValEntry "filter" "Filter a list." (filter :: (A -> Bool) -> [A] -> [A])
          , mkValEntry "mapMaybe" "Map and filter a list." (mapMaybe :: (A -> Maybe B) -> [A] -> [B])
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
          [ mkValEntry "return" "A value as an Action." $ return @(PinaforeAction baseedit) @A
          , mkValEntry ">>=" "Bind the result of an Action to an Action." $ qbind @baseedit
          , mkValEntry ">>" "Do actions in sequence." $ qbind_ @baseedit
          , mkValEntry "afix" "The fixed point of an Action." $ mfix @(PinaforeAction baseedit) @A
          , mkValEntry "fail" "Fail, causing the program to terminate with error." $ qfail @baseedit
          , mkValEntry
                "stop"
                "Stop. This is similar to an exception that can be caught with `onstop`. The default handler (for the main program, button presses, etc.), is to catch and ignore it."
                (empty :: PinaforeAction baseedit BottomType)
          , mkValEntry "onstop" "`onstop p q` does `q` first, and if it stops, then does `p`." $ onstop @baseedit
          , mkValEntry
                "for_"
                "Perform an action on each value of a list."
                (for_ :: [A] -> (A -> PinaforeAction baseedit ()) -> PinaforeAction baseedit ())
          , mkValEntry
                "for"
                "Perform an action on each value of a list, returning a list."
                (for :: [A] -> (A -> PinaforeAction baseedit B) -> PinaforeAction baseedit [B])
          , mkValEntry "output" "Output text to standard output." $ output @baseedit
          , mkValEntry "outputln" "Output text and a newline to standard output." $ outputln @baseedit
          , mkValEntry
                "gettimems"
                "Get the time as a whole number of milliseconds."
                (liftIO gettimems :: PinaforeAction baseedit Integer)
          , mkValEntry "debugmsg" "Debug message (debug only)." (traceIOM . unpack :: Text -> PinaforeAction baseedit ())
          ]
    , docTreeEntry
          "Undo"
          "Undo and redo changes."
          [ mkValEntry "queue_undo" "Undo an action." $ do
                ua <- pinaforeUndoActions
                liftIO $ uaUndo ua noEditSource
          , mkValEntry "queue_redo" "Redo an action." $ do
                ua <- pinaforeUndoActions
                liftIO $ uaRedo ua noEditSource
          ]
    , docTreeEntry
          "References"
          "A reference of type `Ref {-p,+q}` has a setting type of `p` and a getting type of `q`. References keep track of updates, and will update user interfaces constructed from them when their value changes."
          [ mkValEntry
                "pureref"
                "A constant reference for a value."
                (pure :: A -> PinaforeImmutableReference baseedit A)
          , mkValEntry
                "comapref"
                "Map a function on getting a reference."
                (coMapRange :: (A -> B) -> PinaforeReference baseedit '( C, A) -> PinaforeReference baseedit '( C, B))
          , mkValEntry
                "contramapref"
                "Map a function on setting a reference."
                (contraMapRange :: (B -> A) -> PinaforeReference baseedit '( A, C) -> PinaforeReference baseedit '( B, C))
          , mkValEntry
                "applyref"
                "Combine references."
                ((<*>) :: PinaforeImmutableReference baseedit (A -> B) -> PinaforeImmutableReference baseedit A -> PinaforeImmutableReference baseedit B)
          , mkValEntry
                "unknown"
                "The unknown reference, representing missing information."
                (empty :: PinaforeImmutableReference baseedit BottomType)
          , mkValEntry "known" "True if the reference is known." $ \(val :: PinaforeFunctionValue baseedit (Know TopType)) ->
                (funcEditFunction (Known . isKnown) . val :: PinaforeFunctionValue baseedit (Know Bool))
          , mkValEntry
                "??"
                "`p ?? q` = `p` if it is known, else `q`."
                ((<|>) :: PinaforeImmutableReference baseedit A -> PinaforeImmutableReference baseedit A -> PinaforeImmutableReference baseedit A)
          , mkValEntry "get" "Get a reference, or `stop` if the reference is unknown." $
            pinaforeReferenceGet @baseedit @A
          , mkValEntry "runref" "Run an action from a reference." $ runPinaforeReference @baseedit
          , mkValEntry ":=" "Set a reference to a value. Stop if failed." $ setentity @baseedit
          , mkValEntry "delete" "Delete an entity reference. Stop if failed." $ deleteentity @baseedit
          , mkValEntry "newmemref" "Create a new reference to memory, initially unknown." $ newmemref @baseedit
          ]
    , docTreeEntry
          "Sets"
          ""
          [ mkValEntry
                "comapset"
                "Map a function on getting from a set."
                (coMapRange :: (A -> B) -> PinaforeSet baseedit '( C, A) -> PinaforeSet baseedit '( C, B))
          , mkValEntry
                "contramapset"
                "Map a function on setting to a set."
                (contraMapRange :: (B -> A) -> PinaforeSet baseedit '( A, C) -> PinaforeSet baseedit '( B, C))
          , mkValEntry "/\\" "Intersection of sets. The resulting set can be added to, but not deleted from." $
            pinaforeSetMeet @baseedit @A
          , mkValEntry "\\/" "Union of sets. The resulting set can be deleted from, but not added to." $
            pinaforeSetJoin @baseedit @A
          , mkValEntry "setsum" "Sum of sets." $ pinaforeSetSum @baseedit @AP @AQ @BP @BQ
          , mkValEntry "members" "Get all members of a set, by an order." $ pinaforeSetGetOrdered @baseedit @A
          , mkValEntry "membership" "Get the membership of a set." $ pinaforeSetMembership @baseedit
          , mkValEntry "single" "The member of a single-member set, or unknown." $ pinaforeSetSingle @baseedit @A
          , mkValEntry "count" "Count of members in a set." $ pinaforeSetFunc @baseedit @TopType @Int olength
          , mkValEntry "sum" "Sum of numbers in a set." $ pinaforeSetFunc @baseedit @Number @Number sum
          , mkValEntry "mean" "Mean of numbers in a set." $
            pinaforeSetFunc @baseedit @Number @Number $ \s -> sum s / fromIntegral (olength s)
          , mkValEntry "newentity" "Create a new entity in a set and act on it." $ pinaforeSetAddNew @baseedit
          , mkValEntry
                "+="
                "Add an entity to a set."
                (pinaforeSetAdd :: PinaforeSet baseedit '( A, TopType) -> A -> PinaforeAction baseedit ())
          , mkValEntry
                "-="
                "Remove an entity from a set."
                (pinaforeSetRemove :: PinaforeSet baseedit '( A, TopType) -> A -> PinaforeAction baseedit ())
          , mkValEntry
                "removeall"
                "Remove all entities from a set."
                (pinaforeSetRemoveAll :: PinaforeSet baseedit '( BottomType, TopType) -> PinaforeAction baseedit ())
          , mkValEntry "newmemset" "Create a new set reference to memory, initially empty." $ newmemset @baseedit
          ]
    , docTreeEntry
          "Morphisms"
          "Morphisms relate entities."
          [ mkValEntry "identity" "The identity morphism." $ identityPinaforeMorphism @baseedit @A
          , mkValEntry "!." "Compose morphisms." $ composePinaforeMorphism @baseedit @AP @AQ @BP @BQ @CP @CQ
          , mkValEntry "!$" "Apply a morphism to a reference." $ pinaforeApplyMorphismRef @baseedit @AP @AQ @BP @BQ
          , mkValEntry "!$$" "Apply a morphism to a set." $ pinaforeApplyMorphismSet @baseedit @A @BP @BQ
          , mkValEntry "!@" "Co-apply a morphism to a reference." $
            pinaforeApplyInverseMorphismRef @baseedit @AP @AQ @BP @BQ
          , mkValEntry "!@@" "Co-apply a morphism to a set." $ pinaforeApplyInverseMorphismSet @baseedit @AP @AQ @BP @BQ
          ]
    , docTreeEntry
          "Orders"
          ""
          [ mkValEntry "alphabetical" "Alphabetical order." $ alphabetical @baseedit
          , mkValEntry "numerical" "Numercal order." $ numerical @baseedit
              --, mkValEntry "chronological" "Chronological order." $ chronological @baseedit
          , mkValEntry "orders" "Join orders by priority." $ orders @baseedit @A
          , mkValEntry
                "maporder"
                "Map a function on an order."
                (contramap :: (B -> A) -> PinaforeOrder baseedit A -> PinaforeOrder baseedit B)
          , mkValEntry "orderon" "Order by an order on a particular morphism." $ orderon @baseedit @B @A
          , mkValEntry "rev" "Reverse an order." $ rev @baseedit @A
          , mkValEntry "orderEQ" "Equal by an order." $ pinaforeOrderCompare @baseedit @A $ (==) EQ
          , mkValEntry "orderLT" "Less than by an order." $ pinaforeOrderCompare @baseedit @A $ (==) LT
          , mkValEntry "orderLE" "Less than or equal to by an order." $ pinaforeOrderCompare @baseedit @A $ (/=) GT
          , mkValEntry "orderGT" "Greater than by an order." $ pinaforeOrderCompare @baseedit @A $ (==) GT
          , mkValEntry "orderGE" "Greater than or equal to by an order." $ pinaforeOrderCompare @baseedit @A $ (/=) LT
          ]
    ]
