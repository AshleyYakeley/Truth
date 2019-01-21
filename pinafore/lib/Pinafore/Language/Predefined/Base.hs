module Pinafore.Language.Predefined.Base
    ( base_predefinitions
    , outputln
    ) where

import Pinafore.Base
import Pinafore.Language.Doc
import Pinafore.Language.Morphism
import Pinafore.Language.Order
import Pinafore.Language.Predefined.Defs
import Pinafore.Language.Reference
import Pinafore.Language.Set
import Pinafore.Language.Type
import Pinafore.Storage.File
import Shapes
import Truth.Core

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
onstop p q = do
    ka <- knowPinaforeAction q
    case ka of
        Known a -> return a
        Unknown -> p

base_predefinitions ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
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
          , docTreeEntry "Text" "" [mkValEntry "++" "Concatenate text." $ (<>) @Text]
          , docTreeEntry
                "Numeric"
                ""
                [ mkValEntry "+" "Numeric add." $ (+) @Number
                , mkValEntry "-" "Numeric Subtract." $ (-) @Number
                , mkValEntry "*" "Numeric Multiply." $ (*) @Number
                , mkValEntry "/" "Numeric Divide." $ (/) @Number
                , mkValEntry "~==" "Numeric equality, folding exact and inexact numbers." $ (==) @Number
                , mkValEntry "~/=" "Numeric non-equality." $ (/=) @Number
                , mkValEntry "<" "Numeric strictly less." $ (<) @Number
                , mkValEntry "<=" "Numeric less or equal." $ (<=) @Number
                , mkValEntry ">" "Numeric strictly greater." $ (>) @Number
                , mkValEntry ">=" "Numeric greater or equal." $ (>=) @Number
                , mkValEntry "abs" "Numeric absolute value." $ abs @Number
                , mkValEntry "signum" "Numeric sign." $ signum @Number
                , mkValEntry "inexact" "Convert a number to inexact." numberToDouble
                , mkValEntry
                      "approximate"
                      "`approximate d x` gives the exact number that's a multiple of `d` that's closest to `x`."
                      approximate
                ]
          ]
    , docTreeEntry
          "Maybe"
          ""
          [ mkValPatEntry "Just" "Construct a Maybe from a value." (Just @A) $ \(v :: Maybe A) ->
                case v of
                    Just a -> Just (a, ())
                    _ -> Nothing
          , mkValPatEntry "Nothing" "Construct a Maybe without a value." (Nothing @A) $ \(v :: Maybe A) ->
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
          [ mkPatEntry "[]" "Empty list" "[a]" $ \(v :: [A]) ->
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
          , mkValEntry ">>=" "Bind the result of an Action to an Action." $ (>>=) @(PinaforeAction baseedit) @A @B
          , mkValEntry ">>" "Do actions in sequence." $ (>>) @(PinaforeAction baseedit) @TopType @A
          , mkValEntry "afix" "The fixed point of an Action." $ mfix @(PinaforeAction baseedit) @A
          , mkValEntry "fail" "Fail, causing the program to terminate with error." $ qfail @baseedit
          , mkValEntry
                "stop"
                "Stop. This is similar to an exception that can be caught with `onstop`. The default handler (for the main program, button presses, etc.), is to ignore it." $
            pinaforeActionKnow @baseedit @BottomType Unknown
          , mkValEntry "onstop" "`onstop p q` is `q` if it is stopped, else `p`" $ onstop @baseedit
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
          , mkValEntry "known" "True if the literal is known." $ \(val :: PinaforeFunctionValue baseedit (Know Literal)) ->
                (funcEditFunction (Known . isKnown) . val :: PinaforeFunctionValue baseedit (Know Bool))
          , mkValEntry
                "??"
                "`p ?? q` = `p` if it is known, else `q`."
                ((<|>) :: PinaforeImmutableReference baseedit A -> PinaforeImmutableReference baseedit A -> PinaforeImmutableReference baseedit A)
          , mkValEntry "get" "Get a reference, or `stop` if the reference is unknown." $
            pinaforeReferenceGet @baseedit @A
          , mkValEntry "runref" "Run an action from a reference." $ runPinaforeReference @baseedit
          , mkValEntry ":=" "Set a reference to a value." $ setentity @baseedit
          , mkValEntry "delete" "Delete an entity reference." $ deleteentity @baseedit
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
