module Pinafore.Language.Predefined
    ( DefDoc(..)
    , DocTree(..)
    , runDocTree
    , predefinedBindings
    , predefinedPatternConstructors
    , predefinedDoc
    , outputln
    ) where

import Language.Expression.Sealed
import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.Doc
import Pinafore.Language.Expression
import Pinafore.Language.Morphism
import Pinafore.Language.Name
import Pinafore.Language.NamedEntity
import Pinafore.Language.Order
import Pinafore.Language.Reference
import Pinafore.Language.Set
import Pinafore.Language.Type
import Pinafore.Storage.File
import Shapes
import Truth.Core

--import Truth.World.File
type A = UVar "a"

type B = UVar "b"

type C = UVar "c"

type AP = UVar "ap"

type BP = UVar "bp"

type CP = UVar "cp"

type AQ = UVar "aq"

type BQ = UVar "bq"

type CQ = UVar "cq"

output :: forall baseedit. Text -> PinaforeAction baseedit
output text = liftIO $ putStr $ unpack text

outputln :: forall baseedit. Text -> PinaforeAction baseedit
outputln text = liftIO $ putStrLn $ unpack text

valSpecText ::
       UISpec seledit (WholeEdit (Know Text))
    -> PinaforeLensValue baseedit (WholeEdit (Know Text))
    -> UISpec seledit baseedit
valSpecText spec val = uiLens val spec

clearText :: EditFunction (WholeEdit (Know Text)) (WholeEdit Text)
clearText = funcEditFunction (fromKnow mempty)

newentity ::
       forall baseedit.
       PinaforeSet baseedit '( NewEntity, TopType)
    -> (NewEntity -> PinaforeAction baseedit)
    -> PinaforeAction baseedit
newentity set continue = do
    e <- pinaforeSetAddNew set
    continue e

setentity :: forall baseedit. PinaforeReference baseedit '( A, TopType) -> A -> PinaforeAction baseedit
setentity ref val = setPinaforeReference ref (Known val)

deleteentity :: forall baseedit. PinaforeReference baseedit '( BottomType, TopType) -> PinaforeAction baseedit
deleteentity ref = setPinaforeReference ref Unknown

{-
file_import ::
       forall baseedit. HasPinaforeFileEdit baseedit
    => PinaforeSet baseedit '( A, A)
    -> (A -> PinaforeAction baseedit)
    -> PinaforeAction baseedit
file_import set continue = do
    chooseFile <- pinaforeActionRequest witChooseFile
    mpath <- liftIO chooseFile
    case mpath of
        Nothing -> return ()
        Just path -> do
            let sourceobject = fileObject path
            newentity set $ \entity -> do
                mdestobject <-
                    pinaforeLiftView $
                    viewMapEdit (pinaforeFileItemLens entity) $ do
                        MkObject {..} <- viewObject
                        liftIO $
                            runTransform objRun $ do
                                pushEdit $ objEdit [SingleObjectDeleteCreate]
                                objRead ReadSingleObjectStore
                destobject <-
                    case mdestobject of
                        Nothing -> pinaforeLiftResult $ FailureResult $ fromString $ "failed to create object " ++ show entity
                        Just object -> return object
                liftIO $ copyObject sourceobject destobject
                continue entity

file_size :: Object ByteStringEdit -> IO Int64
file_size MkObject {..} = runTransform objRun $ objRead ReadByteStringLength
-}
withSelection :: (NewEntity -> PinaforeAction baseedit) -> PinaforeAction baseedit
withSelection cont = do
    mselection <- pinaforeLiftView viewGetSelection
    case mselection of
        Nothing -> return ()
        Just MkObject {..} -> do
            e <- liftIO $ runTransform objRun $ objRead ReadWhole
            cont $ MkNewEntity e

ui_table ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => [(PinaforeReference baseedit '( BottomType, Text), A -> PinaforeReference baseedit '( BottomType, Text))]
    -> (A -> UIWindow baseedit)
    -> PinaforeSet baseedit '( A, MeetType Entity A)
    -> UISpec (ConstEdit Entity) baseedit
ui_table cols asp val = let
    showCell :: Know Text -> (Text, TableCellProps)
    showCell (Known s) = (s, tableCellPlain)
    showCell Unknown = ("unknown", tableCellPlain {tcItalic = True})
    mapLens :: PinaforeFunctionValue baseedit (Know Text) -> PinaforeFunctionValue baseedit (Text, TableCellProps)
    mapLens ff = funcEditFunction showCell . ff
    getColumn ::
           (PinaforeReference baseedit '( BottomType, Text), A -> PinaforeReference baseedit '( BottomType, Text))
        -> KeyColumn baseedit (MeetType Entity A)
    getColumn (name, f) =
        readOnlyKeyColumn (clearText . pinaforeReferenceToFunction name) $ \p ->
            return $ mapLens $ pinaforeReferenceToFunction $ f $ meet2 p
    aspect :: MeetType Entity A -> IO (UIWindow baseedit)
    aspect entity = return $ asp $ meet2 entity
    in uiSetSelectionLens (funcNoEditLens meet1) $
       uiTable (fmap getColumn cols) aspect $ unPinaforeSet $ contraMapRange meet2 val

type PickerType = Know (MeetType Entity A)

type PickerPairType = (PickerType, Text)

ui_pick ::
       forall baseedit seledit.
       PinaforeMorphism baseedit '( A, TopType) '( BottomType, Text)
    -> PinaforeSet baseedit '( A, MeetType Entity A)
    -> PinaforeReference baseedit '( A, MeetType Entity A)
    -> UISpec seledit baseedit
ui_pick nameMorphism fset ref = let
    getName :: PinaforeFunctionMorphism baseedit (MeetType Entity A) PickerPairType
    getName =
        proc p -> do
            n <- pinaforeMorphismFunction nameMorphism -< Known $ meet2 p
            returnA -< (Known p, fromKnow "" n)
    getNames :: PinaforeFunctionMorphism baseedit (FiniteSet (MeetType Entity A)) (FiniteSet PickerPairType)
    getNames =
        proc fsp -> do
            pairs <- cfmap getName -< fsp
            returnA -< insertSet (Unknown, "") pairs
    opts :: EditFunction baseedit (ListEdit [PickerPairType] (WholeEdit PickerPairType))
    opts =
        (orderedKeyList @(FiniteSet PickerPairType) $ \(_, a) (_, b) -> compare a b) .
        convertEditFunction . applyPinaforeFunction getNames (pinaforeSetFunctionValue fset)
    in uiOption @baseedit @PickerType opts $ pinaforeReferenceToLens $ contraMapRange meet2 ref

qfail :: forall baseedit. Text -> PinaforeAction baseedit
qfail t = liftIO $ fail $ unpack t

ui_dynamic ::
       forall baseedit.
       PinaforeImmutableReference baseedit (UISpec (ConstEdit Entity) baseedit)
    -> UISpec (ConstEdit Entity) baseedit
ui_dynamic uiref = uiSwitch $ pinaforeImmutableReferenceValue uiNull uiref

data BindDoc baseedit = MkBindDoc
    { bdName :: Name
    , bdValue :: Maybe (QValue baseedit)
    , bdPattern :: Maybe (QPatternConstructor baseedit)
    , bdDoc :: DefDoc
    }

mkValEntry ::
       forall baseedit t. (HasPinaforeEntityEdit baseedit, ToPinaforeType baseedit t)
    => Name
    -> Text
    -> t
    -> DocTreeEntry (BindDoc baseedit)
mkValEntry name docDescription val = let
    bdName = name
    bdValue = Just $ toValue val
    bdPattern = Nothing
    docName = name
    docValueType = qTypeDescription @baseedit @t
    docIsPattern = False
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkValPatEntry ::
       forall baseedit t v lt.
       ( HasPinaforeEntityEdit baseedit
       , ToPinaforeType baseedit t
       , FromPinaforeType baseedit v
       , ToTypeF (HListWit (PinaforeType baseedit 'Positive)) (HList lt)
       )
    => Name
    -> Text
    -> t
    -> (v -> Maybe (HList lt))
    -> DocTreeEntry (BindDoc baseedit)
mkValPatEntry name docDescription val pat = let
    bdName = name
    bdValue = Just $ toValue val
    bdPattern = Just $ toPatternConstructor pat
    docName = name
    docValueType = qTypeDescription @baseedit @t
    docIsPattern = True
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkPatEntry ::
       forall baseedit v lt.
       ( HasPinaforeEntityEdit baseedit
       , FromPinaforeType baseedit v
       , ToTypeF (HListWit (PinaforeType baseedit 'Positive)) (HList lt)
       )
    => Name
    -> Text
    -> Text
    -> (v -> Maybe (HList lt))
    -> DocTreeEntry (BindDoc baseedit)
mkPatEntry name docDescription docValueType pat = let
    bdName = name
    bdValue = Nothing
    bdPattern = Just $ toPatternConstructor pat
    docName = name
    docIsPattern = True
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

entityuuid :: Entity -> Text
entityuuid p = pack $ show p

predefinitions ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => DocTree (BindDoc baseedit)
predefinitions =
    MkDocTree
        "Predefined"
        ""
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
              , mkValEntry "!@@" "Co-apply a morphism to a set." $
                pinaforeApplyInverseMorphismSet @baseedit @AP @AQ @BP @BQ
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
              , mkValEntry "orderGE" "Greater than or equal to by an order." $
                pinaforeOrderCompare @baseedit @A $ (/=) LT
              ]
        , docTreeEntry
              "Actions"
              ""
              [ mkValEntry "pass" "Do nothing." (return () :: PinaforeAction baseedit)
              , mkValEntry ">>" "Do actions in sequence." $
                ((>>) :: PinaforeAction baseedit -> PinaforeAction baseedit -> PinaforeAction baseedit)
              , mkValEntry "fail" "Fail, causing the program to terminate with error." $ qfail @baseedit
              , mkValEntry "get" "Get a reference and perform an action on it." $ pinaforeReferenceWith @baseedit @A
              , mkValEntry "runref" "Run an action from a reference." $ runPinaforeReference @baseedit
              , mkValEntry
                    "for"
                    "Perform an action on each value of a list."
                    (for_ :: [A] -> (A -> PinaforeAction baseedit) -> PinaforeAction baseedit)
              , mkValEntry "output" "Output text to standard output." $ output @baseedit
              , mkValEntry "outputln" "Output text and a newline to standard output." $ outputln @baseedit
              , mkValEntry ":=" "Set a reference to a value." $ setentity @baseedit
              , mkValEntry "delete" "Delete an entity reference." $ deleteentity @baseedit
              , mkValEntry "newentity" "Create a new entity in a set and act on it." $ newentity @baseedit
              , mkValEntry
                    "+="
                    "Add an entity to a set."
                    (pinaforeSetAdd :: PinaforeSet baseedit '( A, TopType) -> A -> PinaforeAction baseedit)
              , mkValEntry
                    "-="
                    "Remove an entity from a set."
                    (pinaforeSetRemove :: PinaforeSet baseedit '( A, TopType) -> A -> PinaforeAction baseedit)
              , mkValEntry
                    "removeall"
                    "Remove all entities from a set."
                    (pinaforeSetRemoveAll :: PinaforeSet baseedit '( BottomType, TopType) -> PinaforeAction baseedit)
              ]
        {-
        , docTreeEntry
              "Files"
              [ mkValEntry "file_import" "Import a file into a set." $ file_import @baseedit
              , mkValEntry "file_size" "The size of a file." file_size
              ]
        -}
        , docTreeEntry
              "UI"
              "A user interface is something that goes inside a window."
              [ mkValEntry "openwindow" "Open a new window with this title and UI." viewOpenWindow
              , mkValEntry "openselection" "Open the item selected in the UI of this window." viewOpenSelection
              , mkValEntry "withselection" "Act with the item selected in the UI of this window." $
                withSelection @baseedit
              , mkValEntry "ui_blank" "Blank user-interface" uiNull
              , mkValEntry "ui_unitcheckbox" "(TBD)" $ \name val ->
                    uiCheckbox (clearText . name) $ toEditLens knowBool . val
              , mkValEntry "ui_booleancheckbox" "Checkbox. Use shift-click to set to unknown." $ \name val ->
                    uiMaybeCheckbox (clearText . name) $ (bijectionWholeEditLens knowMaybe) . val
              , mkValEntry "ui_textentry" "Text entry, empty text is unknown." $
                valSpecText $ uiUnknownValue mempty uiTextEntry
              , mkValEntry "ui_textarea" "Text area, empty text is unknown." $
                valSpecText $ uiUnknownValue mempty $ uiNoSelectionLens $ uiConvert uiText
              , mkValEntry "ui_label" "Label." $ valSpecText $ uiUnknownValue mempty $ uiLabel
              , mkValEntry
                    "ui_horizontal"
                    "Items arranged horizontally, each flag is whether to expand into remaining space."
                    uiHorizontal
              , mkValEntry
                    "ui_vertical"
                    "Items arranged vertically, each flag is whether to expand into remaining space."
                    uiVertical
              , mkValEntry
                    "ui_pages"
                    "A notebook of pages. First of each pair is for the page tab (typically a label), second is the content."
                    uiPages
                -- CSS
                -- drag
                -- icon
              , mkValEntry "ui_button" "A button with this text that does this action." $ \(name :: PinaforeFunctionValue baseedit (Know Text)) action ->
                    uiButton (clearText . name) action
              , mkValEntry "ui_pick" "A drop-down menu." $ ui_pick
              , mkValEntry
                    "ui_table"
                    "A list table. First arg is columns (name, property), second is the window to open for a selection, third is the set of items." $
                ui_table @baseedit
              , mkValEntry "ui_dynamic" "A UI that can be updated to different UIs." $ ui_dynamic @baseedit
              ]
        ]

predefinedDoc ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => DocTree DefDoc
predefinedDoc = fmap bdDoc $ predefinitions @baseedit

predefinedBindings ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => StrictMap Name (QValue baseedit)
predefinedBindings =
    mapFromList $
    catMaybes $
    toList $
    fmap
        (\doc -> do
             val <- bdValue doc
             return (bdName doc, val)) $
    predefinitions @baseedit

predefinedPatternConstructors ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => StrictMap Name (PinaforePatternConstructor baseedit)
predefinedPatternConstructors =
    mapFromList $
    catMaybes $
    toList $
    fmap
        (\doc -> do
             pat <- bdPattern doc
             return (bdName doc, pat)) $
    predefinitions @baseedit
