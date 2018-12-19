module Pinafore.Language.Predefined
    ( DefDoc(..)
    , DocTree(..)
    , runDocTree
    , predefinedBindings
    , predefinedDoc
    , outputln
    ) where

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

type BindDoc baseedit = (Maybe (Name, QValue baseedit), DefDoc)

mkDefEntry ::
       forall baseedit t. (HasPinaforeEntityEdit baseedit, ToPinaforeType baseedit t)
    => Name
    -> Text
    -> t
    -> DocTreeEntry (BindDoc baseedit)
mkDefEntry name desc val = EntryDocTreeEntry (Just (name, toValue val), mkDefDoc @baseedit name desc val)

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
              [ mkDefEntry "is" "Entity equality." $ (==) @Entity
              , mkDefEntry "==" "Literal equality. Same as Entity equality restricted to Literal, but faster." $
                (==) @Literal
              , mkDefEntry "/=" "Literal non-equality." $ (/=) @Literal
              , mkDefEntry "entityuuid" "UUID of an entity." entityuuid
              , mkDefEntry "totext" "The text of a literal." unLiteral
              , docTreeEntry
                    "Boolean"
                    ""
                    [ mkDefEntry "&&" "Boolean AND." (&&)
                    , mkDefEntry "||" "Boolean OR." (||)
                    , mkDefEntry "not" "Boolean NOT." not
                    ]
              , docTreeEntry "Text" "" [mkDefEntry "++" "Concatenate text." $ (<>) @Text]
              , docTreeEntry
                    "Numeric"
                    ""
                    [ mkDefEntry "+" "Numeric add." $ (+) @Number
                    , mkDefEntry "-" "Numeric Subtract." $ (-) @Number
                    , mkDefEntry "*" "Numeric Multiply." $ (*) @Number
                    , mkDefEntry "/" "Numeric Divide." $ (/) @Number
                    , mkDefEntry "~==" "Numeric equality, folding exact and inexact numbers." $ (==) @Number
                    , mkDefEntry "~/=" "Numeric non-equality." $ (/=) @Number
                    , mkDefEntry "<" "Numeric strictly less." $ (<) @Number
                    , mkDefEntry "<=" "Numeric less or equal." $ (<=) @Number
                    , mkDefEntry ">" "Numeric strictly greater." $ (>) @Number
                    , mkDefEntry ">=" "Numeric greater or equal." $ (>=) @Number
                    , mkDefEntry "abs" "Numeric absolute value." $ abs @Number
                    , mkDefEntry "signum" "Numeric sign." $ signum @Number
                    , mkDefEntry "inexact" "Convert a number to inexact." numberToDouble
                    , mkDefEntry
                          "approximate"
                          "`approximate d x` gives the exact number that's a multiple of `d` that's closest to `x`."
                          approximate
                    ]
              ]
        , docTreeEntry
              "Pairs"
              ""
              [ mkDefEntry "fst" "Get the first member of a pair." $ fst @A @B
              , mkDefEntry "snd" "Get the second member of a pair." $ snd @A @B
              ]
        , docTreeEntry
              "Either"
              ""
              [ mkDefEntry "left" "Construct an Either from the left." $ Left @A @B
              , mkDefEntry "right" "Construct an Either from the right." $ Right @A @B
              , mkDefEntry "either" "Eliminate an Either" $ either @A @C @B
              ]
        , docTreeEntry
              "Lists"
              ""
              [ mkDefEntry ":" "Construct a list" $ (:) @A
              , mkDefEntry "list" "Eliminate a list" $ \(fnil :: B) fcons (l :: [A]) ->
                    case l of
                        [] -> fnil
                        (a:aa) -> fcons a aa
              ]
        , docTreeEntry
              "Functions"
              ""
              [ mkDefEntry "id" "The identity function." $ id @(->) @A
              , mkDefEntry "$" "Apply a function to a value." $ id @(->) @(A -> B)
              , mkDefEntry "." "Compose functions." $ (.) @(->) @A @B @C
              , mkDefEntry "error" "Error." $ ((\t -> error (unpack t)) :: Text -> BottomType)
              ]
        , docTreeEntry
              "References"
              "A reference of type `Ref {-p,+q}` has a setting type of `p` and a getting type of `q`. References keep track of updates, and will update user interfaces constructed from them when their value changes."
              [ mkDefEntry
                    "pureref"
                    "A constant reference for a value."
                    (pure :: A -> PinaforeImmutableReference baseedit A)
              , mkDefEntry
                    "comapref"
                    "Map a function on getting a reference."
                    (coMapRange :: (A -> B) -> PinaforeReference baseedit '( C, A) -> PinaforeReference baseedit '( C, B))
              , mkDefEntry
                    "contramapref"
                    "Map a function on setting a reference."
                    (contraMapRange :: (B -> A) -> PinaforeReference baseedit '( A, C) -> PinaforeReference baseedit '( B, C))
              , mkDefEntry
                    "applyref"
                    "Combine references."
                    ((<*>) :: PinaforeImmutableReference baseedit (A -> B) -> PinaforeImmutableReference baseedit A -> PinaforeImmutableReference baseedit B)
              , mkDefEntry
                    "unknown"
                    "The unknown reference, representing missing information."
                    (empty :: PinaforeImmutableReference baseedit BottomType)
              , mkDefEntry "known" "True if the literal is known." $ \(val :: PinaforeFunctionValue baseedit (Know Literal)) ->
                    (funcEditFunction (Known . isKnown) . val :: PinaforeFunctionValue baseedit (Know Bool))
              , mkDefEntry
                    "??"
                    "`p ?? q` = `p` if it is known, else `q`."
                    ((<|>) :: PinaforeImmutableReference baseedit A -> PinaforeImmutableReference baseedit A -> PinaforeImmutableReference baseedit A)
              ]
        , docTreeEntry
              "Sets"
              ""
              [ mkDefEntry
                    "comapset"
                    "Map a function on getting from a set."
                    (coMapRange :: (A -> B) -> PinaforeSet baseedit '( C, A) -> PinaforeSet baseedit '( C, B))
              , mkDefEntry
                    "contramapset"
                    "Map a function on setting to a set."
                    (contraMapRange :: (B -> A) -> PinaforeSet baseedit '( A, C) -> PinaforeSet baseedit '( B, C))
              , mkDefEntry "/\\" "Intersection of sets. The resulting set can be added to, but not deleted from." $
                pinaforeSetMeet @baseedit @A
              , mkDefEntry "\\/" "Union of sets. The resulting set can be deleted from, but not added to." $
                pinaforeSetJoin @baseedit @A
              , mkDefEntry "setsum" "Sum of sets." $ pinaforeSetSum @baseedit @AP @AQ @BP @BQ
              , mkDefEntry "members" "Get all members of a set, by an order." $ pinaforeSetGetOrdered @baseedit @A
              , mkDefEntry "membership" "Get the membership of a set." $ pinaforeSetMembership @baseedit
              , mkDefEntry "single" "The member of a single-member set, or unknown." $ pinaforeSetSingle @baseedit @A
              , mkDefEntry "count" "Count of members in a set." $ pinaforeSetFunc @baseedit @TopType @Int olength
              , mkDefEntry "sum" "Sum of numbers in a set." $ pinaforeSetFunc @baseedit @Number @Number sum
              , mkDefEntry "mean" "Mean of numbers in a set." $
                pinaforeSetFunc @baseedit @Number @Number $ \s -> sum s / fromIntegral (olength s)
              ]
        , docTreeEntry
              "Morphisms"
              "Morphisms relate entities."
              [ mkDefEntry "identity" "The identity morphism." $ identityPinaforeMorphism @baseedit @A
              , mkDefEntry "!." "Compose morphisms." $ composePinaforeMorphism @baseedit @AP @AQ @BP @BQ @CP @CQ
              , mkDefEntry "!$" "Apply a morphism to a reference." $ pinaforeApplyMorphismRef @baseedit @AP @AQ @BP @BQ
              , mkDefEntry "!$$" "Apply a morphism to a set." $ pinaforeApplyMorphismSet @baseedit @A @BP @BQ
              , mkDefEntry "!@" "Co-apply a morphism to a reference." $
                pinaforeApplyInverseMorphismRef @baseedit @AP @AQ @BP @BQ
              , mkDefEntry "!@@" "Co-apply a morphism to a set." $
                pinaforeApplyInverseMorphismSet @baseedit @AP @AQ @BP @BQ
              ]
        , docTreeEntry
              "Orders"
              ""
              [ mkDefEntry "alphabetical" "Alphabetical order." $ alphabetical @baseedit
              , mkDefEntry "numerical" "Numercal order." $ numerical @baseedit
              --, mkDefEntry "chronological" "Chronological order." $ chronological @baseedit
              , mkDefEntry "orders" "Join orders by priority." $ orders @baseedit @A
              , mkDefEntry
                    "maporder"
                    "Map a function on an order."
                    (contramap :: (B -> A) -> PinaforeOrder baseedit A -> PinaforeOrder baseedit B)
              , mkDefEntry "orderon" "Order by an order on a particular morphism." $ orderon @baseedit @B @A
              , mkDefEntry "rev" "Reverse an order." $ rev @baseedit @A
              , mkDefEntry "orderEQ" "Equal by an order." $ pinaforeOrderCompare @baseedit @A $ (==) EQ
              , mkDefEntry "orderLT" "Less than by an order." $ pinaforeOrderCompare @baseedit @A $ (==) LT
              , mkDefEntry "orderLE" "Less than or equal to by an order." $ pinaforeOrderCompare @baseedit @A $ (/=) GT
              , mkDefEntry "orderGT" "Greater than by an order." $ pinaforeOrderCompare @baseedit @A $ (==) GT
              , mkDefEntry "orderGE" "Greater than or equal to by an order." $
                pinaforeOrderCompare @baseedit @A $ (/=) LT
              ]
        , docTreeEntry
              "Actions"
              ""
              [ mkDefEntry "pass" "Do nothing." (return () :: PinaforeAction baseedit)
              , mkDefEntry ">>" "Do actions in sequence." $
                ((>>) :: PinaforeAction baseedit -> PinaforeAction baseedit -> PinaforeAction baseedit)
              , mkDefEntry "fail" "Fail, causing the program to terminate with error." $ qfail @baseedit
              , mkDefEntry "get" "Get a reference and perform an action on it." $ pinaforeReferenceWith @baseedit @A
              , mkDefEntry "runref" "Run an action from a reference." $ runPinaforeReference @baseedit
              , mkDefEntry
                    "for"
                    "Perform an action on each value of a list."
                    (for_ :: [A] -> (A -> PinaforeAction baseedit) -> PinaforeAction baseedit)
              , mkDefEntry "output" "Output text to standard output." $ output @baseedit
              , mkDefEntry "outputln" "Output text and a newline to standard output." $ outputln @baseedit
              , mkDefEntry ":=" "Set a reference to a value." $ setentity @baseedit
              , mkDefEntry "delete" "Delete an entity reference." $ deleteentity @baseedit
              , mkDefEntry "newentity" "Create a new entity in a set and act on it." $ newentity @baseedit
              , mkDefEntry
                    "+="
                    "Add an entity to a set."
                    (pinaforeSetAdd :: PinaforeSet baseedit '( A, TopType) -> A -> PinaforeAction baseedit)
              , mkDefEntry
                    "-="
                    "Remove an entity from a set."
                    (pinaforeSetRemove :: PinaforeSet baseedit '( A, TopType) -> A -> PinaforeAction baseedit)
              , mkDefEntry
                    "removeall"
                    "Remove all entities from a set."
                    (pinaforeSetRemoveAll :: PinaforeSet baseedit '( BottomType, TopType) -> PinaforeAction baseedit)
              ]
        {-
        , docTreeEntry
              "Files"
              [ mkDefEntry "file_import" "Import a file into a set." $ file_import @baseedit
              , mkDefEntry "file_size" "The size of a file." file_size
              ]
        -}
        , docTreeEntry
              "UI"
              "A user interface is something that goes inside a window."
              [ mkDefEntry "openwindow" "Open a new window with this title and UI." viewOpenWindow
              , mkDefEntry "openselection" "Open the item selected in the UI of this window." viewOpenSelection
              , mkDefEntry "withselection" "Act with the item selected in the UI of this window." $
                withSelection @baseedit
              , mkDefEntry "ui_blank" "Blank user-interface" uiNull
              , mkDefEntry "ui_unitcheckbox" "(TBD)" $ \name val ->
                    uiCheckbox (clearText . name) $ toEditLens knowBool . val
              , mkDefEntry "ui_booleancheckbox" "Checkbox. Use shift-click to set to unknown." $ \name val ->
                    uiMaybeCheckbox (clearText . name) $ (bijectionWholeEditLens knowMaybe) . val
              , mkDefEntry "ui_textentry" "Text entry, empty text is unknown." $
                valSpecText $ uiUnknownValue mempty uiTextEntry
              , mkDefEntry "ui_textarea" "Text area, empty text is unknown." $
                valSpecText $ uiUnknownValue mempty $ uiNoSelectionLens $ uiConvert uiText
              , mkDefEntry "ui_label" "Label." $ valSpecText $ uiUnknownValue mempty $ uiLabel
              , mkDefEntry
                    "ui_horizontal"
                    "Items arranged horizontally, each flag is whether to expand into remaining space."
                    uiHorizontal
              , mkDefEntry
                    "ui_vertical"
                    "Items arranged vertically, each flag is whether to expand into remaining space."
                    uiVertical
              , mkDefEntry
                    "ui_pages"
                    "A notebook of pages. First of each pair is for the page tab (typically a label), second is the content."
                    uiPages
                -- CSS
                -- drag
                -- icon
              , mkDefEntry "ui_button" "A button with this text that does this action." $ \(name :: PinaforeFunctionValue baseedit (Know Text)) action ->
                    uiButton (clearText . name) action
              , mkDefEntry "ui_pick" "A drop-down menu." $ ui_pick
              , mkDefEntry
                    "ui_table"
                    "A list table. First arg is columns (name, property), second is the window to open for a selection, third is the set of items." $
                ui_table @baseedit
              , mkDefEntry "ui_dynamic" "A UI that can be updated to different UIs." $ ui_dynamic @baseedit
              ]
        ]

predefinedDoc ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => DocTree DefDoc
predefinedDoc = fmap snd $ predefinitions @baseedit

predefinedBindings ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => StrictMap Name (QValue baseedit)
predefinedBindings = mapFromList $ catMaybes $ toList $ fmap fst $ predefinitions @baseedit
