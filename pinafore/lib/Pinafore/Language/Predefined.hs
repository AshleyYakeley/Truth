module Pinafore.Language.Predefined
    ( DefDoc(..)
    , DocTree(..)
    , runDocTree
    , predefinedBindings
    , predefinedDoc
    , outputln
    ) where

import Pinafore.Action
import Pinafore.File
import Pinafore.Know
import Pinafore.Language.Convert
import Pinafore.Language.Doc
import Pinafore.Language.Entity
import Pinafore.Language.Expression
import Pinafore.Language.Morphism
import Pinafore.Language.Name
import Pinafore.Language.Order
import Pinafore.Language.Reference
import Pinafore.Language.Set
import Pinafore.Language.Type
import Pinafore.Literal
import Pinafore.Morphism
import Pinafore.Number
import Pinafore.PredicateMorphism
import Pinafore.Table
import Pinafore.Types
import Shapes
import Truth.Core

--import Truth.World.File
type A = UVar "a"

type B = UVar "b"

type C = UVar "c"

qapply :: (A -> B) -> A -> B
qapply = ($)

qcompose :: (B -> C) -> (A -> B) -> A -> C
qcompose = (.)

qmeet ::
       PinaforeSet baseedit '( A, MeetType Entity A)
    -> PinaforeSet baseedit '( A, MeetType Entity A)
    -> PinaforeSet baseedit '( MeetType Entity A, A)
qmeet = pinaforeSetMeet

qjoin ::
       PinaforeSet baseedit '( A, MeetType Entity A)
    -> PinaforeSet baseedit '( A, MeetType Entity A)
    -> PinaforeSet baseedit '( MeetType Entity A, A)
qjoin = pinaforeSetJoin

output ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => QFuncValue baseedit Text
    -> PinaforeAction baseedit
output val = do
    mtext <- pinaforeFunctionValueGet val
    for_ mtext $ \text -> liftIO $ putStr $ unpack text

outputln ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => QFuncValue baseedit Text
    -> PinaforeAction baseedit
outputln val = do
    mtext <- pinaforeFunctionValueGet val
    for_ mtext $ \text -> liftIO $ putStrLn $ unpack text

qappend :: Text -> Text -> Text
qappend = (<>)

valSpecText :: UISpec seledit (WholeEdit (Know Text)) -> QLensValue baseedit Text -> UISpec seledit baseedit
valSpecText spec val = uiLens val spec

clearText :: EditFunction (WholeEdit (Know Text)) (WholeEdit Text)
clearText = funcEditFunction (fromKnow mempty)

newentity ::
       forall baseedit.
       PinaforeSet baseedit '( Point, TopType)
    -> (Point -> PinaforeAction baseedit)
    -> PinaforeAction baseedit
newentity set continue = do
    point <- pinaforeSetAddNew set
    continue point

getentity ::
       forall baseedit.
       PinaforeReference baseedit '( BottomType, A)
    -> (A -> PinaforeAction baseedit)
    -> PinaforeAction baseedit
getentity ref cont = do
    kq <- getPinaforeReference ref
    case kq of
        Known q -> cont q
        Unknown -> return ()

setentity :: forall baseedit. PinaforeReference baseedit '( A, TopType) -> A -> PinaforeAction baseedit
setentity ref val = setPinaforeReference ref (Known val)

deleteentity :: forall baseedit. PinaforeReference baseedit '( BottomType, TopType) -> PinaforeAction baseedit
deleteentity ref = setPinaforeReference ref Unknown

{-
file_import ::
       forall baseedit. HasPinaforeFileEdit baseedit
    => QLensSet baseedit A
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
                            runUnliftIO objRun $ do
                                pushEdit $ objEdit [SingleObjectDeleteCreate]
                                objRead ReadSingleObjectStore
                destobject <-
                    case mdestobject of
                        Nothing -> pinaforeLiftResult $ FailureResult $ fromString $ "failed to create object " ++ show entity
                        Just object -> return object
                liftIO $ copyObject sourceobject destobject
                continue entity

file_size :: Object ByteStringEdit -> IO Int64
file_size MkObject {..} = runUnliftIO objRun $ objRead ReadByteStringLength
-}
withSelection :: (Entity -> PinaforeAction baseedit) -> PinaforeAction baseedit
withSelection cont = do
    mselection <- pinaforeLiftView viewGetSelection
    case mselection of
        Nothing -> return ()
        Just MkObject {..} -> do
            point <- liftIO $ runUnliftIO objRun $ objRead ReadWhole
            cont point

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
       uiTable (fmap getColumn cols) aspect $ unPinaforeSet $ contraMapTypeRange meet2 val

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
            n <- pinaforeMorphismFunction nameMorphism -< meet2 p
            returnA -< (Known p, n)
    getNames :: PinaforeFunctionMorphism baseedit (FiniteSet (MeetType Entity A)) (FiniteSet PickerPairType)
    getNames =
        proc fsp -> do
            pairs <- cfmap getName -< fsp
            returnA -< insertSet (Unknown, "") pairs
    opts :: EditFunction baseedit (ListEdit [PickerPairType] (WholeEdit PickerPairType))
    opts =
        (orderedKeyList @(FiniteSet PickerPairType) $ \(_, a) (_, b) -> compare a b) .
        convertEditFunction . applyPinaforeFunction getNames (pinaforeSetFunctionValue fset)
    in uiOption @baseedit @PickerType opts $ unPinaforeReference $ contraMapTypeRange meet2 ref

qfail :: forall baseedit. Text -> PinaforeAction baseedit
qfail t = liftIO $ fail $ unpack t

{-
nulljoin :: forall baseedit. Lifted baseedit Literal -> Lifted baseedit Literal -> Lifted baseedit Literal
nulljoin lx ly = let
    qq :: Maybe Literal -> Maybe Literal -> Maybe Literal
    qq Nothing y = y
    qq x _ = x
    in maybeLifted $ qq <$> liftedMaybe lx <*> liftedMaybe ly
-}
type BindDoc baseedit = (Maybe (QBindList baseedit), DefDoc)

mkDefEntry ::
       forall baseedit t. (HasPinaforeEntityEdit baseedit, ToPinaforeType baseedit t)
    => Name
    -> Text
    -> t
    -> DocTreeEntry (BindDoc baseedit)
mkDefEntry name desc val = EntryDocTreeEntry (Just (qBindVal name val), mkDefDoc @baseedit name desc val)

nulljoin ::
       forall baseedit.
       PinaforeImmutableReference baseedit A
    -> PinaforeImmutableReference baseedit A
    -> PinaforeImmutableReference baseedit A
nulljoin = (<|>)

predefinitions ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => DocTree (BindDoc baseedit)
predefinitions =
    MkDocTree
        "Predefined"
        [ docTreeEntry "Identity" [mkDefEntry "is" "Entity identity. This is always `true` or `false`." $ (==) @Entity]
        , docTreeEntry
              "Functions & Morphisms"
              [ mkDefEntry "$" "Apply a function, morphism, or inverse morphism to a value." qapply
              , mkDefEntry "." "Compose functions." qcompose
              , mkDefEntry
                    "identity"
                    "The identity morphism."
                    (identityPinaforeMorphism :: PinaforeMorphism baseedit '( A, A) '( A, A))
              , mkDefEntry
                    "<.>"
                    "Compose morphisms."
                    (composePinaforeMorphism :: PinaforeMorphism baseedit '( B, B) '( C, C) -> PinaforeMorphism baseedit '( A, A) '( B, B) -> PinaforeMorphism baseedit '( A, A) '( C, C))
              {-
              , EntryDocTreeEntry
                    ( Nothing
                    , mkDefDoc "@" "Invert a morphism to an inverse morphism, or an inverse morphism to a morphism." $
                      qinvert @baseedit)
            -}
              ]
        , docTreeEntry
              "Sets"
              [ mkDefEntry "/\\" "Intersection of sets. The resulting set can be added to, but not deleted from." $
                qmeet @baseedit
              , mkDefEntry "\\/" "Union of sets. The resulting set can be deleted from, but not added to." $
                qjoin @baseedit
              , mkDefEntry "members" "Get all members of a set, by an order." $ pinaforeSetGetOrdered @baseedit @A
              , mkDefEntry "contains" "Determine membership of a set." $ pinaforeSetContains @baseedit
              , mkDefEntry "single" "The member of a single-member set, or null." $ pinaforeSetSingle @baseedit @A
              ]
        , docTreeEntry
              "Literals"
              [ mkDefEntry "==" "Literal equality. Note that `null == x` and `x == null` are null for any x." $
                (==) @Literal
              , mkDefEntry "/=" "Literal non-equality. Note that `null /= x` and `x /= null` are null for any x." $
                (/=) @Literal
              , docTreeEntry
                    "Nulls"
                    [ mkDefEntry
                          "null"
                          "Null inhabits every literal type, representing missing information. Note that `is null null` = `false`." $
                      (empty :: PinaforeImmutableReference baseedit A)
                    , mkDefEntry "exists" "True if the literal is not null." $ \(val :: QFuncValue baseedit Literal) ->
                          (funcEditFunction (Known . isKnown) . val :: QFuncValue baseedit Bool)
                    , mkDefEntry "??" "`p ?? q` = `if exists p then p else q`." $ nulljoin @baseedit
                    ]
              , docTreeEntry
                    "Boolean"
                    [ mkDefEntry "&&" "Boolean AND." (&&)
                    , mkDefEntry "||" "Boolean OR." (||)
                    , mkDefEntry "not" "Boolean NOT." not
                    ]
              , docTreeEntry "Text" [mkDefEntry "++" "Concatenate text." qappend]
              , docTreeEntry
                    "Numeric"
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
              "Set Aggregation"
              [ mkDefEntry "count" "Count of non-null literals in a set." $
                pinaforeSetFunc @baseedit @TopType @Int olength
              , mkDefEntry "sum" "Sum of numbers in a set." $ pinaforeSetFunc @baseedit @Number @Number sum
              , mkDefEntry "mean" "Mean of numbers in a set." $
                pinaforeSetFunc @baseedit @Number @Number $ \s -> sum s / fromIntegral (olength s)
              ]
        , docTreeEntry
              "Orders"
              [ mkDefEntry "alphabetical" "Alphabetical order." $ alphabetical @baseedit
              , mkDefEntry "numerical" "Numercal order." $ numerical @baseedit
              --, mkDefEntry "chronological" "Chronological order." $ chronological @baseedit
              , mkDefEntry "orders" "Join orders by priority." $ orders @baseedit @A
              , mkDefEntry "orderon" "Order by an order on a particular morphism." $ orderon @baseedit @A @B
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
              [ mkDefEntry "pass" "Do nothing." (return () :: PinaforeAction baseedit)
              , mkDefEntry ">>" "Do actions in sequence." $
                ((>>) :: PinaforeAction baseedit -> PinaforeAction baseedit -> PinaforeAction baseedit)
              , mkDefEntry "fail" "Fail, causing the program to terminate with error." $ qfail @baseedit
              , mkDefEntry "with" "Perform an action on the value of a reference." $ pinaforeReferenceWith @baseedit @A
              , mkDefEntry
                    "for"
                    "Perform an action on each value of a list."
                    (for_ :: [A] -> (A -> PinaforeAction baseedit) -> PinaforeAction baseedit)
              , mkDefEntry "output" "Output text to standard output." $ output @baseedit
              , mkDefEntry "outputln" "Output text and a newline to standard output." $ outputln @baseedit
              , mkDefEntry ":=" "Set a reference to a value." $ setentity @baseedit
              , mkDefEntry "delete" "Delete an entity reference." $ deleteentity @baseedit
              , mkDefEntry "newentity" "Create a new entity in a set and act on it." $ newentity @baseedit
              , mkDefEntry "get" "Get a reference." $ getentity @baseedit
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
              [ mkDefEntry "openwindow" "Open a new window with this title and UI." viewOpenWindow
              , mkDefEntry "openselection" "Open the item selected in the UI of this window." viewOpenSelection
              , mkDefEntry "withselection" "Act with the item selected in the UI of this window." $
                withSelection @baseedit
              , mkDefEntry "ui_blank" "Blank user-interface" uiNull
              , mkDefEntry "ui_unitcheckbox" "(TBD)" $ \name val ->
                    uiCheckbox (clearText . name) $ toEditLens knowBool . val
              , mkDefEntry "ui_booleancheckbox" "Checkbox. Use shift-click to set to null." $ \name val ->
                    uiMaybeCheckbox (clearText . name) $ (bijectionWholeEditLens knowMaybe) . val
              , mkDefEntry "ui_textentry" "Text entry, empty text is null." $
                valSpecText $ uiUnknownValue mempty uiTextEntry
              , mkDefEntry "ui_textarea" "Text area, empty text is null." $
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
              , mkDefEntry "ui_button" "A button with this text that does this action." $ \(name :: QFuncValue baseedit Text) action ->
                    uiButton (clearText . name) action
              , mkDefEntry "ui_pick" "A drop-down menu." $ ui_pick
                -- switch
              , mkDefEntry
                    "ui_table"
                    "A list table. First arg is columns (name, property), second is the window to open for a selection, third is the set of items." $
                ui_table @baseedit
              ]
        ]

predefinedDoc ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => DocTree DefDoc
predefinedDoc = fmap snd $ predefinitions @baseedit

predefinedBindings ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => QBindList baseedit
predefinedBindings = mconcat $ catMaybes $ toList $ fmap fst $ predefinitions @baseedit
