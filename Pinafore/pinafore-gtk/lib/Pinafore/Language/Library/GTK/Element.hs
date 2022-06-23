{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.Element
    ( elementStuff
    , actionRef
    , LangElement(..)
    ) where

import Changes.Core
import Changes.UI.GTK
import Data.Shim
import Data.Time
import GI.Cairo.Render
import Pinafore.Base
import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Context ()
import Pinafore.Language.Library.GTK.Drawing
import Shapes

-- LangElement
newtype LangElement = MkLangElement
    { unLangElement :: GView 'Locked Widget
    }

elementGroundType :: PinaforeGroundType '[] LangElement
elementGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangElement)|]) "Element"

instance HasPinaforeGroundType '[] LangElement where
    pinaforeGroundType = elementGroundType

-- LangLayoutElement
data LangLayoutElement =
    MkLangLayoutElement LayoutOptions
                        LangElement

layoutElementGroundType :: PinaforeGroundType '[] LangLayoutElement
layoutElementGroundType =
    stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangLayoutElement)|]) "LayoutElement"

instance HasPinaforeGroundType '[] LangLayoutElement where
    pinaforeGroundType = layoutElementGroundType

clearText :: ChangeLens (WholeUpdate (Know Text)) (ROWUpdate Text)
clearText = funcChangeLens (fromKnow mempty)

uiListTable ::
       (HasCallStack, ?pinafore :: PinaforeContext)
    => [(LangWholeRef '( BottomType, Text), A -> LangWholeRef '( BottomType, Text))]
    -> LangListRef '( BottomType, EnA)
    -> (A -> PinaforeAction TopType)
    -> Maybe (LangWholeRef '( A, EnA))
    -> LangElement
uiListTable cols lref onDoubleClick mSelectionLangRef =
    MkLangElement $ do
        esrc <- newEditSource
        let
            mSelectionModel :: Maybe (Model (BiWholeUpdate (Know A) (Know EnA)))
            mSelectionModel = fmap (unWModel . langWholeRefToBiWholeRef) mSelectionLangRef
            readSub :: Model (ROWUpdate EnA) -> View A
            readSub sub =
                viewRunResource sub $ \asub -> do
                    ea <- aModelRead asub ReadWhole
                    return $ meet2 ea
            onSelect :: Model (ROWUpdate EnA) -> GView 'Locked ()
            onSelect osub = do
                a <- gvLiftViewNoUI $ readSub osub
                gvRunUnlocked $ gvLiftView $ runPinaforeAction $ void $ onDoubleClick a
            getColumn ::
                   (LangWholeRef '( BottomType, Text), A -> LangWholeRef '( BottomType, Text))
                -> KeyColumn (ROWUpdate EnA)
            getColumn (nameRef, getCellRef) = let
                showCell :: Know Text -> (Text, TableCellProps)
                showCell (Known s) = (s, plainTableCellProps)
                showCell Unknown = ("unknown", plainTableCellProps {tcStyle = plainTextStyle {tsItalic = True}})
                nameOpenSub :: Model (ROWUpdate Text)
                nameOpenSub = unWModel $ eaMapSemiReadOnly clearText $ langWholeRefToReadOnlyValue nameRef
                getCellSub :: Model (ROWUpdate EnA) -> GView 'Locked (Model (ROWUpdate (Text, TableCellProps)))
                getCellSub osub = do
                    a <- gvLiftViewNoUI $ readSub osub
                    return $
                        unWModel $
                        eaMapSemiReadOnly (funcChangeLens showCell) $ langWholeRefToReadOnlyValue $ getCellRef a
                in readOnlyKeyColumn nameOpenSub getCellSub
            tsn :: SelectNotify (Model (ROWUpdate EnA))
            tsn =
                case mSelectionModel of
                    Nothing -> mempty
                    Just selectionModel ->
                        contramap readSub $ viewLiftSelectNotify $ modelSelectNotify esrc selectionModel
        (widget, setSelection) <-
            createListTable (fmap getColumn cols) (unWModel $ langListRefToOrdered lref) onSelect tsn
        for_ mSelectionModel $ \selectionModel -> let
            setsel :: Know EnA -> GView 'Locked ()
            setsel Unknown = setSelection Nothing
            setsel (Known a) =
                setSelection $
                Just $ do
                    a' <- readM ReadWhole
                    return $ a' == a
            init :: GView 'Locked ()
            init =
                gvRunResourceContext selectionModel $ \unlift (amod :: _ tt) -> do
                    ka <- liftIO $ unlift $ aModelRead amod ReadWhole
                    setsel ka
            recv :: () -> NonEmpty (BiWholeUpdate (Know A) (Know EnA)) -> GView 'Unlocked ()
            recv () updates = let
                MkBiWholeUpdate ka = last updates
                in gvRunLocked $ setsel ka
            in gvBindModel selectionModel (Just esrc) init mempty recv
        return widget

uiList :: (PinaforeImmutableWholeRef A -> LangElement) -> LangListRef '( BottomType, A) -> LangElement
uiList mkElement listRef =
    MkLangElement $
    createListBox (unLangElement . mkElement . functionImmutableRef . MkWModel) $
    unWModel $ langListRefToOrdered listRef

type PickerType = Know EnA

type PickerPairType = (PickerType, ComboBoxCell)

uiPick :: PinaforeImmutableWholeRef (Vector (EnA, Text)) -> LangWholeRef '( A, EnA) -> LangElement
uiPick itemsRef ref =
    MkLangElement $ do
        let
            mapItem :: (EnA, Text) -> PickerPairType
            mapItem (ea, t) = (Known ea, plainComboBoxCell t)
            emptyPickerPairType :: PickerPairType
            emptyPickerPairType =
                ( Unknown
                , (plainComboBoxCell "unknown") {cbcDefault = True, cbcStyle = plainTextStyle {tsItalic = True}})
            mapItems :: Know (Vector (EnA, Text)) -> Vector PickerPairType
            mapItems Unknown = mempty
            mapItems (Known items) = cons emptyPickerPairType $ fmap mapItem items
            itemsLens ::
                   ChangeLens (WholeUpdate (Know (Vector (EnA, Text)))) (ReadOnlyUpdate (OrderedListUpdate (ConstWholeUpdate PickerPairType)))
            itemsLens =
                liftReadOnlyChangeLens (toReadOnlyChangeLens . listOrderedListChangeLens) . funcChangeLens mapItems
            subOpts :: Model (ReadOnlyUpdate (OrderedListUpdate (ConstWholeUpdate PickerPairType)))
            subOpts = unWModel $ eaMapSemiReadOnly itemsLens $ immutableRefToReadOnlyRef itemsRef
            subVal :: Model (WholeUpdate PickerType)
            subVal = unWModel $ langWholeRefToValue $ contraRangeLift meet2 ref
        createComboBox subOpts subVal

actionRef ::
       (?pinafore :: PinaforeContext)
    => PinaforeImmutableWholeRef (PinaforeAction TopType)
    -> PinaforeROWRef (Maybe (GView 'Locked ()))
actionRef raction =
    eaMapReadOnlyWhole
        (fmap (\action -> gvRunUnlocked $ gvLiftView $ runPinaforeAction (action >> return ())) . knowToMaybe) $
    immutableRefToReadOnlyRef raction

uiButton ::
       (?pinafore :: PinaforeContext)
    => PinaforeImmutableWholeRef Text
    -> PinaforeImmutableWholeRef (PinaforeAction TopType)
    -> LangElement
uiButton text raction =
    MkLangElement $
    createButton
        (unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef text)
        (unWModel $ actionRef raction)

uiLabel :: PinaforeImmutableWholeRef Text -> LangElement
uiLabel text =
    MkLangElement $ createLabel $ unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef text

uiDynamic :: PinaforeImmutableWholeRef LangElement -> LangElement
uiDynamic uiref = let
    getSpec :: Know LangElement -> GView 'Locked Widget
    getSpec Unknown = createBlank
    getSpec (Known (MkLangElement pui)) = pui
    in MkLangElement $ createDynamic $ unWModel $ eaMapReadOnlyWhole getSpec $ immutableRefToReadOnlyRef uiref

uiScrolled :: LangElement -> LangElement
uiScrolled (MkLangElement lui) = MkLangElement $ lui >>= createScrolled

uiUnitCheckBox :: PinaforeImmutableWholeRef Text -> WModel (WholeUpdate (Know ())) -> LangElement
uiUnitCheckBox name val =
    MkLangElement $
    createCheckButton (unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef name) $
    unWModel $ eaMap (toChangeLens knowBool) val

uiCheckBox :: PinaforeImmutableWholeRef Text -> WModel (WholeUpdate (Know Bool)) -> LangElement
uiCheckBox name val =
    MkLangElement $
    createMaybeCheckButton (unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef name) $
    unWModel $ eaMap (toChangeLens knowMaybe) val

uiTextEntry :: WModel (WholeUpdate (Know Text)) -> LangElement
uiTextEntry val = MkLangElement $ createTextEntry $ unWModel $ eaMap (unknownValueChangeLens mempty) $ val

uiLayout :: Orientation -> [LangLayoutElement] -> LangElement
uiLayout orientation mitems =
    MkLangElement $ do
        items <-
            for mitems $ \(MkLangLayoutElement lopts (MkLangElement lui)) -> do
                ui <- lui
                return (lopts, ui)
        createLayout orientation items

layoutGrow :: LangLayoutElement -> LangLayoutElement
layoutGrow (MkLangLayoutElement lopts ui) = MkLangLayoutElement (lopts {loGrow = True}) ui

uiNotebook :: LangWholeRef '( Int, TopType) -> [(LangElement, LangElement)] -> LangElement
uiNotebook selref mitems =
    MkLangElement $ do
        items <-
            for mitems $ \(MkLangElement mt, MkLangElement mb) -> do
                t <- mt
                b <- mb
                return (t, b)
        createNotebook (langWholeRefSelectNotify noEditSource selref) items

uiExec :: (?pinafore :: PinaforeContext) => PinaforeAction LangElement -> LangElement
uiExec pui =
    MkLangElement $ do
        kui <- gvRunUnlocked $ gvLiftView $ unliftPinaforeAction pui
        case kui of
            Known (MkLangElement ui) -> ui
            Unknown -> createBlank

uiStyleSheet :: PinaforeImmutableWholeRef Text -> LangElement -> LangElement
uiStyleSheet cssmodel (MkLangElement mw) =
    MkLangElement $ do
        widget <- mw
        bindCSS True maxBound (unWModel $ pinaforeImmutableRefValue mempty cssmodel) widget
        return widget

uiName :: Text -> LangElement -> LangElement
uiName name (MkLangElement mw) =
    MkLangElement $ do
        widget <- mw
        setCSSName name widget
        return widget

uiStyleClass :: Text -> LangElement -> LangElement
uiStyleClass sclass (MkLangElement mw) =
    MkLangElement $ do
        widget <- mw
        setCSSClass sclass widget
        return widget

uiTextArea :: LangTextRef -> LangWholeRef '( PinaforeAction LangTextRef, TopType) -> LangElement
uiTextArea (MkLangTextRef model) selectionRef =
    MkLangElement $
    createTextArea (unWModel model) $
    contramap (\tsel -> fmap MkLangTextRef $ pinaforeFloatMap tsel model) $
    langWholeRefSelectNotify noEditSource selectionRef

uiCalendar :: WModel (WholeUpdate (Know Day)) -> LangElement
uiCalendar day =
    MkLangElement $ createCalendar $ unWModel $ eaMap (unknownValueChangeLens $ fromGregorian 1970 01 01) day

uiDraw :: PinaforeImmutableWholeRef ((Int32, Int32) -> LangDrawing) -> LangElement
uiDraw ref = MkLangElement $ createCairo $ unWModel $ pinaforeImmutableRefValue mempty ref

uiWithContext :: (GTKContext -> LangElement) -> LangElement
uiWithContext call =
    MkLangElement $ do
        gtkc <- gvGetContext
        unLangElement $ call gtkc

elementStuff :: DocTreeEntry BindDoc
elementStuff =
    docTreeEntry
        "Element"
        ""
        [ mkTypeEntry "Element" "A user interface element is something that goes inside a window." $
          MkBoundType elementGroundType
        , hasSubtypeRelationEntry @LangElement @LangLayoutElement "" $
          functionToShim "layout element" $ MkLangLayoutElement defaultLayoutOptions
        , mkValEntry "exec" "Element that runs an Action first." uiExec
        , mkValEntry "withContext" "Element that requires a Context." uiWithContext
        , mkValEntry "blank" "Blank element" $ MkLangElement createBlank
        , mkValEntry "draw" "Drawable element" uiDraw
        , mkValEntry "unitCheckBox" "(TBD)" uiUnitCheckBox
        , mkValEntry "checkBox" "Checkbox. Use shift-click to set to unknown." uiCheckBox
        , mkValEntry
              "textEntry"
              "Text entry, unknown reference will be interpreted as empty text, but the element will not delete the reference."
              uiTextEntry
        , mkValEntry
              "textArea"
              "Text area, unknown reference will be interpreted as empty text, but the element will not delete the reference." $
          uiTextArea
        , mkValEntry "label" "Label." uiLabel
        , mkValEntry "horizontal" "Items arranged horizontally, each flag is whether to expand into remaining space." $
          uiLayout OrientationHorizontal
        , mkValEntry "vertical" "Items arranged vertically, each flag is whether to expand into remaining space." $
          uiLayout OrientationVertical
        , mkValEntry "layoutGrow" "Grow layout element." layoutGrow
        , mkValEntry
              "notebook"
              "A notebook of pages. First of each pair is for the page tab (typically a label), second is the content."
              uiNotebook
                -- drag
                -- icon
        , mkValEntry
              "button"
              "A button with this text that does this action. Button will be disabled if the action reference is unknown."
              uiButton
        , mkValEntry "pick" "A drop-down menu." uiPick
        , mkValEntry "list" "A dynamic list of elements." uiList
        , mkValEntry
              "listTable"
              "A list table. First arg is columns (name, property), second is list-reference of items, third is the action for item activation, fourth is an optional reference for the selected row."
              uiListTable
        , mkValEntry "calendar" "A calendar." uiCalendar
        , mkValEntry "scrolled" "A scrollable container." uiScrolled
        , mkValEntry "dynamic" "An element that can be updated to different UIs." uiDynamic
        , mkValEntry
              "name"
              "An element with name set. You can use something like `#text` to refer to it in the CSS style-sheet."
              uiName
        , mkValEntry
              "styleClass"
              "An element with CSS class set. You can use something like `.text` to refer to all elements in this class in the CSS style-sheet."
              uiStyleClass
        , mkValEntry
              "styleSheet"
              "An element with a CSS style-sheet (applied to the whole tree of elements). \
                    \See the GTK+ CSS [overview](https://developer.gnome.org/gtk3/stable/chap-css-overview.html) and [properties](https://developer.gnome.org/gtk3/stable/chap-css-properties.html) for how this works."
              uiStyleSheet
        ]
