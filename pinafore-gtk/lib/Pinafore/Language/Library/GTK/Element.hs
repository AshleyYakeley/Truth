{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.Element
    ( elementStuff
    , actionRef
    , LangUIElement(..)
    ) where

import Changes.Core
import Changes.UI.GTK
import Data.Shim
import Data.Time
import GI.Cairo.Render
import GI.Gtk.Objects.DrawingArea
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Drawing
import Shapes

newtype LangUIElement =
    MkLangUIElement (CreateView Widget)

elementGroundType :: PinaforeGroundType '[] LangUIElement
elementGroundType =
    SimpleGroundType NilListType NilDolanVarianceMap ("Element", 0) $
    MkProvidedType $(iowitness [t|'MkWitKind (HetEqual LangUIElement)|]) HetRefl

-- LangUIElement
instance ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) LangUIElement where
    toShimWit = mkShimWit $ GroundDolanSingularType elementGroundType NilDolanArguments

instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) LangUIElement where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) LangUIElement where
    fromShimWit = mkShimWit $ GroundDolanSingularType elementGroundType NilDolanArguments

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) LangUIElement where
    fromShimWit = singleDolanShimWit fromJMShimWit

clearText :: ChangeLens (WholeUpdate (Know Text)) (ROWUpdate Text)
clearText = funcChangeLens (fromKnow mempty)

uiListTable ::
       (HasCallStack, ?pinafore :: PinaforeContext)
    => [(LangWholeRef '( BottomType, Text), A -> LangWholeRef '( BottomType, Text))]
    -> LangRefOrder A
    -> LangFiniteSetRef '( A, EnA)
    -> (A -> PinaforeAction TopType)
    -> Maybe (LangWholeRef '( A, EnA))
    -> LangUIElement
uiListTable cols order val onDoubleClick mSelectionLangRef =
    MkLangUIElement $ do
        let
            mSelectionModel :: Maybe (Model (BiWholeUpdate (Know A) (Know EnA)))
            mSelectionModel = fmap (unWModel . langWholeRefToBiWholeRef) mSelectionLangRef
            uo :: UpdateOrder (ContextUpdate PinaforeStorageUpdate (ConstWholeUpdate EnA))
            uo =
                mapUpdateOrder
                    (changeLensToFloating $
                     liftContextChangeLens $ fromReadOnlyRejectingChangeLens . funcChangeLens (Known . meet2)) $
                pinaforeUpdateOrder order
            rows :: Model (FiniteSetUpdate EnA)
            rows = unWModel $ unLangFiniteSetRef $ contraRangeLift meet2 val
            pkSub :: Model (ContextUpdate PinaforeStorageUpdate (FiniteSetUpdate EnA))
            pkSub = contextModels pinaforeEntityModel rows
            readSub :: Model (ConstWholeUpdate EnA) -> View A
            readSub sub =
                viewRunResource sub $ \asub -> do
                    ea <- aModelRead asub ReadWhole
                    return $ meet2 ea
            onSelect :: Model (ConstWholeUpdate EnA) -> View ()
            onSelect osub = do
                a <- readSub osub
                runPinaforeAction $ void $ onDoubleClick a
            getColumn ::
                   (LangWholeRef '( BottomType, Text), A -> LangWholeRef '( BottomType, Text))
                -> KeyColumn (ConstWholeUpdate EnA)
            getColumn (nameRef, getCellRef) = let
                showCell :: Know Text -> (Text, TableCellProps)
                showCell (Known s) = (s, plainTableCellProps)
                showCell Unknown = ("unknown", plainTableCellProps {tcStyle = plainTextStyle {tsItalic = True}})
                nameOpenSub :: Model (ROWUpdate Text)
                nameOpenSub = unWModel $ eaMapSemiReadOnly clearText $ langWholeRefToReadOnlyValue nameRef
                getCellSub :: Model (ConstWholeUpdate EnA) -> CreateView (Model (ROWUpdate (Text, TableCellProps)))
                getCellSub osub = do
                    a <- liftToLifeCycle $ readSub osub
                    return $
                        unWModel $
                        eaMapSemiReadOnly (funcChangeLens showCell) $ langWholeRefToReadOnlyValue $ getCellRef a
                in readOnlyKeyColumn nameOpenSub getCellSub
        colSub :: Model (ContextUpdate PinaforeStorageUpdate (OrderedListUpdate [EnA] (ConstWholeUpdate EnA))) <-
            cvFloatMapModel (contextOrderedSetLens uo) pkSub
        esrc <- newEditSource
        let
            olsub :: Model (OrderedListUpdate [EnA] (ConstWholeUpdate EnA))
            olsub = mapModel (tupleChangeLens SelectContent) colSub
            tsn :: SelectNotify (Model (ConstWholeUpdate EnA))
            tsn =
                case mSelectionModel of
                    Nothing -> mempty
                    Just selectionModel ->
                        contramap readSub $ viewLiftSelectNotify $ modelSelectNotify esrc selectionModel
        (widget, setSelection) <- createListTable (fmap getColumn cols) olsub onSelect tsn
        case mSelectionModel of
            Nothing -> return ()
            Just selectionModel -> let
                setsel :: Know EnA -> View ()
                setsel Unknown = setSelection Nothing
                setsel (Known a) =
                    setSelection $
                    Just $ do
                        a' <- readM ReadWhole
                        return $ a' == a
                init :: CreateView ()
                init =
                    viewRunResourceContext selectionModel $ \unlift (amod :: _ tt) -> do
                        ka <- liftIO $ unlift $ aModelRead amod ReadWhole
                        liftToLifeCycle $ setsel ka
                recv :: () -> NonEmpty (BiWholeUpdate (Know A) (Know EnA)) -> View ()
                recv () updates = let
                    MkBiWholeUpdate ka = last updates
                    in setsel ka
                in cvBindModel selectionModel (Just esrc) init mempty recv
        return widget

type PickerType = Know EnA

type PickerPairType = (PickerType, ComboBoxCell)

uiPick :: PinaforeImmutableWholeRef ([(EnA, Text)]) -> LangWholeRef '( A, EnA) -> LangUIElement
uiPick itemsRef ref =
    MkLangUIElement $ do
        let
            mapItem :: (EnA, Text) -> PickerPairType
            mapItem (ea, t) = (Known ea, plainComboBoxCell t)
            mapItems :: Know [(EnA, Text)] -> [PickerPairType]
            mapItems Unknown = []
            mapItems (Known items) =
                ( Unknown
                , (plainComboBoxCell "unknown") {cbcDefault = True, cbcStyle = plainTextStyle {tsItalic = True}}) :
                fmap mapItem items
            itemsLens ::
                   ChangeLens (WholeUpdate (Know [(EnA, Text)])) (ReadOnlyUpdate (OrderedListUpdate [PickerPairType] (ConstWholeUpdate PickerPairType)))
            itemsLens =
                liftReadOnlyChangeLens (toReadOnlyChangeLens . listOrderedListChangeLens) . funcChangeLens mapItems
            subOpts :: Model (ReadOnlyUpdate (OrderedListUpdate [PickerPairType] (ConstWholeUpdate PickerPairType)))
            subOpts = unWModel $ eaMapSemiReadOnly itemsLens $ immutableRefToReadOnlyRef itemsRef
            subVal :: Model (WholeUpdate PickerType)
            subVal = unWModel $ langWholeRefToValue $ contraRangeLift meet2 ref
        createComboBox subOpts subVal

actionRef ::
       (?pinafore :: PinaforeContext)
    => PinaforeImmutableWholeRef (PinaforeAction TopType)
    -> PinaforeROWRef (Maybe (View ()))
actionRef raction =
    eaMapReadOnlyWhole (fmap (\action -> runPinaforeAction (action >> return ())) . knowToMaybe) $
    immutableRefToReadOnlyRef raction

uiButton ::
       (?pinafore :: PinaforeContext)
    => PinaforeImmutableWholeRef Text
    -> PinaforeImmutableWholeRef (PinaforeAction TopType)
    -> LangUIElement
uiButton text raction =
    MkLangUIElement $
    createButton
        (unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef text)
        (unWModel $ actionRef raction)

uiLabel :: PinaforeImmutableWholeRef Text -> LangUIElement
uiLabel text =
    MkLangUIElement $ createLabel $ unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef text

uiDynamic :: PinaforeImmutableWholeRef LangUIElement -> LangUIElement
uiDynamic uiref = let
    getSpec :: Know LangUIElement -> CreateView Widget
    getSpec Unknown = createBlank
    getSpec (Known (MkLangUIElement pui)) = pui
    in MkLangUIElement $ createDynamic $ unWModel $ eaMapReadOnlyWhole getSpec $ immutableRefToReadOnlyRef uiref

uiScrolled :: LangUIElement -> LangUIElement
uiScrolled (MkLangUIElement lui) = MkLangUIElement $ lui >>= createScrolled

uiUnitCheckBox :: PinaforeImmutableWholeRef Text -> WModel (WholeUpdate (Know ())) -> LangUIElement
uiUnitCheckBox name val =
    MkLangUIElement $
    createCheckButton (unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef name) $
    unWModel $ eaMap (toChangeLens knowBool) val

uiCheckBox :: PinaforeImmutableWholeRef Text -> WModel (WholeUpdate (Know Bool)) -> LangUIElement
uiCheckBox name val =
    MkLangUIElement $
    createMaybeCheckButton (unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef name) $
    unWModel $ eaMap (toChangeLens knowMaybe) val

uiTextEntry :: WModel (WholeUpdate (Know Text)) -> LangUIElement
uiTextEntry val = MkLangUIElement $ createTextEntry $ unWModel $ eaMap (unknownValueChangeLens mempty) $ val

uiHorizontal :: [(Bool, LangUIElement)] -> LangUIElement
uiHorizontal mitems =
    MkLangUIElement $ do
        items <-
            for mitems $ \(f, MkLangUIElement lui) -> do
                ui <- lui
                return (f, ui)
        createLayout OrientationHorizontal items

uiVertical :: [(Bool, LangUIElement)] -> LangUIElement
uiVertical mitems =
    MkLangUIElement $ do
        items <-
            for mitems $ \(f, MkLangUIElement lui) -> do
                ui <- lui
                return (f, ui)
        createLayout OrientationVertical items

uiNotebook :: LangWholeRef '( Int, TopType) -> [(LangUIElement, LangUIElement)] -> LangUIElement
uiNotebook selref mitems =
    MkLangUIElement $ do
        items <-
            for mitems $ \(MkLangUIElement mt, MkLangUIElement mb) -> do
                t <- mt
                b <- mb
                return (t, b)
        createNotebook (langWholeRefSelectNotify noEditSource selref) items

uiRun :: (?pinafore :: PinaforeContext) => PinaforeAction LangUIElement -> LangUIElement
uiRun pui =
    MkLangUIElement $ do
        kui <- unliftPinaforeAction pui
        case kui of
            Known (MkLangUIElement ui) -> ui
            Unknown -> createBlank

uiStyleSheet :: PinaforeImmutableWholeRef Text -> LangUIElement -> LangUIElement
uiStyleSheet cssmodel (MkLangUIElement mw) =
    MkLangUIElement $ do
        widget <- mw
        bindCSS True maxBound (unWModel $ pinaforeImmutableRefValue mempty cssmodel) widget
        return widget

uiName :: Text -> LangUIElement -> LangUIElement
uiName name (MkLangUIElement mw) =
    MkLangUIElement $ do
        widget <- mw
        setCSSName name widget
        return widget

uiStyleClass :: Text -> LangUIElement -> LangUIElement
uiStyleClass sclass (MkLangUIElement mw) =
    MkLangUIElement $ do
        widget <- mw
        setCSSClass sclass widget
        return widget

uiTextArea :: WModel (WholeUpdate (Know Text)) -> LangUIElement
uiTextArea val =
    MkLangUIElement $ createTextArea (unWModel $ eaMap (convertChangeLens . unknownValueChangeLens mempty) val) mempty

uiCalendar :: WModel (WholeUpdate (Know Day)) -> LangUIElement
uiCalendar day =
    MkLangUIElement $ createCalendar $ unWModel $ eaMap (unknownValueChangeLens $ fromGregorian 1970 01 01) day

convertDrawing :: ((Int32, Int32) -> LangDrawing) -> DrawingArea -> Render ()
convertDrawing drawing widget = do
    w <- #getAllocatedWidth widget
    h <- #getAllocatedHeight widget
    unLangDrawing $ drawing (w, h)

uiDraw :: PinaforeImmutableWholeRef ((Int32, Int32) -> LangDrawing) -> LangUIElement
uiDraw ref =
    MkLangUIElement $ createCairo $ unWModel $ pinaforeImmutableRefValue (\_ -> return ()) $ fmap convertDrawing ref

elementStuff :: DocTreeEntry BindDoc
elementStuff =
    docTreeEntry
        "Element"
        ""
        [ mkTypeEntry "Element" "A user interface element is something that goes inside a window." $
          MkBoundType elementGroundType
        , mkValEntry "run" "Element that runs an Action first." uiRun
        , mkValEntry "blank" "Blank element" $ MkLangUIElement createBlank
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
        , mkValEntry
              "horizontal"
              "Items arranged horizontally, each flag is whether to expand into remaining space."
              uiHorizontal
        , mkValEntry
              "vertical"
              "Items arranged vertically, each flag is whether to expand into remaining space."
              uiVertical
        , mkValEntry
              "notebook"
              "A notebook of pages. First of each pair is for the page tab (typically a label), second is the content."
              uiNotebook
                -- CSS
                -- drag
                -- icon
        , mkValEntry
              "button"
              "A button with this text that does this action. Button will be disabled if the action reference is unknown."
              uiButton
        , mkValEntry "pick" "A drop-down menu." uiPick
        , mkValEntry
              "listTable"
              "A list table. First arg is columns (name, property), second is order, third is the set of items, fourth is the window to open for a selection, fifth is an optional reference for the selected row."
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
