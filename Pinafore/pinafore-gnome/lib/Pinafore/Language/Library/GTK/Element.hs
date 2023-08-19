module Pinafore.Language.Library.GTK.Element
    ( elementStuff
    , actionRef
    , ElementContext(..)
    , LangElement(..)
    ) where

import Changes.Core
import Changes.World.GNOME.GTK
import Data.Media.Image hiding (Unknown)
import Data.Shim
import Data.Time
import Pinafore.Base
import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Context
import Pinafore.Language.Library.GTK.Element.Context
import Pinafore.Language.Library.Media
import Shapes

-- LangLayoutElement
data LangLayoutElement =
    MkLangLayoutElement LayoutOptions
                        LangElement

layoutElementGroundType :: QGroundType '[] LangLayoutElement
layoutElementGroundType =
    stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangLayoutElement)|]) "Layout.Element.GTK."

instance HasQGroundType '[] LangLayoutElement where
    qGroundType = layoutElementGroundType

clearText :: ChangeLens (WholeUpdate (Know Text)) (ROWUpdate Text)
clearText = funcChangeLens (fromKnow mempty)

uiListTable ::
       HasCallStack
    => [(LangWholeModel '( BottomType, Text), A -> LangWholeModel '( BottomType, Text))]
    -> LangListModel '( BottomType, EnA)
    -> (A -> Action TopType)
    -> Maybe (LangWholeModel '( A, EnA))
    -> LangElement
uiListTable cols lref onDoubleClick mSelectionLangRef =
    MkLangElement $ \MkElementContext {..} -> do
        esrc <- gvNewEditSource
        let
            mSelectionModel :: Maybe (Model (BiWholeUpdate (Know A) (Know EnA)))
            mSelectionModel = fmap (unWModel . langWholeModelToBiWholeModel) mSelectionLangRef
            readSub :: Model (ROWUpdate EnA) -> View A
            readSub sub =
                viewRunResource sub $ \asub -> do
                    ea <- aModelRead asub ReadWhole
                    return $ meet2 ea
            onSelect :: Model (ROWUpdate EnA) -> GView 'Locked ()
            onSelect osub =
                gvRunUnlocked $
                gvLiftView $ do
                    a <- readSub osub
                    viewRunAction ecUnlift $ void $ onDoubleClick a
            getColumn ::
                   (LangWholeModel '( BottomType, Text), A -> LangWholeModel '( BottomType, Text))
                -> KeyColumn (ROWUpdate EnA)
            getColumn (nameRef, getCellRef) = let
                showCell :: Know Text -> (Text, TableCellProps)
                showCell (Known s) = (s, plainTableCellProps)
                showCell Unknown = ("unknown", plainTableCellProps {tcStyle = plainTextStyle {tsItalic = True}})
                nameOpenSub :: Model (ROWUpdate Text)
                nameOpenSub = unWModel $ eaMapSemiReadOnly clearText $ langWholeModelToReadOnlyValue nameRef
                getCellSub :: Model (ROWUpdate EnA) -> GView 'Unlocked (Model (ROWUpdate (Text, TableCellProps)))
                getCellSub osub = do
                    a <- gvLiftView $ readSub osub
                    return $
                        unWModel $
                        eaMapSemiReadOnly (funcChangeLens showCell) $ langWholeModelToReadOnlyValue $ getCellRef a
                in readOnlyKeyColumn nameOpenSub getCellSub
            tsn :: SelectNotify (Model (ROWUpdate EnA))
            tsn =
                case mSelectionModel of
                    Nothing -> mempty
                    Just selectionModel ->
                        contramap readSub $ viewLiftSelectNotify $ modelSelectNotify esrc selectionModel
        (widget, setSelection) <-
            createListTable (fmap getColumn cols) (unWModel $ langListModelToOrdered lref) onSelect tsn
        for_ mSelectionModel $ \selectionModel -> let
            setsel :: Know EnA -> GView 'Unlocked ()
            setsel Unknown = setSelection Nothing
            setsel (Known a) =
                setSelection $
                Just $ do
                    a' <- readM ReadWhole
                    return $ a' == a
            finit :: GView 'Unlocked ()
            finit =
                gvLiftViewWithUnlift $ \unlift ->
                    viewRunResourceContext selectionModel $ \unliftR (amod :: _ tt) -> do
                        ka <- liftIO $ unliftR $ aModelRead amod ReadWhole
                        unlift $ setsel ka
            recv :: () -> NonEmpty (BiWholeUpdate (Know A) (Know EnA)) -> GView 'Unlocked ()
            recv () updates = let
                MkBiWholeUpdate ka = last updates
                in setsel ka
            in gvBindModel selectionModel (Just esrc) finit mempty recv
        return widget

uiList :: (ImmutableWholeModel A -> LangElement) -> LangListModel '( BottomType, A) -> LangElement
uiList mkElement listModel =
    MkLangElement $ \ec ->
        createListBox (\model -> unLangElement (mkElement $ functionImmutableModel $ MkWModel model) ec) $
        unWModel $ langListModelToOrdered listModel

type PickerType = Know EnA

type PickerPairType = (PickerType, ComboBoxCell)

uiPick :: ImmutableWholeModel (Vector (EnA, Text)) -> LangWholeModel '( A, EnA) -> LangElement
uiPick itemsRef ref =
    MkLangElement $ \_ -> do
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
            subOpts = unWModel $ eaMapSemiReadOnly itemsLens $ immutableModelToReadOnlyModel itemsRef
            subVal :: Model (WholeUpdate PickerType)
            subVal = unWModel $ langWholeModelToValue $ contraRangeLift meet2 ref
        createComboBox subOpts subVal

actionRef :: (View --> IO) -> ImmutableWholeModel (Action TopType) -> WROWModel (Maybe (GView 'Locked ()))
actionRef unlift raction =
    eaMapReadOnlyWhole
        (fmap (\action -> gvRunUnlocked $ gvLiftView $ viewRunAction unlift $ action >> return ()) . knowToMaybe) $
    immutableModelToReadOnlyModel raction

uiButton :: ImmutableWholeModel Text -> ImmutableWholeModel (Action TopType) -> LangElement
uiButton text raction =
    MkLangElement $ \MkElementContext {..} ->
        createButton
            (unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableModelToReadOnlyModel text)
            (unWModel $ actionRef ecUnlift raction)

uiLabel :: ImmutableWholeModel Text -> LangElement
uiLabel text =
    MkLangElement $ \_ ->
        createLabel $ unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableModelToReadOnlyModel text

uiDynamic :: ImmutableWholeModel LangElement -> LangElement
uiDynamic uiref =
    MkLangElement $ \ec -> let
        getSpec :: Know LangElement -> GView 'Unlocked Widget
        getSpec Unknown = createBlank
        getSpec (Known (MkLangElement pui)) = pui ec
        in createDynamic $ unWModel $ eaMapReadOnlyWhole getSpec $ immutableModelToReadOnlyModel uiref

uiScrolled :: LangElement -> LangElement
uiScrolled (MkLangElement lui) =
    MkLangElement $ \ec -> do
        w <- lui ec
        createScrolled w

uiUnitCheckBox :: ImmutableWholeModel Text -> WModel (WholeUpdate (Know ())) -> LangElement
uiUnitCheckBox name val =
    MkLangElement $ \_ ->
        createCheckButton (unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableModelToReadOnlyModel name) $
        unWModel $ eaMap (toChangeLens knowBool) val

uiCheckBox :: ImmutableWholeModel Text -> WModel (WholeUpdate (Know Bool)) -> LangElement
uiCheckBox name val =
    MkLangElement $ \_ ->
        createMaybeCheckButton (unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableModelToReadOnlyModel name) $
        unWModel $ eaMap (toChangeLens knowMaybe) val

uiTextEntry :: WModel (WholeUpdate (Know Text)) -> LangElement
uiTextEntry val = MkLangElement $ \_ -> createTextEntry $ unWModel $ eaMap (unknownValueChangeLens mempty) $ val

uiLayout :: Orientation -> [LangLayoutElement] -> LangElement
uiLayout orientation mitems =
    MkLangElement $ \ec -> do
        items <-
            for mitems $ \(MkLangLayoutElement lopts (MkLangElement lui)) -> do
                ui <- lui ec
                return (lopts, ui)
        createLayout orientation items

layoutGrow :: LangLayoutElement -> LangLayoutElement
layoutGrow (MkLangLayoutElement lopts ui) = MkLangLayoutElement (lopts {loGrow = True}) ui

uiNotebook :: LangWholeModel '( Int, TopType) -> [(LangElement, LangElement)] -> LangElement
uiNotebook selref mitems =
    MkLangElement $ \ec -> do
        items <-
            for mitems $ \(MkLangElement mt, MkLangElement mb) -> do
                t <- mt ec
                b <- mb ec
                return (t, b)
        createNotebook (langWholeModelSelectNotify noEditSource selref) items

uiExec :: Action LangElement -> LangElement
uiExec pui =
    MkLangElement $ \ec -> do
        kui <- gvLiftView $ unliftAction pui
        case kui of
            Known (MkLangElement ui) -> ui ec
            Unknown -> createBlank

uiStyleSheet :: ImmutableWholeModel Text -> LangElement -> LangElement
uiStyleSheet cssmodel (MkLangElement mw) =
    MkLangElement $ \ec -> do
        widget <- mw ec
        bindCSS True maxBound (unWModel $ immutableWholeModelValue mempty cssmodel) widget
        return widget

uiName :: Text -> LangElement -> LangElement
uiName name (MkLangElement mw) =
    MkLangElement $ \ec -> do
        widget <- mw ec
        gvRunLocked $ setCSSName name widget
        return widget

uiStyleClass :: Text -> LangElement -> LangElement
uiStyleClass sclass (MkLangElement mw) =
    MkLangElement $ \ec -> do
        widget <- mw ec
        gvRunLocked $ setCSSClass sclass widget
        return widget

uiTextArea :: LangTextModel -> LangElement
uiTextArea (MkLangTextModel model) =
    MkLangElement $ \ec ->
        createTextArea (unWModel model) $
        contramap (\tsel -> fmap (TextSelectionModel . MkLangTextModel) $ viewFloatMap tsel model) $
        viewLiftSelectNotify $ ecSelectNotify ec

uiCalendar :: WModel (WholeUpdate (Know Day)) -> LangElement
uiCalendar day =
    MkLangElement $ \_ -> createCalendar $ unWModel $ eaMap (unknownValueChangeLens $ fromGregorian 1970 01 01) day

uiWithContext :: (LangContext -> LangElement) -> LangElement
uiWithContext call =
    MkLangElement $ \ec -> do
        gtkc <- gvGetContext
        unLangElement (call $ MkLangContext gtkc $ ecOtherContext ec) ec

uiNotifySelection :: (Action LangTextModel -> Action ()) -> LangElement -> LangElement
uiNotifySelection notify (MkLangElement e) = let
    sel :: SelectNotify SelectionModel
    sel =
        MkSelectNotify $ \vms ->
            runAction $ notify $ actionLiftViewKnow $ fmap (fmap (\(TextSelectionModel x) -> x) . maybeToKnow) vms
    in MkLangElement $ \ec -> e (ec {ecSelectNotify = ecSelectNotify ec <> sel})

uiOwned :: LangElement -> LangElement
uiOwned (MkLangElement mw) =
    MkLangElement $ \ec ->
        gvLiftViewWithUnlift $ \unliftView ->
            liftIOWithUnlift $ \unliftIO -> unliftIO $ unliftView $ mw ec {ecUnlift = unliftIO}

langImage :: ImmutableWholeModel LangImage -> LangElement
langImage ref =
    MkLangElement $ \_ ->
        createImage $
        unWModel $
        eaMapReadOnlyWhole (fmap (someConvertImage . unLangImage) . knowToMaybe) $ immutableModelToReadOnlyModel ref

elementStuff :: BindDocStuff ()
elementStuff =
    headingBDS
        "Element"
        ""
        [ typeBDS
              "Element"
              "A user interface element is something that goes inside a window."
              (MkSomeGroundType elementGroundType)
              []
        , namespaceBDS
              "Element"
              [ typeBDS "Layout" "An Element in the context of a layout." (MkSomeGroundType layoutElementGroundType) []
              , hasSubtypeRelationBDS @LangElement @LangLayoutElement Verify "" $
                functionToShim "layout element" $ MkLangLayoutElement defaultLayoutOptions
              , valBDS "exec" "Element that runs an Action first." uiExec
              , valBDS "withContext" "Element that requires a Context." uiWithContext
              , valBDS "notifySelection" "Notify whenever the selection changes." uiNotifySelection
              , valBDS "owned" "Run actions caused by this element in the window's lifecycle." uiOwned
              , valBDS "blank" "Blank element" $ MkLangElement $ \_ -> createBlank
              , valBDS "image" "Blank element" langImage
              , valBDS "unitCheckBox" "(TBD)" uiUnitCheckBox
              , valBDS "checkBox" "Checkbox. Use shift-click to set to unknown." uiCheckBox
              , valBDS
                    "textEntry"
                    "Text entry, unknown reference will be interpreted as empty text, but the element will not delete the reference."
                    uiTextEntry
              , valBDS
                    "textArea"
                    "Text area, unknown reference will be interpreted as empty text, but the element will not delete the reference." $
                uiTextArea
              , valBDS "label" "Label." uiLabel
              , valBDS "horizontal" "Elements laid out horizontally." $ uiLayout OrientationHorizontal
              , valBDS "vertical" "Elements laid out vertically." $ uiLayout OrientationVertical
              , valBDS "layoutGrow" "Allow the element to expand into remaining space within the layout." layoutGrow
              , valBDS
                    "notebook"
                    "A notebook of pages. First of each pair is for the page tab (typically a label), second is the content."
                    uiNotebook
                    -- drag
                    -- icon
              , valBDS
                    "button"
                    "A button with this text that does this action. Button will be disabled if the action reference is unknown."
                    uiButton
              , valBDS "pick" "A drop-down menu." uiPick
              , valBDS "list" "A dynamic list of elements." uiList
              , valBDS
                    "listTable"
                    "A list table. First arg is columns (name, property), second is list-reference of items, third is the action for item activation, fourth is an optional reference for the selected row."
                    uiListTable
              , valBDS "calendar" "A calendar." uiCalendar
              , valBDS "scrolled" "A scrollable container." uiScrolled
              , valBDS "dynamic" "An element that can be updated to different UIs." uiDynamic
              , valBDS
                    "name"
                    "An element with name set. You can use something like `#text` to refer to it in the CSS style-sheet."
                    uiName
              , valBDS
                    "styleClass"
                    "An element with CSS class set. You can use something like `.text` to refer to all elements in this class in the CSS style-sheet."
                    uiStyleClass
              , valBDS
                    "styleSheet"
                    "An element with a CSS style-sheet (applied to the whole tree of elements). \
                        \See the GTK+ CSS [overview](https://developer.gnome.org/gtk3/stable/chap-css-overview.html) and [properties](https://developer.gnome.org/gtk3/stable/chap-css-properties.html) for how this works."
                    uiStyleSheet
              ]
        ]
