module Pinafore.Language.Library.GTK.Widget
    ( widgetStuff
    , actionRef
    , WidgetContext(..)
    , LangWidget(..)
    ) where

import Changes.Core
import Changes.World.GNOME.GTK
import Data.Media.Image hiding (Unknown)
import Data.Shim
import Data.Time
import Pinafore.Base
import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Context
import Pinafore.Language.Library.GTK.Widget.Context
import Pinafore.Language.Library.Media
import Shapes

-- LangLayoutWidget
data LangLayoutWidget =
    MkLangLayoutWidget LayoutOptions
                       LangWidget

layoutWidgetGroundType :: QGroundType '[] LangLayoutWidget
layoutWidgetGroundType =
    stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangLayoutWidget)|]) "Layout.Widget.GTK."

instance HasQGroundType '[] LangLayoutWidget where
    qGroundType = layoutWidgetGroundType

clearText :: ChangeLens (WholeUpdate (Know Text)) (ROWUpdate Text)
clearText = funcChangeLens (fromKnow mempty)

uiListTable ::
       HasCallStack
    => [(LangWholeModel '( BottomType, Text), A -> LangWholeModel '( BottomType, Text))]
    -> LangListModel '( BottomType, EnA)
    -> (A -> Action TopType)
    -> Maybe (LangWholeModel '( A, EnA))
    -> LangWidget
uiListTable cols lref onDoubleClick mSelectionLangRef =
    MkLangWidget $ \MkWidgetContext {..} -> do
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
                    viewRunAction wcUnlift $ void $ onDoubleClick a
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

uiList :: (ImmutableWholeModel A -> LangWidget) -> LangListModel '( BottomType, A) -> LangWidget
uiList mkWidget listModel =
    MkLangWidget $ \ec ->
        createListBox (\model -> unLangWidget (mkWidget $ functionImmutableModel $ MkWModel model) ec) $
        unWModel $ langListModelToOrdered listModel

type PickerType = Know EnA

type PickerPairType = (PickerType, ComboBoxCell)

uiPick :: ImmutableWholeModel (Vector (EnA, Text)) -> LangWholeModel '( A, EnA) -> LangWidget
uiPick itemsRef ref =
    MkLangWidget $ \_ -> do
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

uiButton :: ImmutableWholeModel Text -> ImmutableWholeModel (Action TopType) -> LangWidget
uiButton text raction =
    MkLangWidget $ \MkWidgetContext {..} ->
        createButton
            (unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableModelToReadOnlyModel text)
            (unWModel $ actionRef wcUnlift raction)

uiLabel :: ImmutableWholeModel Text -> LangWidget
uiLabel text =
    MkLangWidget $ \_ ->
        createLabel $ unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableModelToReadOnlyModel text

uiDynamic :: ImmutableWholeModel LangWidget -> LangWidget
uiDynamic uiref =
    MkLangWidget $ \ec -> let
        getSpec :: Know LangWidget -> GView 'Unlocked Widget
        getSpec Unknown = createBlank
        getSpec (Known (MkLangWidget pui)) = pui ec
        in createDynamic $ unWModel $ eaMapReadOnlyWhole getSpec $ immutableModelToReadOnlyModel uiref

uiScrolled :: LangWidget -> LangWidget
uiScrolled (MkLangWidget lui) =
    MkLangWidget $ \ec -> do
        w <- lui ec
        createScrolled w

uiUnitCheckBox :: ImmutableWholeModel Text -> WModel (WholeUpdate (Know ())) -> LangWidget
uiUnitCheckBox name val =
    MkLangWidget $ \_ ->
        createCheckButton (unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableModelToReadOnlyModel name) $
        unWModel $ eaMap (toChangeLens knowBool) val

uiCheckBox :: ImmutableWholeModel Text -> WModel (WholeUpdate (Know Bool)) -> LangWidget
uiCheckBox name val =
    MkLangWidget $ \_ ->
        createMaybeCheckButton (unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableModelToReadOnlyModel name) $
        unWModel $ eaMap (toChangeLens knowMaybe) val

uiTextEntry :: WModel (WholeUpdate (Know Text)) -> LangWidget
uiTextEntry val = MkLangWidget $ \_ -> createTextEntry $ unWModel $ eaMap (unknownValueChangeLens mempty) $ val

uiLayout :: Orientation -> [LangLayoutWidget] -> LangWidget
uiLayout orientation mitems =
    MkLangWidget $ \ec -> do
        items <-
            for mitems $ \(MkLangLayoutWidget lopts (MkLangWidget lui)) -> do
                ui <- lui ec
                return (lopts, ui)
        createLayout orientation items

layoutGrow :: LangLayoutWidget -> LangLayoutWidget
layoutGrow (MkLangLayoutWidget lopts ui) = MkLangLayoutWidget (lopts {loGrow = True}) ui

uiNotebook :: LangWholeModel '( Int, TopType) -> [(LangWidget, LangWidget)] -> LangWidget
uiNotebook selref mitems =
    MkLangWidget $ \ec -> do
        items <-
            for mitems $ \(MkLangWidget mt, MkLangWidget mb) -> do
                t <- mt ec
                b <- mb ec
                return (t, b)
        createNotebook (langWholeModelSelectNotify noEditSource selref) items

uiExec :: Action LangWidget -> LangWidget
uiExec pui =
    MkLangWidget $ \ec -> do
        kui <- gvLiftView $ unliftAction pui
        case kui of
            Known (MkLangWidget ui) -> ui ec
            Unknown -> createBlank

uiStyleSheet :: ImmutableWholeModel Text -> LangWidget -> LangWidget
uiStyleSheet cssmodel (MkLangWidget mw) =
    MkLangWidget $ \ec -> do
        widget <- mw ec
        bindCSS True maxBound (unWModel $ immutableWholeModelValue mempty cssmodel) widget
        return widget

uiName :: Text -> LangWidget -> LangWidget
uiName name (MkLangWidget mw) =
    MkLangWidget $ \ec -> do
        widget <- mw ec
        gvRunLocked $ setCSSName name widget
        return widget

uiStyleClass :: Text -> LangWidget -> LangWidget
uiStyleClass sclass (MkLangWidget mw) =
    MkLangWidget $ \ec -> do
        widget <- mw ec
        gvRunLocked $ setCSSClass sclass widget
        return widget

uiTextArea :: LangTextModel -> LangWidget
uiTextArea (MkLangTextModel model) =
    MkLangWidget $ \ec ->
        createTextArea (unWModel model) $
        contramap (\tsel -> fmap (TextSelectionModel . MkLangTextModel) $ viewFloatMap tsel model) $
        viewLiftSelectNotify $ wcSelectNotify ec

uiCalendar :: WModel (WholeUpdate (Know Day)) -> LangWidget
uiCalendar day =
    MkLangWidget $ \_ -> createCalendar $ unWModel $ eaMap (unknownValueChangeLens $ fromGregorian 1970 01 01) day

uiWithContext :: (LangContext -> LangWidget) -> LangWidget
uiWithContext call =
    MkLangWidget $ \ec -> do
        gtkc <- gvGetContext
        unLangWidget (call $ MkLangContext gtkc $ wcOtherContext ec) ec

uiNotifySelection :: (Action LangTextModel -> Action ()) -> LangWidget -> LangWidget
uiNotifySelection notify (MkLangWidget e) = let
    sel :: SelectNotify SelectionModel
    sel =
        MkSelectNotify $ \vms ->
            runAction $ notify $ actionLiftViewKnow $ fmap (fmap (\(TextSelectionModel x) -> x) . maybeToKnow) vms
    in MkLangWidget $ \ec -> e (ec {wcSelectNotify = wcSelectNotify ec <> sel})

uiOwned :: LangWidget -> LangWidget
uiOwned (MkLangWidget mw) =
    MkLangWidget $ \ec ->
        gvLiftViewWithUnlift $ \unliftView ->
            liftIOWithUnlift $ \unliftIO -> unliftIO $ unliftView $ mw ec {wcUnlift = unliftIO}

langImage :: ImmutableWholeModel LangImage -> LangWidget
langImage ref =
    MkLangWidget $ \_ ->
        createImage $
        unWModel $
        eaMapReadOnlyWhole (fmap (someConvertImage . unLangImage) . knowToMaybe) $ immutableModelToReadOnlyModel ref

widgetStuff :: LibraryStuff ()
widgetStuff =
    headingBDS
        "Widget"
        ""
        [ typeBDS
              "Widget"
              "A user interface widget is something that goes inside a window."
              (MkSomeGroundType widgetGroundType)
              []
        , namespaceBDS
              "Widget"
              [ typeBDS "Layout" "A widget in the context of a layout." (MkSomeGroundType layoutWidgetGroundType) []
              , hasSubtypeRelationBDS @LangWidget @LangLayoutWidget Verify "" $
                functionToShim "layout widget" $ MkLangLayoutWidget defaultLayoutOptions
              , valBDS "exec" "A widget that runs an Action first." uiExec
              , valBDS "withContext" "A widget that requires a Context." uiWithContext
              , valBDS "notifySelection" "Notify whenever the selection changes." uiNotifySelection
              , valBDS "owned" "Run actions caused by this widget in the window's lifecycle." uiOwned
              , valBDS "blank" "Blank widget" $ MkLangWidget $ \_ -> createBlank
              , valBDS "image" "A widget for an image" langImage
              , valBDS "unitCheckBox" "(TBD)" uiUnitCheckBox
              , valBDS "checkBox" "Checkbox. Use shift-click to set to unknown." uiCheckBox
              , valBDS
                    "textEntry"
                    "Text entry, unknown reference will be interpreted as empty text, but the widget will not delete the reference."
                    uiTextEntry
              , valBDS
                    "textArea"
                    "Text area, unknown reference will be interpreted as empty text, but the widget will not delete the reference." $
                uiTextArea
              , valBDS "label" "Label." uiLabel
              , valBDS "horizontal" "Widgets laid out horizontally." $ uiLayout OrientationHorizontal
              , valBDS "vertical" "Widgets laid out vertically." $ uiLayout OrientationVertical
              , valBDS "layoutGrow" "Allow the widget to expand into remaining space within the layout." layoutGrow
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
              , valBDS "list" "A dynamic list of widgets." uiList
              , valBDS
                    "listTable"
                    "A list table. First arg is columns (name, property), second is list-reference of items, third is the action for item activation, fourth is an optional reference for the selected row."
                    uiListTable
              , valBDS "calendar" "A calendar." uiCalendar
              , valBDS "scrolled" "A scrollable container." uiScrolled
              , valBDS "dynamic" "A widget that can be updated to different UIs." uiDynamic
              , valBDS
                    "name"
                    "A widget with name set. You can use something like `#text` to refer to it in the CSS style-sheet."
                    uiName
              , valBDS
                    "styleClass"
                    "A widget with CSS class set. You can use something like `.text` to refer to all widgets in this class in the CSS style-sheet."
                    uiStyleClass
              , valBDS
                    "styleSheet"
                    "A widget with a CSS style-sheet (applied to the whole tree of widgets). \
                        \See the GTK+ CSS [overview](https://developer.gnome.org/gtk3/stable/chap-css-overview.html) and [properties](https://developer.gnome.org/gtk3/stable/chap-css-properties.html) for how this works."
                    uiStyleSheet
              ]
        ]
