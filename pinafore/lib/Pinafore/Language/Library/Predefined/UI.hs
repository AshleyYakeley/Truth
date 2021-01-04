module Pinafore.Language.Library.Predefined.UI
    ( ui_predefinitions
    ) where

import Changes.Core
import Changes.UI.GTK
import Data.Shim
import Data.Time
import Pinafore.Base
import Pinafore.Context
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Value
import Pinafore.Language.Var
import Shapes

clearText :: ChangeLens (WholeUpdate (Know Text)) (ROWUpdate Text)
clearText = funcChangeLens (fromKnow mempty)

uiListTable ::
       (HasCallStack, ?pinafore :: PinaforeContext)
    => [(LangWholeRef '( BottomType, Text), A -> LangWholeRef '( BottomType, Text))]
    -> LangRefOrder A
    -> LangFiniteSetRef '( A, EnA)
    -> (A -> PinaforeAction TopType)
    -> Maybe (LangWholeRef '( A, EnA))
    -> LangUI
uiListTable cols order val onDoubleClick mSelectionLangRef =
    MkLangUI $ do
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
                        contramap readSub $
                        viewLiftSelectNotify $
                        MkSelectNotify $ \vma -> do
                            ma <- vma
                            viewRunResource selectionModel $ \asub -> do
                                _ <-
                                    pushEdit esrc $
                                    aModelEdit asub $ pure $ MkBiEdit $ MkWholeReaderEdit $ maybeToKnow ma
                                return ()
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

uiPick :: PinaforeImmutableWholeRef ([(EnA, Text)]) -> LangWholeRef '( A, EnA) -> LangUI
uiPick itemsRef ref =
    MkLangUI $ do
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
    -> LangUI
uiButton text raction =
    MkLangUI $
    createButton
        (unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef text)
        (unWModel $ actionRef raction)

uiLabel :: PinaforeImmutableWholeRef Text -> LangUI
uiLabel text = MkLangUI $ createLabel $ unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef text

uiDynamic :: PinaforeImmutableWholeRef LangUI -> LangUI
uiDynamic uiref = let
    getSpec :: Know LangUI -> CreateView Widget
    getSpec Unknown = createBlank
    getSpec (Known (MkLangUI pui)) = pui
    in MkLangUI $ createDynamic $ unWModel $ eaMapReadOnlyWhole getSpec $ immutableRefToReadOnlyRef uiref

openWindow ::
       (?pinafore :: PinaforeContext)
    => PinaforeImmutableWholeRef Text
    -> PinaforeImmutableWholeRef MenuBar
    -> LangUI
    -> PinaforeAction LangWindow
openWindow title mbar (MkLangUI wsContent) =
    mfix $ \w ->
        pinaforeNewWindow $ let
            wsCloseBoxAction :: View ()
            wsCloseBoxAction = pwClose w
            wsTitle :: Model (ROWUpdate Text)
            wsTitle = unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef title
            wsMenuBar :: Maybe (Model (ROWUpdate MenuBar))
            wsMenuBar = Just $ unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef mbar
            in MkWindowSpec {..}

uiTextArea :: WModel (WholeUpdate (Know Text)) -> LangUI
uiTextArea val =
    MkLangUI $ createTextArea (unWModel $ eaMap (convertChangeLens . unknownValueChangeLens mempty) val) mempty

uiCalendar :: WModel (WholeUpdate (Know Day)) -> LangUI
uiCalendar day = MkLangUI $ createCalendar $ unWModel $ eaMap (unknownValueChangeLens $ fromGregorian 1970 01 01) day

interpretAccelerator :: String -> Maybe MenuAccelerator
interpretAccelerator [c] = Just $ MkMenuAccelerator [] c
interpretAccelerator ('C':'t':'r':'l':'+':s) = do
    MkMenuAccelerator mods c <- interpretAccelerator s
    return $ MkMenuAccelerator (KMCtrl : mods) c
interpretAccelerator ('S':'h':'i':'f':'t':'+':s) = do
    MkMenuAccelerator mods c <- interpretAccelerator s
    return $ MkMenuAccelerator (KMShift : mods) c
interpretAccelerator ('A':'l':'t':'+':s) = do
    MkMenuAccelerator mods c <- interpretAccelerator s
    return $ MkMenuAccelerator (KMAlt : mods) c
interpretAccelerator _ = Nothing

menuAction ::
       (?pinafore :: PinaforeContext)
    => Text
    -> Maybe Text
    -> PinaforeImmutableWholeRef (PinaforeAction TopType)
    -> LangMenuEntry
menuAction label maccelStr raction = let
    maccel = do
        accelStr <- maccelStr
        interpretAccelerator $ unpack accelStr
    in ActionMenuEntry label maccel $ unWModel $ actionRef raction

uiScrolled :: LangUI -> LangUI
uiScrolled (MkLangUI lui) = MkLangUI $ lui >>= createScrolled

uiUnitCheckBox :: PinaforeImmutableWholeRef Text -> WModel (WholeUpdate (Know ())) -> LangUI
uiUnitCheckBox name val =
    MkLangUI $
    createCheckButton (unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef name) $
    unWModel $ eaMap (toChangeLens knowBool) val

uiCheckBox :: PinaforeImmutableWholeRef Text -> WModel (WholeUpdate (Know Bool)) -> LangUI
uiCheckBox name val =
    MkLangUI $
    createMaybeCheckButton (unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef name) $
    unWModel $ eaMap (toChangeLens knowMaybe) val

uiTextEntry :: WModel (WholeUpdate (Know Text)) -> LangUI
uiTextEntry val = MkLangUI $ createTextEntry $ unWModel $ eaMap (unknownValueChangeLens mempty) $ val

uiHorizontal :: [(Bool, LangUI)] -> LangUI
uiHorizontal mitems =
    MkLangUI $ do
        items <-
            for mitems $ \(f, MkLangUI lui) -> do
                ui <- lui
                return (f, ui)
        createLayout OrientationHorizontal items

uiVertical :: [(Bool, LangUI)] -> LangUI
uiVertical mitems =
    MkLangUI $ do
        items <-
            for mitems $ \(f, MkLangUI lui) -> do
                ui <- lui
                return (f, ui)
        createLayout OrientationVertical items

uiPages :: [(LangUI, LangUI)] -> LangUI
uiPages mitems =
    MkLangUI $ do
        items <-
            for mitems $ \(MkLangUI mt, MkLangUI mb) -> do
                t <- mt
                b <- mb
                return (t, b)
        createNotebook items

uiRun :: (?pinafore :: PinaforeContext) => PinaforeAction LangUI -> LangUI
uiRun pui =
    MkLangUI $ do
        kui <- unliftPinaforeAction pui
        case kui of
            Known (MkLangUI ui) -> ui
            Unknown -> createBlank

uiStyleSheet :: PinaforeImmutableWholeRef Text -> LangUI -> LangUI
uiStyleSheet cssmodel (MkLangUI mw) =
    MkLangUI $ do
        widget <- mw
        bindCSS True maxBound (unWModel $ pinaforeImmutableRefValue mempty cssmodel) widget
        return widget

uiName :: Text -> LangUI -> LangUI
uiName name (MkLangUI mw) =
    MkLangUI $ do
        widget <- mw
        setCSSName name widget
        return widget

uiStyleClass :: Text -> LangUI -> LangUI
uiStyleClass sclass (MkLangUI mw) =
    MkLangUI $ do
        widget <- mw
        setCSSClass sclass widget
        return widget

ui_predefinitions :: [DocTreeEntry BindDoc]
ui_predefinitions =
    [ docTreeEntry
          "UI"
          "A user interface is something that goes inside a window."
          [ mkValEntry "uiRun" "UI that runs an Action first." uiRun
          , mkValEntry "uiBlank" "Blank user-interface" $ MkLangUI createBlank
          , mkValEntry "uiUnitCheckBox" "(TBD)" uiUnitCheckBox
          , mkValEntry "uiCheckBox" "Checkbox. Use shift-click to set to unknown." uiCheckBox
          , mkValEntry
                "uiTextEntry"
                "Text entry, unknown reference will be interpreted as empty text, but the UI will not delete the reference."
                uiTextEntry
          , mkValEntry
                "uiTextArea"
                "Text area, unknown reference will be interpreted as empty text, but the UI will not delete the reference." $
            uiTextArea
          , mkValEntry "uiLabel" "Label." uiLabel
          , mkValEntry
                "uiHorizontal"
                "Items arranged horizontally, each flag is whether to expand into remaining space."
                uiHorizontal
          , mkValEntry
                "uiVertical"
                "Items arranged vertically, each flag is whether to expand into remaining space."
                uiVertical
          , mkValEntry
                "uiPages"
                "A notebook of pages. First of each pair is for the page tab (typically a label), second is the content."
                uiPages
                -- CSS
                -- drag
                -- icon
          , mkValEntry
                "uiButton"
                "A button with this text that does this action. Button will be disabled if the action reference is unknown."
                uiButton
          , mkValEntry "uiPick" "A drop-down menu." uiPick
          , mkValEntry
                "uiListTable"
                "A list table. First arg is columns (name, property), second is order, third is the set of items, fourth is the window to open for a selection, fifth is an optional reference for the selected row."
                uiListTable
          , mkValEntry "uiCalendar" "A calendar." uiCalendar
          , mkValEntry "uiScrolled" "A scrollable container." uiScrolled
          , mkValEntry "uiDynamic" "A UI that can be updated to different UIs." uiDynamic
          , mkValEntry
                "uiName"
                "A UI with name set. You can use something like `#text` to refer to it in the CSS style-sheet."
                uiName
          , mkValEntry
                "uiStyleClass"
                "A UI with CSS class set. You can use something like `.text` to refer to all elements in this class in the CSS style-sheet."
                uiStyleClass
          , mkValEntry
                "uiStyleSheet"
                "A UI with a CSS style-sheet (applied to the whole tree of UI elements). \
                    \See the GTK+ CSS [overview](https://developer.gnome.org/gtk3/stable/chap-css-overview.html) and [properties](https://developer.gnome.org/gtk3/stable/chap-css-properties.html) for how this works."
                uiStyleSheet
          ]
    , docTreeEntry
          "Menu"
          "Menu items."
          [ mkValEntry "menuSeparator" "Separator menu item." SeparatorMenuEntry
          , mkValEntry "menuSubmenu" "Submenu menu item." SubMenuEntry
          , mkValEntry
                "menuAction"
                "Action menu item. Item will be disabled if the action reference is unknown."
                menuAction
          ]
    , docTreeEntry
          "Window"
          "User interface windows."
          [ mkValEntry "openWindow" "Open a new window with this title and UI." openWindow
          , mkValEntry "closeWindow" "Close a window." pwClose
          , mkValEntry "showWindow" "Show a window." uiWindowShow
          , mkValEntry "hideWindow" "Hide a window." uiWindowHide
          , mkValEntry "exitUI" "Exit the user interface." pinaforeExit
          ]
    ]
