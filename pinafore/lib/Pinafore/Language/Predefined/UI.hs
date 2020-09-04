module Pinafore.Language.Predefined.UI
    ( ui_predefinitions
    ) where

import Data.Shim
import Data.Time
import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.Predefined.Defs
import Pinafore.Language.Value
import Pinafore.Language.Var
import Shapes
import Truth.Core
import Truth.UI.GTK

clearText :: ChangeLens (WholeUpdate (Know Text)) (ROWUpdate Text)
clearText = funcChangeLens (fromKnow mempty)

uiTable ::
       (HasCallStack, ?pinafore :: PinaforeContext)
    => [(LangRef '( BottomType, Text), A -> LangRef '( BottomType, Text))]
    -> LangOrder A
    -> LangFiniteSetRef '( A, EnA)
    -> (A -> PinaforeAction TopType)
    -> Maybe (LangRef '( A, EnA))
    -> LangUI
uiTable cols order val onDoubleClick mSelectionLangRef = do
    let
        mSelectionModel :: Maybe (Model (BiWholeUpdate (Know A) (Know EnA)))
        mSelectionModel = fmap (unPinaforeRef . langRefToBiWholeRef) mSelectionLangRef
        uo :: UpdateOrder (ContextUpdate PinaforeStorageUpdate (ConstWholeUpdate EnA))
        uo =
            mapUpdateOrder
                (changeLensToFloating $
                 liftContextChangeLens $ fromReadOnlyRejectingChangeLens . funcChangeLens (Known . meet2)) $
            pinaforeUpdateOrder order
        rows :: Model (FiniteSetUpdate EnA)
        rows = unPinaforeRef $ unLangFiniteSetRef $ contraRangeLift meet2 val
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
               (LangRef '( BottomType, Text), A -> LangRef '( BottomType, Text)) -> KeyColumn (ConstWholeUpdate EnA)
        getColumn (nameRef, getCellRef) = let
            showCell :: Know Text -> (Text, TableCellProps)
            showCell (Known s) = (s, plainTableCellProps)
            showCell Unknown = ("unknown", plainTableCellProps {tcStyle = plainTextStyle {tsItalic = True}})
            nameOpenSub :: Model (ROWUpdate Text)
            nameOpenSub = pinaforeRefModel $ eaMapSemiReadOnly clearText $ langRefToReadOnlyValue nameRef
            getCellSub :: Model (ConstWholeUpdate EnA) -> CreateView (Model (ROWUpdate (Text, TableCellProps)))
            getCellSub osub = do
                a <- cvLiftView $ readSub osub
                return $
                    pinaforeRefModel $
                    eaMapSemiReadOnly (funcChangeLens showCell) $ langRefToReadOnlyValue $ getCellRef a
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
                            _ <- pushEdit esrc $ aModelEdit asub $ pure $ MkBiEdit $ MkWholeReaderEdit $ maybeToKnow ma
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
                    cvLiftView $ setsel ka
            recv :: () -> NonEmpty (BiWholeUpdate (Know A) (Know EnA)) -> View ()
            recv () updates = let
                MkBiWholeUpdate ka = last updates
                in setsel ka
            in cvBindModel selectionModel (Just esrc) init mempty recv
    return widget

type PickerType = Know EnA

type PickerPairType = (PickerType, ComboBoxCell)

uiPick :: PinaforeImmutableRef ([(EnA, Text)]) -> LangRef '( A, EnA) -> CreateView Widget
uiPick itemsRef ref = do
    let
        mapItem :: (EnA, Text) -> PickerPairType
        mapItem (ea, t) = (Known ea, plainComboBoxCell t)
        mapItems :: Know [(EnA, Text)] -> [PickerPairType]
        mapItems Unknown = []
        mapItems (Known items) =
            (Unknown, (plainComboBoxCell "unknown") {cbcDefault = True, cbcStyle = plainTextStyle {tsItalic = True}}) :
            fmap mapItem items
        itemsLens ::
               ChangeLens (WholeUpdate (Know [(EnA, Text)])) (ReadOnlyUpdate (OrderedListUpdate [PickerPairType] (ConstWholeUpdate PickerPairType)))
        itemsLens = liftReadOnlyChangeLens (toReadOnlyChangeLens . listOrderedListChangeLens) . funcChangeLens mapItems
        subOpts :: Model (ReadOnlyUpdate (OrderedListUpdate [PickerPairType] (ConstWholeUpdate PickerPairType)))
        subOpts = pinaforeRefModel $ eaMapSemiReadOnly itemsLens $ immutableRefToReadOnlyRef itemsRef
        subVal :: Model (WholeUpdate PickerType)
        subVal = pinaforeRefModel $ langRefToValue $ contraRangeLift meet2 ref
    createComboBox subOpts subVal

actionRef ::
       (?pinafore :: PinaforeContext)
    => PinaforeImmutableRef (PinaforeAction TopType)
    -> PinaforeROWRef (Maybe (View ()))
actionRef raction =
    eaMapReadOnlyWhole (fmap (\action -> runPinaforeAction (action >> return ())) . knowToMaybe) $
    immutableRefToReadOnlyRef raction

uiButton ::
       (?pinafore :: PinaforeContext)
    => PinaforeImmutableRef Text
    -> PinaforeImmutableRef (PinaforeAction TopType)
    -> CreateView Widget
uiButton text raction =
    createButton
        (pinaforeRefModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef text)
        (pinaforeRefModel $ actionRef raction)

uiLabel :: PinaforeImmutableRef Text -> CreateView Widget
uiLabel text = createLabel $ pinaforeRefModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef text

uiDynamic :: PinaforeImmutableRef LangUI -> LangUI
uiDynamic uiref = let
    getSpec :: Know LangUI -> CreateView Widget
    getSpec Unknown = createBlank
    getSpec (Known pui) = pui
    in createDynamic $ pinaforeRefModel $ eaMapReadOnlyWhole getSpec $ immutableRefToReadOnlyRef uiref

openWindow ::
       (?pinafore :: PinaforeContext)
    => PinaforeImmutableRef Text
    -> PinaforeImmutableRef MenuBar
    -> LangUI
    -> PinaforeAction LangWindow
openWindow title mbar mContent = do
    wsContent <- createViewPinaforeAction mContent
    mfix $ \w ->
        pinaforeNewWindow $ let
            wsCloseBoxAction :: View ()
            wsCloseBoxAction = pwClose w
            wsTitle :: Model (ROWUpdate Text)
            wsTitle = pinaforeRefModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef title
            wsMenuBar :: Maybe (Model (ROWUpdate MenuBar))
            wsMenuBar = Just $ pinaforeRefModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef mbar
            in MkWindowSpec {..}

uiTextArea :: PinaforeRef (WholeUpdate (Know Text)) -> CreateView Widget
uiTextArea val =
    createTextArea (pinaforeRefModel $ eaMap (convertChangeLens . unknownValueChangeLens mempty) val) mempty

uiCalendar :: PinaforeRef (WholeUpdate (Know Day)) -> CreateView Widget
uiCalendar day = createCalendar $ pinaforeRefModel $ eaMap (unknownValueChangeLens $ fromGregorian 1970 01 01) day

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
    -> PinaforeImmutableRef (PinaforeAction TopType)
    -> LangMenuEntry
menuAction label maccelStr raction = let
    maccel = do
        accelStr <- maccelStr
        interpretAccelerator $ unpack accelStr
    in ActionMenuEntry label maccel $ pinaforeRefModel $ actionRef raction

uiScrolled :: LangUI -> LangUI
uiScrolled lui = lui >>= createScrolled

uiUnitCheckBox :: PinaforeImmutableRef Text -> PinaforeRef (WholeUpdate (Know ())) -> CreateView Widget
uiUnitCheckBox name val =
    createCheckButton (pinaforeRefModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef name) $
    pinaforeRefModel $ eaMap (toChangeLens knowBool) val

uiCheckBox :: PinaforeImmutableRef Text -> PinaforeRef (WholeUpdate (Know Bool)) -> CreateView Widget
uiCheckBox name val =
    createMaybeCheckButton (pinaforeRefModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef name) $
    pinaforeRefModel $ eaMap (toChangeLens knowMaybe) val

uiTextEntry :: PinaforeRef (WholeUpdate (Know Text)) -> CreateView Widget
uiTextEntry val = createTextEntry $ pinaforeRefModel $ eaMap (unknownValueChangeLens mempty) $ val

uiHorizontal :: [(Bool, LangUI)] -> LangUI
uiHorizontal mitems = do
    items <-
        for mitems $ \(f, lui) -> do
            ui <- lui
            return (f, ui)
    createLayout OrientationHorizontal items

uiVertical :: [(Bool, LangUI)] -> LangUI
uiVertical mitems = do
    items <-
        for mitems $ \(f, lui) -> do
            ui <- lui
            return (f, ui)
    createLayout OrientationVertical items

uiPages :: [(LangUI, LangUI)] -> LangUI
uiPages mitems = do
    items <-
        for mitems $ \(mt, mb) -> do
            t <- mt
            b <- mb
            return (t, b)
    createNotebook items

uiRun :: (?pinafore :: PinaforeContext) => PinaforeAction LangUI -> LangUI
uiRun pui = do
    kui <- cvLiftView $ unliftPinaforeAction pui
    case kui of
        Known ui -> ui
        Unknown -> createBlank

ui_predefinitions :: [DocTreeEntry BindDoc]
ui_predefinitions =
    [ docTreeEntry
          "UI"
          "A user interface is something that goes inside a window."
          [ mkValEntry "uiRun" "UI that runs an Action first." uiRun
          , mkValEntry "uiBlank" "Blank user-interface" createBlank
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
                "uiTable"
                "A list table. First arg is columns (name, property), second is order, third is the set of items, fourth is the window to open for a selection, fifth is an optional reference for the selected row."
                uiTable
          , mkValEntry "uiCalendar" "A calendar." uiCalendar
          , mkValEntry "uiScrolled" "A scrollable container." uiScrolled
          , mkValEntry "uiDynamic" "A UI that can be updated to different UIs." uiDynamic
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
          , mkValEntry "exitUI" "Exit the user interface." pinaforeExit
          ]
    ]
