module Pinafore.Language.Predefined.UI
    ( ui_predefinitions
    ) where

import Data.Shim
import Data.Time
import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.Predefined.Defs
import Pinafore.Language.Value
import Pinafore.Storage
import Shapes
import Truth.Core

clearText :: ChangeLens (WholeUpdate (Know Text)) (ROWUpdate Text)
clearText = funcChangeLens (fromKnow mempty)

uiTable ::
       forall baseupdate.
       ( ?pinafore :: PinaforeContext baseupdate
       , BaseChangeLens PinaforeEntityUpdate baseupdate {-, ApplicableEdit (UpdateEdit baseupdate)-}
       )
    => [(LangRef '( BottomType, Text), A -> LangRef '( BottomType, Text))]
    -> LangOrder baseupdate A
    -> LangFiniteSetRef '( A, EnA)
    -> (A -> PinaforeAction TopType)
    -> SelectNotify A
    -> LangUI
uiTable cols order val onDoubleClick sn = do
    let
        uo :: UpdateOrder (ContextUpdate baseupdate (ConstWholeUpdate EnA))
        uo =
            mapUpdateOrder
                (changeLensToFloating $
                 liftContextChangeLens $ fromReadOnlyRejectingChangeLens . funcChangeLens (Known . meet2)) $
            pinaforeUpdateOrder order
        rows :: Model (FiniteSetUpdate EnA)
        rows = unPinaforeRef $ unLangFiniteSetRef $ contraRangeLift meet2 val
        pkSub :: Model (ContextUpdate baseupdate (FiniteSetUpdate EnA))
        pkSub = contextModels pinaforeBase rows
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
    colSub :: Model (ContextUpdate baseupdate (OrderedListUpdate [EnA] (ConstWholeUpdate EnA))) <-
        cvFloatMapModel (contextOrderedSetLens uo) pkSub
    let
        olsub :: Model (OrderedListUpdate [EnA] (ConstWholeUpdate EnA))
        olsub = mapModel (tupleChangeLens SelectContent) colSub
        tsn :: SelectNotify (Model (ConstWholeUpdate EnA))
        tsn = contramap readSub $ viewLiftSelectNotify sn
    tableUISpec (fmap getColumn cols) olsub onSelect tsn

type PickerType = Know EnA

type PickerPairType = (PickerType, OptionUICell)

uiPick :: PinaforeImmutableRef ([(EnA, Text)]) -> LangRef '( A, EnA) -> CVUISpec
uiPick itemsRef ref = do
    let
        mapItem :: (EnA, Text) -> PickerPairType
        mapItem (ea, t) = (Known ea, plainOptionUICell t)
        mapItems :: Know [(EnA, Text)] -> [PickerPairType]
        mapItems Unknown = []
        mapItems (Known items) = fmap mapItem items
        itemsLens ::
               ChangeLens (WholeUpdate (Know [(EnA, Text)])) (ReadOnlyUpdate (OrderedListUpdate [PickerPairType] (ConstWholeUpdate PickerPairType)))
        itemsLens = liftReadOnlyChangeLens (toReadOnlyChangeLens . listOrderedListChangeLens) . funcChangeLens mapItems
        subOpts :: Model (ReadOnlyUpdate (OrderedListUpdate [PickerPairType] (ConstWholeUpdate PickerPairType)))
        subOpts = pinaforeRefModel $ eaMapSemiReadOnly itemsLens $ immutableRefToReadOnlyRef itemsRef
        subVal :: Model (WholeUpdate PickerType)
        subVal = pinaforeRefModel $ langRefToValue $ contraRangeLift meet2 ref
    optionUISpec subOpts subVal

actionRef ::
       (?pinafore :: PinaforeContext baseupdate)
    => PinaforeImmutableRef (PinaforeAction TopType)
    -> PinaforeROWRef (Maybe (View ()))
actionRef raction =
    eaMapReadOnlyWhole (fmap (\action -> runPinaforeAction (action >> return ())) . knowToMaybe) $
    immutableRefToReadOnlyRef raction

uiButton ::
       forall baseupdate. (?pinafore :: PinaforeContext baseupdate)
    => PinaforeImmutableRef Text
    -> PinaforeImmutableRef (PinaforeAction TopType)
    -> CVUISpec
uiButton text raction =
    buttonUISpec
        (pinaforeRefModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef text)
        (pinaforeRefModel $ actionRef raction)

uiLabel :: PinaforeImmutableRef Text -> CVUISpec
uiLabel text = labelUISpec $ pinaforeRefModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef text

uiDynamic :: PinaforeImmutableRef LangUI -> LangUI
uiDynamic uiref = let
    getSpec :: Know LangUI -> CVUISpec
    getSpec Unknown = nullUISpec
    getSpec (Known pui) = pui
    in switchUISpec $ pinaforeRefModel $ eaMapReadOnlyWhole getSpec $ immutableRefToReadOnlyRef uiref

openWindow ::
       (?pinafore :: PinaforeContext baseupdate)
    => PinaforeImmutableRef Text
    -> PinaforeImmutableRef MenuBar
    -> LangUI
    -> PinaforeAction PinaforeWindow
openWindow title mbar wsContent = do
    mfix $ \w ->
        pinaforeNewWindow $ let
            wsCloseBoxAction :: View ()
            wsCloseBoxAction = pwClose w
            wsTitle :: Model (ROWUpdate Text)
            wsTitle = pinaforeRefModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef title
            wsMenuBar :: Maybe (Model (ROWUpdate MenuBar))
            wsMenuBar = Just $ pinaforeRefModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef mbar
            in MkWindowSpec {..}

uiTextArea :: PinaforeRef (WholeUpdate (Know Text)) -> CVUISpec
uiTextArea val =
    textAreaUISpec (pinaforeRefModel $ eaMap (convertChangeLens . unknownValueChangeLens mempty) val) mempty

uiCalendar :: PinaforeRef (WholeUpdate (Know Day)) -> CVUISpec
uiCalendar day = calendarUISpec $ pinaforeRefModel $ eaMap (unknownValueChangeLens $ fromGregorian 1970 01 01) day

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
       forall baseupdate. (?pinafore :: PinaforeContext baseupdate)
    => Text
    -> Maybe Text
    -> PinaforeImmutableRef (PinaforeAction TopType)
    -> MenuEntry
menuAction label maccelStr raction = let
    maccel = do
        accelStr <- maccelStr
        interpretAccelerator $ unpack accelStr
    in ActionMenuEntry label maccel $ pinaforeRefModel $ actionRef raction

uiScrolled :: LangUI -> LangUI
uiScrolled = scrolledUISpec

uiUnitCheckBox :: PinaforeImmutableRef Text -> PinaforeRef (WholeUpdate (Know ())) -> CVUISpec
uiUnitCheckBox name val =
    checkboxUISpec (pinaforeRefModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef name) $
    pinaforeRefModel $ eaMap (toChangeLens knowBool) val

uiCheckBox :: PinaforeImmutableRef Text -> PinaforeRef (WholeUpdate (Know Bool)) -> CVUISpec
uiCheckBox name val =
    maybeCheckboxUISpec (pinaforeRefModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef name) $
    pinaforeRefModel $ eaMap (toChangeLens knowMaybe) val

uiTextEntry :: PinaforeRef (WholeUpdate (Know Text)) -> CVUISpec
uiTextEntry val = textEntryUISpec $ pinaforeRefModel $ eaMap (unknownValueChangeLens mempty) $ val

uiHorizontal :: [(LangUI, Bool)] -> LangUI
uiHorizontal = horizontalUISpec

uiVertical :: [(LangUI, Bool)] -> LangUI
uiVertical = verticalUISpec

uiPages :: [(LangUI, LangUI)] -> LangUI
uiPages = pagesUISpec

noNotifier :: LangNotifier TopType
noNotifier = mempty

notifiers :: [LangNotifier A] -> LangNotifier A
notifiers = mconcat

mapNotifier :: (B -> A) -> LangNotifier A -> LangNotifier B
mapNotifier = contramap

makeNotifier :: PinaforeAction (LangNotifier A, PinaforeAction A)
makeNotifier = do
    (sn, getsel) <- liftIO makeRefSelectNotify
    let
        action = do
            ma <- viewPinaforeAction getsel
            pinaforeActionKnow $ maybeToKnow ma
    return (sn, action)

notify :: LangNotifier A -> Maybe A -> PinaforeAction ()
notify notifier ma = viewPinaforeAction $ runSelectNotify notifier $ return ma

uiRun :: (?pinafore :: PinaforeContext baseupdate) => PinaforeAction LangUI -> LangUI
uiRun pui = do
    kui <- cvLiftView $ unliftPinaforeAction pui
    case kui of
        Known ui -> ui
        Unknown -> nullUISpec

ui_predefinitions ::
       forall baseupdate. (BaseChangeLens PinaforeEntityUpdate baseupdate, BaseChangeLens PinaforeFileUpdate baseupdate)
    => [DocTreeEntry (BindDoc baseupdate)]
ui_predefinitions =
    [ docTreeEntry
          "Notifiers"
          "Notifiers are used to track selections in some UI elements."
          [ mkValEntry "noNotifier" "No notifier, same as `notifiers []`." noNotifier
          , mkValEntry "notifiers" "Join notifiers." notifiers
          , mkValEntry "mapNotifier" "Map notifier." mapNotifier
          , mkValEntry "makeNotifier" "Create a notifier." makeNotifier
          , mkValEntry "notify" "Notify a notifier." notify
          ]
    , docTreeEntry
          "UI"
          "A user interface is something that goes inside a window."
          [ mkValEntry "uiRun" "UI that runs an Action first." uiRun
          , mkValEntry "uiBlank" "Blank user-interface" nullUISpec
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
                "A list table. First arg is columns (name, property), second is order, third is the set of items, fourth is the window to open for a selection." $
            uiTable @baseupdate
          , mkValEntry "uiCalendar" "A calendar." uiCalendar
          , mkValEntry "uiScrolled" "A scrollable container." uiScrolled
          , mkValEntry "uiDynamic" "A UI that can be updated to different UIs." uiDynamic
          ]
    , docTreeEntry
          "Menu"
          "Menu items."
          [ mkValEntry "menuSeparator" "Separator menu item." SeparatorMenuEntry
          , mkValEntry "menuSubmenu" "Submenu menu item." SubMenuEntry
          , mkValEntry "menuAction" "Action menu item. Item will be disabled if the action reference is unknown." $
            menuAction @baseupdate
          ]
    , docTreeEntry
          "Window"
          "User interface windows."
          [ mkValEntry "openWindow" "Open a new window with this title and UI." openWindow
          , mkValEntry "closeWindow" "Close a window." pwClose
          , mkValEntry "exitUI" "Exit the user interface." pinaforeExit
          ]
    ]
