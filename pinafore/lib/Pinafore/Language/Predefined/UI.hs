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
       , HasPinaforeEntityUpdate baseupdate {-, ApplicableEdit (UpdateEdit baseupdate)-}
       )
    => [(LangRef '( BottomType, Text), A -> LangRef '( BottomType, Text))]
    -> LangOrder baseupdate A
    -> LangFiniteSetRef '( A, EA)
    -> (A -> PinaforeAction TopType)
    -> SelectNotify A
    -> LangUI
uiTable cols order val onDoubleClick sn = do
    let
        uo :: UpdateOrder (ContextUpdate baseupdate (ConstWholeUpdate EA))
        uo =
            mapUpdateOrder
                (changeLensToFloating $
                 liftContextChangeLens $ fromReadOnlyRejectingChangeLens . funcChangeLens (Known . meet2)) $
            pinaforeUpdateOrder order
        rows :: Model (FiniteSetUpdate EA)
        rows = unPinaforeRef $ unLangFiniteSetRef $ contraRangeLift meet2 val
        pkSub :: Model (ContextUpdate baseupdate (FiniteSetUpdate EA))
        pkSub = contextModels pinaforeBase rows
        readSub :: Model (ConstWholeUpdate EA) -> View A
        readSub sub =
            viewRunResource sub $ \asub -> do
                ea <- aModelRead asub ReadWhole
                return $ meet2 ea
        onSelect :: Model (ConstWholeUpdate EA) -> View ()
        onSelect osub = do
            a <- readSub osub
            runPinaforeAction $ void $ onDoubleClick a
        getColumn ::
               (LangRef '( BottomType, Text), A -> LangRef '( BottomType, Text)) -> KeyColumn (ConstWholeUpdate EA)
        getColumn (nameRef, getCellRef) = let
            showCell :: Know Text -> (Text, TableCellProps)
            showCell (Known s) = (s, plainTableCellProps)
            showCell Unknown = ("unknown", plainTableCellProps {tcStyle = plainTextStyle {tsItalic = True}})
            nameOpenSub :: Model (ROWUpdate Text)
            nameOpenSub = pinaforeRefOpenModel $ eaMapSemiReadOnly clearText $ langRefToReadOnlyValue nameRef
            getCellSub :: Model (ConstWholeUpdate EA) -> CreateView (Model (ROWUpdate (Text, TableCellProps)))
            getCellSub osub = do
                a <- cvLiftView $ readSub osub
                return $
                    pinaforeRefOpenModel $
                    eaMapSemiReadOnly (funcChangeLens showCell) $ langRefToReadOnlyValue $ getCellRef a
            in readOnlyKeyColumn nameOpenSub getCellSub
    colSub :: Model (ContextUpdate baseupdate (OrderedListUpdate [EA] (ConstWholeUpdate EA))) <-
        cvFloatMapModel (contextOrderedSetLens uo) pkSub
    let
        olsub :: Model (OrderedListUpdate [EA] (ConstWholeUpdate EA))
        olsub = mapModel (tupleChangeLens SelectContent) colSub
        tsn :: SelectNotify (Model (ConstWholeUpdate EA))
        tsn = contramap readSub $ viewLiftSelectNotify sn
    tableUISpec (fmap getColumn cols) olsub onSelect tsn

type PickerType = Know EA

type PickerPairType = (PickerType, OptionUICell)

makeCell :: Know Text -> OptionUICell
makeCell Unknown = (plainOptionUICell "unknown") {optionCellStyle = plainTextStyle {tsItalic = True}}
makeCell (Known t) = plainOptionUICell t

uiPick ::
       forall baseupdate. (?pinafore :: PinaforeContext baseupdate)
    => LangMorphism baseupdate '( A, TopType) '( BottomType, Text)
    -> LangFiniteSetRef '( A, EA)
    -> LangRef '( A, EA)
    -> CVUISpec
uiPick nameMorphism fset ref = do
    let
        getName :: PinaforeFunctionMorphism baseupdate EA PickerPairType
        getName =
            proc p -> do
                n <- langMorphismFunction nameMorphism -< Known $ meet2 p
                returnA -< (Known p, makeCell n)
        getNames :: PinaforeFunctionMorphism baseupdate (FiniteSet EA) (FiniteSet PickerPairType)
        getNames =
            proc fsp -> do
                pairs <- cfmap getName -< fsp
                returnA -< insertSet (Unknown, makeCell Unknown) pairs
        updateOrder :: UpdateOrder (ConstWholeUpdate PickerPairType)
        updateOrder = MkUpdateOrder (comparing $ optionCellText . snd) $ changeLensToFloating convertReadOnlyChangeLens
        orderLens ::
               FloatingChangeLens (WholeUpdate (FiniteSet PickerPairType)) (ReadOnlyUpdate (OrderedListUpdate [PickerPairType] (ConstWholeUpdate PickerPairType)))
        --orderLens = (orderedKeyList {- @(FiniteSet PickerPairType) -} $ comparing $ optionCellText . snd) . convertChangeLens
        orderLens =
            changeLensToFloating toReadOnlyChangeLens .
            orderedSetLens updateOrder . changeLensToFloating convertChangeLens
    rc <- viewGetResourceContext
    opts :: PinaforeRef (ReadOnlyUpdate (OrderedListUpdate [PickerPairType] (ConstWholeUpdate PickerPairType))) <-
        liftLifeCycleIO $
        eaFloatMapReadOnly rc orderLens $
        applyPinaforeFunction pinaforeBase getNames $ langFiniteSetRefFunctionValue fset
    let
        subOpts :: Model (ReadOnlyUpdate (OrderedListUpdate [PickerPairType] (ConstWholeUpdate PickerPairType)))
        subOpts = pinaforeRefOpenModel opts
        subVal :: Model (WholeUpdate PickerType)
        subVal = pinaforeRefOpenModel $ langRefToValue $ contraRangeLift meet2 ref
    optionUISpec subOpts subVal

actionRef ::
       (?pinafore :: PinaforeContext baseupdate)
    => PinaforeImmutableRef (PinaforeAction TopType)
    -> PinaforeROWRef (Maybe (View ()))
actionRef raction =
    eaMapReadOnlyWhole (fmap (\action -> runPinaforeAction (action >> return ())) . knowToMaybe) $
    immutableRefToReadOnlyValue raction

uiButton ::
       forall baseupdate. (?pinafore :: PinaforeContext baseupdate)
    => PinaforeImmutableRef Text
    -> PinaforeImmutableRef (PinaforeAction TopType)
    -> CVUISpec
uiButton text raction =
    buttonUISpec
        (pinaforeRefOpenModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyValue text)
        (pinaforeRefOpenModel $ actionRef raction)

uiLabel :: PinaforeImmutableRef Text -> CVUISpec
uiLabel text =
    labelUISpec $ pinaforeRefOpenModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyValue text

uiDynamic :: PinaforeImmutableRef LangUI -> LangUI
uiDynamic uiref = let
    getSpec :: Know LangUI -> CVUISpec
    getSpec Unknown = nullUISpec
    getSpec (Known pui) = pui
    in switchUISpec $ pinaforeRefOpenModel $ eaMapReadOnlyWhole getSpec $ immutableRefToReadOnlyValue uiref

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
            wsTitle = pinaforeRefOpenModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyValue title
            wsMenuBar :: Maybe (Model (ROWUpdate MenuBar))
            wsMenuBar =
                Just $ pinaforeRefOpenModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyValue mbar
            in MkWindowSpec {..}

uiTextArea :: PinaforeRef (WholeUpdate (Know Text)) -> CVUISpec
uiTextArea val =
    textAreaUISpec (pinaforeRefOpenModel $ eaMap (convertChangeLens . unknownValueChangeLens mempty) val) mempty

uiCalendar :: PinaforeRef (WholeUpdate (Know Day)) -> CVUISpec
uiCalendar day = calendarUISpec $ pinaforeRefOpenModel $ eaMap (unknownValueChangeLens $ fromGregorian 1970 01 01) day

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
    in ActionMenuEntry label maccel $ pinaforeRefOpenModel $ actionRef raction

uiScrolled :: LangUI -> LangUI
uiScrolled = scrolledUISpec

uiUnitCheckBox :: PinaforeImmutableRef Text -> PinaforeRef (WholeUpdate (Know ())) -> CVUISpec
uiUnitCheckBox name val =
    checkboxUISpec (pinaforeRefOpenModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyValue name) $
    pinaforeRefOpenModel $ eaMap (toChangeLens knowBool) val

uiCheckBox :: PinaforeImmutableRef Text -> PinaforeRef (WholeUpdate (Know Bool)) -> CVUISpec
uiCheckBox name val =
    maybeCheckboxUISpec (pinaforeRefOpenModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyValue name) $
    pinaforeRefOpenModel $ eaMap (toChangeLens knowMaybe) val

uiTextEntry :: PinaforeRef (WholeUpdate (Know Text)) -> CVUISpec
uiTextEntry val = textEntryUISpec $ pinaforeRefOpenModel $ eaMap (unknownValueChangeLens mempty) $ val

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
       forall baseupdate. (HasPinaforeEntityUpdate baseupdate, HasPinaforeFileUpdate baseupdate)
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
                "A button with this text that does this action. Button will be disabled if the action reference is unknown." $
            uiButton
          , mkValEntry "uiPick" "A drop-down menu." $ uiPick @baseupdate
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
