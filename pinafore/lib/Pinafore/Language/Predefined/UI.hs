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

uiMap :: (A -> B) -> LangUI A -> LangUI B
uiMap = fmap

uiTable ::
       forall baseupdate.
       ( ?pinafore :: PinaforeContext baseupdate
       , HasPinaforeEntityUpdate baseupdate {-, ApplicableEdit (UpdateEdit baseupdate)-}
       )
    => [(LangRef '( BottomType, Text), A -> LangRef '( BottomType, Text))]
    -> LangOrder baseupdate A
    -> LangFiniteSetRef '( A, EA)
    -> (A -> PinaforeAction TopType)
    -> LangUI A
uiTable cols order val onDoubleClick =
    MkLangUI $ \sn -> do
        let
            uo :: UpdateOrder (ContextUpdate baseupdate (ConstWholeUpdate EA))
            uo =
                mapUpdateOrder
                    (changeLensToFloating $
                     liftContextChangeLens $ fromReadOnlyRejectingChangeLens . funcChangeLens (Known . meet2)) $
                pinaforeUpdateOrder order
            rows :: Model (FiniteSetUpdate EA)
            rows = unPinaforeValue $ unLangFiniteSetRef $ contraRangeLift meet2 val
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
                nameOpenSub = pinaforeValueOpenModel $ eaMapSemiReadOnly clearText $ langRefToReadOnlyValue nameRef
                getCellSub :: Model (ConstWholeUpdate EA) -> CreateView (Model (ROWUpdate (Text, TableCellProps)))
                getCellSub osub = do
                    a <- cvLiftView $ readSub osub
                    return $
                        pinaforeValueOpenModel $
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
    opts :: PinaforeValue (ReadOnlyUpdate (OrderedListUpdate [PickerPairType] (ConstWholeUpdate PickerPairType))) <-
        liftLifeCycleIO $
        eaFloatMapReadOnly rc orderLens $
        applyPinaforeFunction pinaforeBase getNames $ langFiniteSetRefFunctionValue fset
    let
        subOpts :: Model (ReadOnlyUpdate (OrderedListUpdate [PickerPairType] (ConstWholeUpdate PickerPairType)))
        subOpts = pinaforeValueOpenModel opts
        subVal :: Model (WholeUpdate PickerType)
        subVal = pinaforeValueOpenModel $ langRefToValue $ contraRangeLift meet2 ref
    optionUISpec subOpts subVal

actionReference ::
       (?pinafore :: PinaforeContext baseupdate)
    => PinaforeImmutableReference (PinaforeAction TopType)
    -> PinaforeReadOnlyValue (Maybe (View ()))
actionReference raction =
    eaMapReadOnlyWhole (fmap (\action -> runPinaforeAction (action >> return ())) . knowToMaybe) $
    immutableReferenceToReadOnlyValue raction

uiButton ::
       forall baseupdate. (?pinafore :: PinaforeContext baseupdate)
    => PinaforeImmutableReference Text
    -> PinaforeImmutableReference (PinaforeAction TopType)
    -> CVUISpec
uiButton text raction =
    buttonUISpec
        (pinaforeValueOpenModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableReferenceToReadOnlyValue text)
        (pinaforeValueOpenModel $ actionReference raction)

uiLabel :: PinaforeImmutableReference Text -> CVUISpec
uiLabel text =
    labelUISpec $ pinaforeValueOpenModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableReferenceToReadOnlyValue text

uiDynamic :: PinaforeImmutableReference (LangUI A) -> LangUI A
uiDynamic uiref =
    MkLangUI $ \sn -> let
        getSpec :: Know (LangUI A) -> CVUISpec
        getSpec Unknown = nullUISpec
        getSpec (Known (MkLangUI pui)) = pui sn
        in switchUISpec $ pinaforeValueOpenModel $ eaMapReadOnlyWhole getSpec $ immutableReferenceToReadOnlyValue uiref

openWindow ::
       (?pinafore :: PinaforeContext baseupdate)
    => PinaforeImmutableReference Text
    -> (PinaforeAction A -> PinaforeImmutableReference MenuBar)
    -> LangUI A
    -> PinaforeAction PinaforeWindow
openWindow title getmbar (MkLangUI pui) = do
    (sn, getsel) <- liftIO makeRefSelectNotify
    mfix $ \w ->
        pinaforeNewWindow $ let
            wsCloseBoxAction :: View ()
            wsCloseBoxAction = pwClose w
            wsTitle :: Model (ROWUpdate Text)
            wsTitle =
                pinaforeValueOpenModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableReferenceToReadOnlyValue title
            wsMenuBar :: Maybe (Model (ROWUpdate MenuBar))
            wsMenuBar =
                Just $
                pinaforeValueOpenModel $
                eaMapReadOnlyWhole (fromKnow mempty) $
                immutableReferenceToReadOnlyValue $
                getmbar $ do
                    ma <- viewPinaforeAction getsel
                    pinaforeActionKnow $ maybeToKnow ma
            wsContent :: CVUISpec
            wsContent = pui sn
            in MkWindowSpec {..}

uiWithSelection :: (PinaforeAction A -> LangUI A) -> LangUI A
uiWithSelection f =
    MkLangUI $ \seln2 -> do
        (seln1, getsel) <- liftIO makeRefSelectNotify
        let
            pa :: PinaforeAction A
            pa = do
                ma <- viewPinaforeAction getsel
                pinaforeActionKnow $ maybeToKnow ma
        unLangUI (f pa) (seln1 <> seln2)

uiTextArea :: PinaforeValue (WholeUpdate (Know Text)) -> CVUISpec
uiTextArea val =
    textAreaUISpec (pinaforeValueOpenModel $ eaMap (convertChangeLens . unknownValueChangeLens mempty) val) mempty

uiCalendar :: PinaforeValue (WholeUpdate (Know Day)) -> CVUISpec
uiCalendar day = calendarUISpec $ pinaforeValueOpenModel $ eaMap (unknownValueChangeLens $ fromGregorian 1970 01 01) day

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
    -> PinaforeImmutableReference (PinaforeAction TopType)
    -> MenuEntry
menuAction label maccelStr raction = let
    maccel = do
        accelStr <- maccelStr
        interpretAccelerator $ unpack accelStr
    in ActionMenuEntry label maccel $ pinaforeValueOpenModel $ actionReference raction

uiScrolled :: LangUI A -> LangUI A
uiScrolled (MkLangUI lspec) = MkLangUI $ \sn -> scrolledUISpec $ lspec sn

uiUnitCheckBox :: PinaforeImmutableReference Text -> PinaforeValue (WholeUpdate (Know ())) -> CVUISpec
uiUnitCheckBox name val =
    checkboxUISpec
        (pinaforeValueOpenModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableReferenceToReadOnlyValue name) $
    pinaforeValueOpenModel $ eaMap (toChangeLens knowBool) val

uiCheckBox :: PinaforeImmutableReference Text -> PinaforeValue (WholeUpdate (Know Bool)) -> CVUISpec
uiCheckBox name val =
    maybeCheckboxUISpec
        (pinaforeValueOpenModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableReferenceToReadOnlyValue name) $
    pinaforeValueOpenModel $ eaMap (toChangeLens knowMaybe) val

uiTextEntry :: PinaforeValue (WholeUpdate (Know Text)) -> CVUISpec
uiTextEntry val = textEntryUISpec $ pinaforeValueOpenModel $ eaMap (unknownValueChangeLens mempty) $ val

uiIgnore :: LangUI TopType -> LangUI BottomType
uiIgnore (MkLangUI lspec) = MkLangUI $ \_ -> lspec mempty

uiHorizontal :: [(LangUI A, Bool)] -> LangUI A
uiHorizontal items = MkLangUI $ \sn -> horizontalUISpec $ fmap (\(MkLangUI pui, e) -> (pui sn, e)) items

uiVertical :: [(LangUI A, Bool)] -> LangUI A
uiVertical items = MkLangUI $ \sn -> verticalUISpec $ fmap (\(MkLangUI pui, e) -> (pui sn, e)) items

uiPages :: [(LangUI TopType, LangUI A)] -> LangUI A
uiPages items = MkLangUI $ \sn -> pagesUISpec $ fmap (\(MkLangUI lpui, MkLangUI ppui) -> (lpui mempty, ppui sn)) items

ui_predefinitions ::
       forall baseupdate. (HasPinaforeEntityUpdate baseupdate, HasPinaforeFileUpdate baseupdate)
    => [DocTreeEntry (BindDoc baseupdate)]
ui_predefinitions =
    [ docTreeEntry
          "UI"
          "A user interface is something that goes inside a window."
          [ mkValEntry "uiWithSelection" "User interface with selection." uiWithSelection
          , mkValEntry "uiMap" "Map user interface selection" uiMap
          , mkValEntry "uiIgnore" "Ignore user interface selection" uiIgnore
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
