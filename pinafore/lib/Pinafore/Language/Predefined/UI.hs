module Pinafore.Language.Predefined.UI
    ( ui_predefinitions
    ) where

import Data.Shim
import Data.Time
import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.Predefined.Defs
import Pinafore.Language.Value
import Shapes
import Truth.Core

clearText :: EditLens (WholeUpdate (Know Text)) (ROWUpdate Text)
clearText = funcEditLens (fromKnow mempty)

uiMap :: (A -> B) -> PinaforeUI A -> PinaforeUI B
uiMap = fmap

uiTable ::
       (?pinafore :: PinaforeContext)
    => [(PinaforeRef '( BottomType, Text), A -> PinaforeRef '( BottomType, Text))]
    -> PinaforeOrder A
    -> PinaforeFiniteSetRef '( A, EA)
    -> (A -> PinaforeAction TopType)
    -> LUISpec A
uiTable cols order val onDoubleClick = do
    let
        uo :: UpdateOrder (ContextUpdate PinaforeEntityUpdate (ConstWholeUpdate EA))
        uo =
            mapUpdateOrder
                (editLensToFloating $ liftContextEditLens $ fromReadOnlyRejectingEditLens . funcEditLens (Known . meet2)) $
            pinaforeUpdateOrder order
        rows :: Subscriber (FiniteSetUpdate EA)
        rows = unPinaforeValue $ unPinaforeFiniteSetRef $ contraRangeLift meet2 val
        pkSub :: Subscriber (ContextUpdate PinaforeEntityUpdate (FiniteSetUpdate EA))
        pkSub = contextSubscribers pinaforeSubEntity rows
        readOpenSub :: OpenSubscriber (ConstWholeUpdate EA) -> IO A
        readOpenSub sub =
            withOpenResource sub $ \asub -> do
                ea <- subRead asub ReadWhole
                return $ meet2 ea
        onSelect :: OpenSubscriber (ConstWholeUpdate EA) -> IO ()
        onSelect osub = do
            a <- readOpenSub osub
            runPinaforeAction $ void $ onDoubleClick a
        getColumn ::
               (PinaforeRef '( BottomType, Text), A -> PinaforeRef '( BottomType, Text))
            -> KeyColumn (ConstWholeUpdate EA)
        getColumn (nameRef, getCellRef) = let
            showCell :: Know Text -> (Text, TableCellProps)
            showCell (Known s) = (s, plainTableCellProps)
            showCell Unknown = ("unknown", plainTableCellProps {tcStyle = plainTextStyle {tsItalic = True}})
            nameOpenSub :: OpenSubscriber (ROWUpdate Text)
            nameOpenSub = pinaforeValueOpenSubscriber $ eaMapSemiReadOnly clearText $ pinaforeRefToReadOnlyValue nameRef
            getCellSub :: OpenSubscriber (ConstWholeUpdate EA) -> IO (OpenSubscriber (ROWUpdate (Text, TableCellProps)))
            getCellSub osub = do
                a <- readOpenSub osub
                return $
                    pinaforeValueOpenSubscriber $
                    eaMapSemiReadOnly (funcEditLens showCell) $ pinaforeRefToReadOnlyValue $ getCellRef a
            in readOnlyKeyColumn nameOpenSub getCellSub
    colSub :: Subscriber (ContextUpdate PinaforeEntityUpdate (OrderedListUpdate [EA] (ConstWholeUpdate EA))) <-
        floatMapSubscriber (contextOrderedSetLens uo) pkSub
    let
        olsub :: Subscriber (OrderedListUpdate [EA] (ConstWholeUpdate EA))
        olsub = mapSubscriber (tupleEditLens SelectContent) colSub
    mapSelectionUISpec (liftIO . readOpenSub . openResource) $
        tableUISpec (fmap getColumn cols) (openResource olsub) onSelect

type PickerType = Know EA

type PickerPairType = (PickerType, OptionUICell)

makeCell :: Know Text -> OptionUICell
makeCell Unknown = (plainOptionUICell "unknown") {optionCellStyle = plainTextStyle {tsItalic = True}}
makeCell (Known t) = plainOptionUICell t

uiPick ::
       (?pinafore :: PinaforeContext)
    => PinaforeMorphism '( A, TopType) '( BottomType, Text)
    -> PinaforeFiniteSetRef '( A, EA)
    -> PinaforeRef '( A, EA)
    -> LUISpec BottomType
uiPick nameMorphism fset ref = do
    let
        getName :: PinaforeFunctionMorphism PinaforeEntityUpdate EA PickerPairType
        getName =
            proc p -> do
                n <- pinaforeMorphismFunction nameMorphism -< Known $ meet2 p
                returnA -< (Known p, makeCell n)
        getNames :: PinaforeFunctionMorphism PinaforeEntityUpdate (FiniteSet EA) (FiniteSet PickerPairType)
        getNames =
            proc fsp -> do
                pairs <- cfmap getName -< fsp
                returnA -< insertSet (Unknown, makeCell Unknown) pairs
        updateOrder :: UpdateOrder (ConstWholeUpdate PickerPairType)
        updateOrder = MkUpdateOrder (comparing $ optionCellText . snd) $ editLensToFloating convertReadOnlyEditLens
        orderLens ::
               FloatingEditLens (WholeUpdate (FiniteSet PickerPairType)) (ReadOnlyUpdate (OrderedListUpdate [PickerPairType] (ConstWholeUpdate PickerPairType)))
        --orderLens = (orderedKeyList {- @(FiniteSet PickerPairType) -} $ comparing $ optionCellText . snd) . convertEditLens
        orderLens =
            editLensToFloating toReadOnlyEditLens . orderedSetLens updateOrder . editLensToFloating convertEditLens
    opts :: PinaforeValue (ReadOnlyUpdate (OrderedListUpdate [PickerPairType] (ConstWholeUpdate PickerPairType))) <-
        eaFloatMapReadOnly orderLens $
        applyPinaforeFunction pinaforeSubEntity getNames $ pinaforeFiniteSetRefFunctionValue fset
    let
        subOpts ::
               OpenSubscriber (ReadOnlyUpdate (OrderedListUpdate [PickerPairType] (ConstWholeUpdate PickerPairType)))
        subOpts = pinaforeValueOpenSubscriber opts
        subVal :: OpenSubscriber (WholeUpdate PickerType)
        subVal = pinaforeValueOpenSubscriber $ pinaforeRefToValue $ contraRangeLift meet2 ref
    optionUISpec subOpts subVal

actionReference ::
       (?pinafore :: PinaforeContext)
    => PinaforeImmutableReference (PinaforeAction TopType)
    -> PinaforeReadOnlyValue (Maybe (IO ()))
actionReference raction =
    eaMapReadOnlyWhole (fmap (\action -> runPinaforeAction (action >> return ())) . knowToMaybe) $
    immutableReferenceToReadOnlyValue raction

uiButton ::
       (?pinafore :: PinaforeContext)
    => PinaforeImmutableReference Text
    -> PinaforeImmutableReference (PinaforeAction TopType)
    -> LUISpec BottomType
uiButton text raction =
    buttonUISpec
        (pinaforeValueOpenSubscriber $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableReferenceToReadOnlyValue text)
        (pinaforeValueOpenSubscriber $ actionReference raction)

uiLabel :: PinaforeImmutableReference Text -> LUISpec BottomType
uiLabel text =
    labelUISpec $
    pinaforeValueOpenSubscriber $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableReferenceToReadOnlyValue text

uiDynamic :: PinaforeImmutableReference (PinaforeUI A) -> LUISpec A
uiDynamic uiref = let
    getSpec :: Know (PinaforeUI A) -> LUISpec A
    getSpec Unknown = nullUISpec
    getSpec (Known (MkPinaforeUI ui)) = ui
    in switchUISpec $ pinaforeValueOpenSubscriber $ eaMapReadOnlyWhole getSpec $ immutableReferenceToReadOnlyValue uiref

aspectToAction :: Aspect a -> PinaforeAction a
aspectToAction aspect = do
    ma <- pinaforeLiftLifeCycleIO aspect
    pinaforeActionKnow $ maybeToKnow ma

openWindow ::
       (?pinafore :: PinaforeContext)
    => PinaforeImmutableReference Text
    -> (PinaforeAction A -> PinaforeImmutableReference MenuBar)
    -> LUISpec A
    -> PinaforeAction PinaforeWindow
openWindow title getmbar wsContent =
    mfix $ \w ->
        pinaforeNewWindow $ let
            wsCloseBoxAction :: IO ()
            wsCloseBoxAction = pwClose w
            wsTitle :: OpenSubscriber (ROWUpdate Text)
            wsTitle =
                pinaforeValueOpenSubscriber $
                eaMapReadOnlyWhole (fromKnow mempty) $ immutableReferenceToReadOnlyValue title
            wsMenuBar :: Maybe (Aspect A -> OpenSubscriber (ROWUpdate MenuBar))
            wsMenuBar =
                Just $ \aspect ->
                    pinaforeValueOpenSubscriber $
                    eaMapReadOnlyWhole (fromKnow mempty) $
                    immutableReferenceToReadOnlyValue $ getmbar $ aspectToAction aspect
            in MkWindowSpec {..}

uiWithSelection :: (PinaforeAction A -> LUISpec A) -> LUISpec A
uiWithSelection f = withAspectUISpec $ \aspect -> f $ aspectToAction aspect

uiTextArea :: PinaforeValue (WholeUpdate (Know Text)) -> LUISpec BottomType
uiTextArea val =
    noSelectionUISpec $
    textAreaUISpec $ pinaforeValueOpenSubscriber $ eaMap (convertEditLens . unknownValueEditLens mempty) val

uiCalendar :: PinaforeValue (WholeUpdate (Know Day)) -> LUISpec BottomType
uiCalendar day =
    calendarUISpec $ pinaforeValueOpenSubscriber $ eaMap (unknownValueEditLens $ fromGregorian 1970 01 01) day

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
    -> PinaforeImmutableReference (PinaforeAction TopType)
    -> MenuEntry
menuAction label maccelStr raction = let
    maccel = do
        accelStr <- maccelStr
        interpretAccelerator $ unpack accelStr
    in ActionMenuEntry label maccel $ pinaforeValueOpenSubscriber $ actionReference raction

uiScrolled :: PinaforeUI A -> PinaforeUI A
uiScrolled (MkPinaforeUI lspec) = MkPinaforeUI $ scrolledUISpec lspec

uiUnitCheckBox :: PinaforeImmutableReference Text -> PinaforeValue (WholeUpdate (Know ())) -> LUISpec BottomType
uiUnitCheckBox name val =
    checkboxUISpec
        (pinaforeValueOpenSubscriber $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableReferenceToReadOnlyValue name) $
    pinaforeValueOpenSubscriber $ eaMap (toEditLens knowBool) val

uiCheckBox :: PinaforeImmutableReference Text -> PinaforeValue (WholeUpdate (Know Bool)) -> LUISpec BottomType
uiCheckBox name val =
    maybeCheckboxUISpec
        (pinaforeValueOpenSubscriber $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableReferenceToReadOnlyValue name) $
    pinaforeValueOpenSubscriber $ eaMap (toEditLens knowMaybe) val

uiTextEntry :: PinaforeValue (WholeUpdate (Know Text)) -> LUISpec BottomType
uiTextEntry val = textEntryUISpec $ pinaforeValueOpenSubscriber $ eaMap (unknownValueEditLens mempty) $ val

uiIgnore :: PinaforeUI TopType -> PinaforeUI BottomType
uiIgnore (MkPinaforeUI lspec) = MkPinaforeUI $ noSelectionUISpec lspec

ui_predefinitions :: forall baseupdate. [DocTreeEntry (BindDoc baseupdate)]
ui_predefinitions =
    [ docTreeEntry
          "UI"
          "A user interface is something that goes inside a window."
          [ mkValEntry "uiWithSelection" "User interface with selection." uiWithSelection
          , mkValEntry "uiMap" "Map user interface selection" uiMap
          , mkValEntry "uiIgnore" "Ignore user interface selection" uiIgnore
          , mkValEntry "uiBlank" "Blank user-interface" $ nullUISpec @BottomType
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
                "Items arranged horizontally, each flag is whether to expand into remaining space." $
            horizontalUISpec @A
          , mkValEntry "uiVertical" "Items arranged vertically, each flag is whether to expand into remaining space." $
            verticalUISpec @A
          , mkValEntry
                "uiPages"
                "A notebook of pages. First of each pair is for the page tab (typically a label), second is the content." $
            pagesUISpec @A @TopType
                -- CSS
                -- drag
                -- icon
          , mkValEntry
                "uiButton"
                "A button with this text that does this action. Button will be disabled if the action reference is unknown." $
            uiButton
          , mkValEntry "uiPick" "A drop-down menu." $ uiPick
          , mkValEntry
                "uiTable"
                "A list table. First arg is columns (name, property), second is order, third is the set of items, fourth is the window to open for a selection." $
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
          , mkValEntry "menuAction" "Action menu item. Item will be disabled if the action reference is unknown." $
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
