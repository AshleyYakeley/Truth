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

clearText :: UpdateFunction (WholeUpdate (Know Text)) (WholeUpdate Text)
clearText = funcUpdateFunction (fromKnow mempty)

uiMap :: (A -> B) -> PinaforeUI A -> PinaforeUI B
uiMap = fmap

uiTable ::
       forall baseupdate. (?pinafore :: PinaforeContext baseupdate, HasPinaforeEntityUpdate baseupdate)
    => [(PinaforeRef '( BottomType, Text), A -> PinaforeRef '( BottomType, Text))]
    -> PinaforeOrder baseupdate A
    -> PinaforeFiniteSetRef '( A, MeetType Entity A)
    -> (A -> PinaforeAction TopType)
    -> UISpec A
uiTable cols (MkPinaforeOrder geto order) val onDoubleClick = let
    showCell :: Know Text -> (Text, TableCellProps)
    showCell (Known s) = (s, plainTableCellProps)
    showCell Unknown = ("unknown", plainTableCellProps {tcStyle = plainTextStyle {tsItalic = True}})
    mapLens :: PinaforeReadOnlyValue (Know Text) -> PinaforeReadOnlyValue (Text, TableCellProps)
    mapLens ff = eaMapReadOnlyWhole showCell ff
    getColumn ::
           (PinaforeRef '( BottomType, Text), A -> PinaforeRef '( BottomType, Text)) -> KeyColumn (MeetType Entity A)
    getColumn (name, f) =
        readOnlyKeyColumn (unPinaforeValue $ eaMapReadOnly clearText $ pinaforeRefToReadOnlyValue name) $ \p ->
            return $ unPinaforeValue $ mapLens $ pinaforeRefToReadOnlyValue $ f $ meet2 p
    in mapSelectionUISpec meet2 $
       tableUISpec
           (fmap getColumn cols)
           order
           (\mea -> unPinaforeValue $ applyPinaforeFunction pinaforeBase geto $ eaPure $ Known $ meet2 mea)
           (unPinaforeValue $ unPinaforeFiniteSetRef $ contraRangeLift meet2 val)
           (\a -> runPinaforeAction $ void $ onDoubleClick $ meet2 a)

type PickerType = Know (MeetType Entity A)

type PickerPairType = (PickerType, OptionUICell)

makeCell :: Know Text -> OptionUICell
makeCell Unknown = (plainOptionUICell "unknown") {optionCellStyle = plainTextStyle {tsItalic = True}}
makeCell (Known t) = plainOptionUICell t

uiPick ::
       forall baseupdate. (?pinafore :: PinaforeContext baseupdate)
    => PinaforeMorphism baseupdate '( A, TopType) '( BottomType, Text)
    -> PinaforeFiniteSetRef '( A, MeetType Entity A)
    -> PinaforeRef '( A, MeetType Entity A)
    -> UISpec BottomType
uiPick nameMorphism fset ref = let
    getName :: PinaforeFunctionMorphism baseupdate (MeetType Entity A) PickerPairType
    getName =
        proc p -> do
            n <- pinaforeMorphismFunction nameMorphism -< Known $ meet2 p
            returnA -< (Known p, makeCell n)
    getNames :: PinaforeFunctionMorphism baseupdate (FiniteSet (MeetType Entity A)) (FiniteSet PickerPairType)
    getNames =
        proc fsp -> do
            pairs <- cfmap getName -< fsp
            returnA -< insertSet (Unknown, makeCell Unknown) pairs
    ufOrder ::
           UpdateFunction (WholeUpdate (FiniteSet PickerPairType)) (ListUpdate [PickerPairType] (WholeUpdate PickerPairType))
    ufOrder = (orderedKeyList @(FiniteSet PickerPairType) $ comparing $ optionCellText . snd) . convertUpdateFunction
    opts :: ReadOnlySubscriber (ListUpdate [PickerPairType] (WholeUpdate PickerPairType))
    opts =
        unPinaforeValue $
        eaMapReadOnly ufOrder $ applyPinaforeFunction pinaforeBase getNames $ pinaforeFiniteSetRefFunctionValue fset
    in optionUISpec @PickerType opts $ unPinaforeValue $ pinaforeRefToValue $ contraRangeLift meet2 ref

actionReference ::
       (?pinafore :: PinaforeContext baseupdate)
    => PinaforeImmutableReference (PinaforeAction TopType)
    -> PinaforeReadOnlyValue (Maybe (IO ()))
actionReference raction =
    eaMapReadOnlyWhole (fmap (\action -> runPinaforeAction (action >> return ())) . knowToMaybe) $
    immutableReferenceToReadOnlyValue raction

uiButton ::
       forall baseupdate. (?pinafore :: PinaforeContext baseupdate)
    => PinaforeImmutableReference Text
    -> PinaforeImmutableReference (PinaforeAction TopType)
    -> UISpec BottomType
uiButton text raction =
    buttonUISpec
        (unPinaforeValue $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableReferenceToReadOnlyValue text)
        (unPinaforeValue $ actionReference raction)

uiLabel :: PinaforeImmutableReference Text -> UISpec BottomType
uiLabel text =
    labelUISpec $ unPinaforeValue $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableReferenceToReadOnlyValue text

uiDynamic :: PinaforeImmutableReference (PinaforeUI A) -> UISpec A
uiDynamic uiref = let
    getSpec :: Know (PinaforeUI A) -> UISpec A
    getSpec Unknown = nullUISpec
    getSpec (Known (MkPinaforeUI ui)) = ui
    in switchUISpec $ unPinaforeValue $ eaMapReadOnlyWhole getSpec $ immutableReferenceToReadOnlyValue uiref

aspectToAction :: Aspect a -> PinaforeAction a
aspectToAction aspect = do
    ma <- pinaforeLiftLifeCycleIO aspect
    pinaforeActionKnow $ maybeToKnow ma

openWindow ::
       (?pinafore :: PinaforeContext baseupdate)
    => PinaforeImmutableReference Text
    -> (PinaforeAction A -> PinaforeImmutableReference MenuBar)
    -> UISpec A
    -> PinaforeAction PinaforeWindow
openWindow title getmbar wsContent =
    mfix $ \w ->
        pinaforeNewWindow $ let
            wsCloseBoxAction :: IO ()
            wsCloseBoxAction = pwClose w
            wsTitle :: ReadOnlySubscriber (WholeUpdate Text)
            wsTitle = unPinaforeValue $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableReferenceToReadOnlyValue title
            wsMenuBar :: Maybe (Aspect A -> ReadOnlySubscriber (WholeUpdate MenuBar))
            wsMenuBar =
                Just $ \aspect ->
                    unPinaforeValue $
                    eaMapReadOnlyWhole (fromKnow mempty) $
                    immutableReferenceToReadOnlyValue $ getmbar $ aspectToAction aspect
            in MkWindowSpec {..}

uiWithSelection :: (PinaforeAction A -> UISpec A) -> UISpec A
uiWithSelection f = withAspectUISpec $ \aspect -> f $ aspectToAction aspect

uiTextArea :: PinaforeValue (WholeUpdate (Know Text)) -> UISpec BottomType
uiTextArea val =
    noSelectionUISpec $ textAreaUISpec $ unPinaforeValue $ eaMap (convertEditLens . unknownValueEditLens mempty) val

uiCalendar :: PinaforeValue (WholeUpdate (Know Day)) -> UISpec BottomType
uiCalendar day = calendarUISpec $ unPinaforeValue $ eaMap (unknownValueEditLens $ fromGregorian 1970 01 01) day

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
    in ActionMenuEntry label maccel $ unPinaforeValue $ actionReference raction

uiScrolled :: PinaforeUI A -> PinaforeUI A
uiScrolled (MkPinaforeUI ui) = MkPinaforeUI $ scrolledUISpec ui

uiUnitCheckBox :: PinaforeImmutableReference Text -> PinaforeValue (WholeUpdate (Know ())) -> UISpec BottomType
uiUnitCheckBox name val =
    checkboxUISpec (unPinaforeValue $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableReferenceToReadOnlyValue name) $
    unPinaforeValue $ eaMap (toEditLens knowBool) val

uiCheckBox :: PinaforeImmutableReference Text -> PinaforeValue (WholeUpdate (Know Bool)) -> UISpec BottomType
uiCheckBox name val =
    maybeCheckboxUISpec
        (unPinaforeValue $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableReferenceToReadOnlyValue name) $
    unPinaforeValue $ eaMap (toEditLens knowMaybe) val

uiTextEntry :: PinaforeValue (WholeUpdate (Know Text)) -> UISpec BottomType
uiTextEntry val = textEntryUISpec $ unPinaforeValue $ eaMap (unknownValueEditLens mempty) $ val

ui_predefinitions ::
       forall baseupdate. (HasPinaforeEntityUpdate baseupdate, HasPinaforeFileUpdate baseupdate)
    => [DocTreeEntry (BindDoc baseupdate)]
ui_predefinitions =
    [ docTreeEntry
          "UI"
          "A user interface is something that goes inside a window."
          [ mkValEntry "uiWithSelection" "User interface with selection." uiWithSelection
          , mkValEntry "uiMap" "Map user interface selection" uiMap
          , mkValEntry "uiIgnore" "Ignore user interface selection" $ noSelectionUISpec @TopType @BottomType
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
