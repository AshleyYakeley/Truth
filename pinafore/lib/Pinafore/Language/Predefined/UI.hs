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

valSpecText ::
       UISpec sel (WholeUpdate (Know Text))
    -> PinaforeLensValue baseupdate (WholeUpdate (Know Text))
    -> UISpec sel baseupdate
valSpecText spec val = mapUpdateUISpec (return val) spec

clearText :: UpdateFunction (WholeUpdate (Know Text)) (WholeUpdate Text)
clearText = funcUpdateFunction (fromKnow mempty)

uiMap :: forall baseupdate. (A -> B) -> PinaforeUI baseupdate A -> PinaforeUI baseupdate B
uiMap = fmap

uiTable ::
       forall baseupdate. (?pinafore :: PinaforeContext baseupdate, HasPinaforeEntityUpdate baseupdate)
    => [(PinaforeRef baseupdate '( BottomType, Text), A -> PinaforeRef baseupdate '( BottomType, Text))]
    -> PinaforeOrder baseupdate A
    -> PinaforeFiniteSetRef baseupdate '( A, MeetType Entity A)
    -> (A -> PinaforeAction baseupdate TopType)
    -> UISpec A baseupdate
uiTable cols (MkPinaforeOrder geto order) val onDoubleClick = let
    showCell :: Know Text -> (Text, TableCellProps)
    showCell (Known s) = (s, plainTableCellProps)
    showCell Unknown = ("unknown", plainTableCellProps {tcStyle = plainTextStyle {tsItalic = True}})
    mapLens :: PinaforeFunctionValue baseupdate (Know Text) -> PinaforeFunctionValue baseupdate (Text, TableCellProps)
    mapLens ff = funcUpdateFunction showCell . ff
    getColumn ::
           (PinaforeRef baseupdate '( BottomType, Text), A -> PinaforeRef baseupdate '( BottomType, Text))
        -> KeyColumn baseupdate (MeetType Entity A)
    getColumn (name, f) =
        readOnlyKeyColumn (clearText . pinaforeRefToFunction name) $ \p ->
            return $ mapLens $ pinaforeRefToFunction $ f $ meet2 p
    in mapSelectionUISpec meet2 $
       tableUISpec
           (fmap getColumn cols)
           order
           (\mea -> applyPinaforeFunction geto $ constUpdateFunction $ Known $ meet2 mea)
           (unPinaforeFiniteSetRef $ contraRangeLift meet2 val)
           (\a -> runPinaforeAction $ void $ onDoubleClick $ meet2 a)

type PickerType = Know (MeetType Entity A)

type PickerPairType = (PickerType, OptionUICell)

makeCell :: Know Text -> OptionUICell
makeCell Unknown = (plainOptionUICell "unknown") {optionCellStyle = plainTextStyle {tsItalic = True}}
makeCell (Known t) = plainOptionUICell t

uiPick ::
       forall baseupdate.
       PinaforeMorphism baseupdate '( A, TopType) '( BottomType, Text)
    -> PinaforeFiniteSetRef baseupdate '( A, MeetType Entity A)
    -> PinaforeRef baseupdate '( A, MeetType Entity A)
    -> UISpec BottomType baseupdate
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
    opts :: UpdateFunction baseupdate (ListUpdate [PickerPairType] (WholeUpdate PickerPairType))
    opts =
        (orderedKeyList @(FiniteSet PickerPairType) $ comparing $ optionCellText . snd) .
        convertUpdateFunction . applyPinaforeFunction getNames (pinaforeFiniteSetRefFunctionValue fset)
    in optionUISpec @baseupdate @PickerType opts $ pinaforeRefToLens $ contraRangeLift meet2 ref

actionReference ::
       (?pinafore :: PinaforeContext baseupdate)
    => PinaforeImmutableReference baseupdate (PinaforeAction baseupdate TopType)
    -> UpdateFunction baseupdate (WholeUpdate (Maybe (IO ())))
actionReference raction =
    funcUpdateFunction (fmap (\action -> runPinaforeAction (action >> return ())) . knowToMaybe) .
    immutableReferenceToFunction raction

uiButton ::
       (?pinafore :: PinaforeContext baseupdate)
    => PinaforeImmutableReference baseupdate Text
    -> PinaforeImmutableReference baseupdate (PinaforeAction baseupdate TopType)
    -> UISpec BottomType baseupdate
uiButton text raction = buttonUISpec (clearText . immutableReferenceToFunction text) $ actionReference raction

uiLabel :: forall baseupdate. PinaforeImmutableReference baseupdate Text -> UISpec BottomType baseupdate
uiLabel text = mapUpdateUISpec (return $ immutableReferenceToLens text) $ uiUnknownValue mempty $ labelUISpec

uiDynamic :: forall baseupdate. PinaforeImmutableReference baseupdate (UISpec A baseupdate) -> UISpec A baseupdate
uiDynamic uiref = switchUISpec $ pinaforeImmutableReferenceValue nullUISpec uiref

aspectToAction :: Aspect a -> PinaforeAction baseupdate a
aspectToAction aspect = do
    ma <- pinaforeLiftLifeCycleIO aspect
    pinaforeActionKnow $ maybeToKnow ma

openWindow ::
       forall baseupdate. (?pinafore :: PinaforeContext baseupdate)
    => PinaforeImmutableReference baseupdate Text
    -> (PinaforeAction baseupdate A -> PinaforeImmutableReference baseupdate (MenuBar baseupdate))
    -> UISpec A baseupdate
    -> PinaforeAction baseupdate PinaforeWindow
openWindow title getmbar wsContent =
    mfix $ \w -> let
        wsCloseBoxAction = pwClose w
        wsTitle = clearText . immutableReferenceToFunction title
        wsMenuBar :: Maybe (Aspect A -> UpdateFunction baseupdate (WholeUpdate (MenuBar baseupdate)))
        wsMenuBar =
            Just $ \aspect ->
                funcUpdateFunction (fromKnow mempty) . immutableReferenceToFunction (getmbar $ aspectToAction aspect)
        in pinaforeNewWindow MkWindowSpec {..}

uiWithSelection :: (PinaforeAction baseupdate A -> UISpec A baseupdate) -> UISpec A baseupdate
uiWithSelection f = withAspectUISpec $ \aspect -> f $ aspectToAction aspect

uiTextArea :: forall baseupdate. PinaforeLensValue baseupdate (WholeUpdate (Know Text)) -> UISpec BottomType baseupdate
uiTextArea = valSpecText $ uiUnknownValue mempty $ noSelectionUISpec $ convertEditUISpec textAreaUISpec

uiCalendar :: forall baseupdate. PinaforeLensValue baseupdate (WholeUpdate (Know Day)) -> UISpec BottomType baseupdate
uiCalendar day = mapUpdateUISpec (return day) $ uiUnknownValue (fromGregorian 1970 01 01) calendarUISpec

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
    -> PinaforeImmutableReference baseupdate (PinaforeAction baseupdate TopType)
    -> MenuEntry baseupdate
menuAction label maccel raction =
    ActionMenuEntry
        label
        (do
             accel <- maccel
             interpretAccelerator $ unpack accel) $
    actionReference raction

uiScrolled :: forall baseupdate. UISpec A baseupdate -> UISpec A baseupdate
uiScrolled = scrolledUISpec

ui_predefinitions ::
       forall baseupdate. (HasPinaforeEntityUpdate baseupdate, HasPinaforeFileUpdate baseupdate)
    => [DocTreeEntry (BindDoc baseupdate)]
ui_predefinitions =
    [ docTreeEntry
          "UI"
          "A user interface is something that goes inside a window."
          [ mkValEntry "uiWithSelection" "User interface with selection." $ uiWithSelection @baseupdate
          , mkValEntry "uiMap" "Map user interface selection" $ uiMap @baseupdate
          , mkValEntry "uiIgnore" "Ignore user interface selection" $ noSelectionUISpec @baseupdate @TopType @BottomType
          , mkValEntry "uiBlank" "Blank user-interface" $ nullUISpec @baseupdate @BottomType
          , mkValEntry "uiUnitCheckBox" "(TBD)" $ \name val ->
                checkboxUISpec @baseupdate @BottomType (clearText . name) $ toEditLens knowBool . val
          , mkValEntry "uiCheckBox" "Checkbox. Use shift-click to set to unknown." $ \name val ->
                maybeCheckboxUISpec @baseupdate @BottomType (clearText . name) $
                (bijectionWholeEditLens knowMaybe) . val
          , mkValEntry
                "uiTextEntry"
                "Text entry, unknown reference will be interpreted as empty text, but the UI will not delete the reference." $
            valSpecText $ uiUnknownValue mempty $ textAreaUISpecEntry @BottomType
          , mkValEntry
                "uiTextArea"
                "Text area, unknown reference will be interpreted as empty text, but the UI will not delete the reference." $
            uiTextArea @baseupdate
          , mkValEntry "uiLabel" "Label." $ uiLabel @baseupdate
          , mkValEntry
                "uiHorizontal"
                "Items arranged horizontally, each flag is whether to expand into remaining space." $
            horizontalUISpec @baseupdate @A
          , mkValEntry "uiVertical" "Items arranged vertically, each flag is whether to expand into remaining space." $
            verticalUISpec @baseupdate @A
          , mkValEntry
                "uiPages"
                "A notebook of pages. First of each pair is for the page tab (typically a label), second is the content." $
            pagesUISpec @baseupdate @A @TopType
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
          , mkValEntry "uiCalendar" "A calendar." $ uiCalendar @baseupdate
          , mkValEntry "uiScrolled" "A scrollable container." $ uiScrolled @baseupdate
          , mkValEntry "uiDynamic" "A UI that can be updated to different UIs." $ uiDynamic @baseupdate
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
