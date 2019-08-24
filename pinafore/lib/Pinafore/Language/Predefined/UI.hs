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
       UISpec sel (WholeEdit (Know Text)) -> PinaforeLensValue baseedit (WholeEdit (Know Text)) -> UISpec sel baseedit
valSpecText spec val = mapEditUISpec val spec

clearText :: EditFunction (WholeEdit (Know Text)) (WholeEdit Text)
clearText = funcEditFunction (fromKnow mempty)

uiMap :: forall baseedit. (A -> B) -> PinaforeUI baseedit A -> PinaforeUI baseedit B
uiMap = fmap

uiTable ::
       forall baseedit. (?pinafore :: PinaforeContext baseedit, HasPinaforeEntityEdit baseedit)
    => [(PinaforeRef baseedit '( BottomType, Text), A -> PinaforeRef baseedit '( BottomType, Text))]
    -> PinaforeOrder baseedit A
    -> PinaforeSetRef baseedit '( A, MeetType Entity A)
    -> (A -> PinaforeAction baseedit TopType)
    -> UISpec A baseedit
uiTable cols (MkPinaforeOrder geto order) val onDoubleClick = let
    showCell :: Know Text -> (Text, TableCellProps)
    showCell (Known s) = (s, tableCellPlain)
    showCell Unknown = ("unknown", tableCellPlain {tcItalic = True})
    mapLens :: PinaforeFunctionValue baseedit (Know Text) -> PinaforeFunctionValue baseedit (Text, TableCellProps)
    mapLens ff = funcEditFunction showCell . ff
    getColumn ::
           (PinaforeRef baseedit '( BottomType, Text), A -> PinaforeRef baseedit '( BottomType, Text))
        -> KeyColumn baseedit (MeetType Entity A)
    getColumn (name, f) =
        readOnlyKeyColumn (clearText . pinaforeRefToFunction name) $ \p ->
            return $ mapLens $ pinaforeRefToFunction $ f $ meet2 p
    in mapSelectionUISpec meet2 $
       tableUISpec
           (fmap getColumn cols)
           order
           (\mea -> applyPinaforeFunction geto $ constEditFunction $ Known $ meet2 mea)
           (unPinaforeSetRef $ contraRangeLift meet2 val)
           (\a -> runPinaforeAction $ void $ onDoubleClick $ meet2 a)

type PickerType = Know (MeetType Entity A)

type PickerPairType = (PickerType, Text)

uiPick ::
       forall baseedit.
       PinaforeMorphism baseedit '( A, TopType) '( BottomType, Text)
    -> PinaforeSetRef baseedit '( A, MeetType Entity A)
    -> PinaforeRef baseedit '( A, MeetType Entity A)
    -> UISpec BottomType baseedit
uiPick nameMorphism fset ref = let
    getName :: PinaforeFunctionMorphism baseedit (MeetType Entity A) PickerPairType
    getName =
        proc p -> do
            n <- pinaforeMorphismFunction nameMorphism -< Known $ meet2 p
            returnA -< (Known p, fromKnow "" n)
    getNames :: PinaforeFunctionMorphism baseedit (FiniteSet (MeetType Entity A)) (FiniteSet PickerPairType)
    getNames =
        proc fsp -> do
            pairs <- cfmap getName -< fsp
            returnA -< insertSet (Unknown, "") pairs
    opts :: EditFunction baseedit (ListEdit [PickerPairType] (WholeEdit PickerPairType))
    opts =
        (orderedKeyList @(FiniteSet PickerPairType) $ \(_, a) (_, b) -> compare a b) .
        convertEditFunction . applyPinaforeFunction getNames (pinaforeSetRefFunctionValue fset)
    in optionUISpec @baseedit @PickerType opts $ pinaforeRefToLens $ contraRangeLift meet2 ref

actionReference ::
       (?pinafore :: PinaforeContext baseedit)
    => PinaforeImmutableReference baseedit (PinaforeAction baseedit TopType)
    -> EditFunction baseedit (WholeEdit (Maybe (IO ())))
actionReference raction =
    funcEditFunction (fmap (\action -> runPinaforeAction (action >> return ())) . knowToMaybe) .
    immutableReferenceToFunction raction

uiButton ::
       (?pinafore :: PinaforeContext baseedit)
    => PinaforeImmutableReference baseedit Text
    -> PinaforeImmutableReference baseedit (PinaforeAction baseedit TopType)
    -> UISpec BottomType baseedit
uiButton text raction = buttonUISpec (clearText . immutableReferenceToFunction text) $ actionReference raction

uiLabel :: forall baseedit. PinaforeImmutableReference baseedit Text -> UISpec BottomType baseedit
uiLabel text = mapEditUISpec (immutableReferenceToLens text) $ uiUnknownValue mempty $ labelUISpec

uiDynamic :: forall baseedit. PinaforeImmutableReference baseedit (UISpec A baseedit) -> UISpec A baseedit
uiDynamic uiref = switchUISpec $ pinaforeImmutableReferenceValue nullUISpec uiref

aspectToAction :: Aspect a -> PinaforeAction baseedit a
aspectToAction aspect = do
    ma <- liftIO aspect
    pinaforeActionKnow $ maybeToKnow ma

openWindow ::
       forall baseedit. (?pinafore :: PinaforeContext baseedit)
    => PinaforeImmutableReference baseedit Text
    -> (PinaforeAction baseedit A -> PinaforeImmutableReference baseedit (MenuBar baseedit))
    -> UISpec A baseedit
    -> PinaforeAction baseedit PinaforeWindow
openWindow title getmbar wsContent =
    mfix $ \w -> let
        wsCloseBoxAction = pwClose w
        wsTitle = clearText . immutableReferenceToFunction title
        wsMenuBar :: Maybe (Aspect A -> EditFunction baseedit (WholeEdit (MenuBar baseedit)))
        wsMenuBar =
            Just $ \aspect ->
                funcEditFunction (fromKnow mempty) . immutableReferenceToFunction (getmbar $ aspectToAction aspect)
        in pinaforeNewWindow MkWindowSpec {..}

uiWithSelection :: (PinaforeAction baseedit A -> UISpec A baseedit) -> UISpec A baseedit
uiWithSelection f = withAspectUISpec $ \aspect -> f $ aspectToAction aspect

uiTextArea :: forall baseedit. PinaforeLensValue baseedit (WholeEdit (Know Text)) -> UISpec BottomType baseedit
uiTextArea = valSpecText $ uiUnknownValue mempty $ noSelectionUISpec $ convertEditUISpec textAreaUISpec

uiCalendar :: forall baseedit. PinaforeLensValue baseedit (WholeEdit (Know Day)) -> UISpec BottomType baseedit
uiCalendar day = mapEditUISpec day $ uiUnknownValue (fromGregorian 1970 01 01) calendarUISpec

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
       forall baseedit. (?pinafore :: PinaforeContext baseedit)
    => Text
    -> Maybe Text
    -> PinaforeImmutableReference baseedit (PinaforeAction baseedit TopType)
    -> MenuEntry baseedit
menuAction label maccel raction =
    ActionMenuEntry
        label
        (do
             accel <- maccel
             interpretAccelerator $ unpack accel) $
    actionReference raction

uiScrolled :: forall baseedit. UISpec A baseedit -> UISpec A baseedit
uiScrolled = scrolledUISpec

ui_predefinitions ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => [DocTreeEntry (BindDoc baseedit)]
ui_predefinitions =
    [ docTreeEntry
          "UI"
          "A user interface is something that goes inside a window."
          [ mkValEntry "uiWithSelection" "User interface with selection." $ uiWithSelection @baseedit
          , mkValEntry "uiMap" "Map user interface selection" $ uiMap @baseedit
          , mkValEntry "uiIgnore" "Ignore user interface selection" $ noSelectionUISpec @baseedit @TopType @BottomType
          , mkValEntry "uiBlank" "Blank user-interface" $ nullUISpec @baseedit @BottomType
          , mkValEntry "uiUnitCheckBox" "(TBD)" $ \name val ->
                checkboxUISpec @baseedit @BottomType (clearText . name) $ toEditLens knowBool . val
          , mkValEntry "uiCheckBox" "Checkbox. Use shift-click to set to unknown." $ \name val ->
                maybeCheckboxUISpec @baseedit @BottomType (clearText . name) $ (bijectionWholeEditLens knowMaybe) . val
          , mkValEntry "uiTextEntry" "Text entry, empty text is unknown." $
            valSpecText $ uiUnknownValue mempty $ textAreaUISpecEntry @BottomType
          , mkValEntry "uiTextArea" "Text area, empty text is unknown." $ uiTextArea @baseedit
          , mkValEntry "uiLabel" "Label." $ uiLabel @baseedit
          , mkValEntry
                "uiHorizontal"
                "Items arranged horizontally, each flag is whether to expand into remaining space." $
            horizontalUISpec @baseedit @A
          , mkValEntry "uiVertical" "Items arranged vertically, each flag is whether to expand into remaining space." $
            verticalUISpec @baseedit @A
          , mkValEntry
                "uiPages"
                "A notebook of pages. First of each pair is for the page tab (typically a label), second is the content." $
            pagesUISpec @baseedit @A @TopType
                -- CSS
                -- drag
                -- icon
          , mkValEntry
                "uiButton"
                "A button with this text that does this action. Button will be disabled if the action reference is unknown." $
            uiButton
          , mkValEntry "uiPick" "A drop-down menu." $ uiPick @baseedit
          , mkValEntry
                "uiTable"
                "A list table. First arg is columns (name, property), second is order, third is the set of items, fourth is the window to open for a selection." $
            uiTable @baseedit
          , mkValEntry "uiCalendar" "A calendar." $ uiCalendar @baseedit
          , mkValEntry "uiScrolled" "A scrollable container." $ uiScrolled @baseedit
          , mkValEntry "uiDynamic" "A UI that can be updated to different UIs." $ uiDynamic @baseedit
          ]
    , docTreeEntry
          "Menu"
          "Menu items."
          [ mkValEntry "menuSeparator" "Separator menu item." SeparatorMenuEntry
          , mkValEntry "menuSubmenu" "Submenu menu item." SubMenuEntry
          , mkValEntry "menuAction" "Action menu item. Item will be disabled if the action reference is unknown." $
            menuAction @baseedit
          ]
    , docTreeEntry
          "Window"
          "User interface windows."
          [ mkValEntry "openWindow" "Open a new window with this title and UI." openWindow
          , mkValEntry "closeWindow" "Close a window." pwClose
          , mkValEntry "exitUI" "Exit the user interface." pinaforeExit
          ]
    ]
