module Pinafore.Language.Predefined.UI
    ( ui_predefinitions
    ) where

import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.Morphism
import Pinafore.Language.Predefined.Defs
import Pinafore.Language.Reference
import Pinafore.Language.Set
import Pinafore.Language.Type
import Pinafore.Language.UI
import Pinafore.Storage.File
import Shapes
import Truth.Core

valSpecText ::
       UISpec sel (WholeEdit (Know Text)) -> PinaforeLensValue baseedit (WholeEdit (Know Text)) -> UISpec sel baseedit
valSpecText spec val = mapUISpec val spec

clearText :: EditFunction (WholeEdit (Know Text)) (WholeEdit Text)
clearText = funcEditFunction (fromKnow mempty)

ui_map :: forall baseedit. (A -> B) -> PinaforeUI baseedit A -> PinaforeUI baseedit B
ui_map = fmap

ui_table ::
       forall baseedit. (?pinafore :: PinaforeContext baseedit, HasPinaforeEntityEdit baseedit)
    => [(PinaforeReference baseedit '( BottomType, Text), A -> PinaforeReference baseedit '( BottomType, Text))]
    -> PinaforeSet baseedit '( A, MeetType Entity A)
    -> (A -> PinaforeAction baseedit TopType)
    -> UISpec A baseedit
ui_table cols val onDoubleClick = let
    showCell :: Know Text -> (Text, TableCellProps)
    showCell (Known s) = (s, tableCellPlain)
    showCell Unknown = ("unknown", tableCellPlain {tcItalic = True})
    mapLens :: PinaforeFunctionValue baseedit (Know Text) -> PinaforeFunctionValue baseedit (Text, TableCellProps)
    mapLens ff = funcEditFunction showCell . ff
    getColumn ::
           (PinaforeReference baseedit '( BottomType, Text), A -> PinaforeReference baseedit '( BottomType, Text))
        -> KeyColumn baseedit (MeetType Entity A)
    getColumn (name, f) =
        readOnlyKeyColumn (clearText . pinaforeReferenceToFunction name) $ \p ->
            return $ mapLens $ pinaforeReferenceToFunction $ f $ meet2 p
    in mapSelectionUISpec meet2 $
       tableUISpec
           (fmap getColumn cols)
           (unPinaforeSet $ contraMapRange meet2 val)
           (\a -> runPinaforeAction $ void $ onDoubleClick $ meet2 a)

type PickerType = Know (MeetType Entity A)

type PickerPairType = (PickerType, Text)

ui_pick ::
       forall baseedit.
       PinaforeMorphism baseedit '( A, TopType) '( BottomType, Text)
    -> PinaforeSet baseedit '( A, MeetType Entity A)
    -> PinaforeReference baseedit '( A, MeetType Entity A)
    -> UISpec BottomType baseedit
ui_pick nameMorphism fset ref = let
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
        convertEditFunction . applyPinaforeFunction getNames (pinaforeSetFunctionValue fset)
    in optionUISpec @baseedit @PickerType opts $ pinaforeReferenceToLens $ contraMapRange meet2 ref

actionReference ::
       (?pinafore :: PinaforeContext baseedit)
    => PinaforeImmutableReference baseedit (PinaforeAction baseedit TopType)
    -> EditFunction baseedit (WholeEdit (Maybe (IO ())))
actionReference raction =
    funcEditFunction (fmap (\action -> runPinaforeAction (action >> return ())) . knowToMaybe) .
    immutableReferenceToFunction raction

ui_button ::
       (?pinafore :: PinaforeContext baseedit)
    => PinaforeImmutableReference baseedit Text
    -> PinaforeImmutableReference baseedit (PinaforeAction baseedit TopType)
    -> UISpec BottomType baseedit
ui_button text raction = buttonUISpec (clearText . immutableReferenceToFunction text) $ actionReference raction

ui_label :: forall baseedit. PinaforeImmutableReference baseedit Text -> UISpec BottomType baseedit
ui_label text = mapUISpec (immutableReferenceToLens text) $ uiUnknownValue mempty $ labelUISpec

ui_dynamic :: forall baseedit. PinaforeImmutableReference baseedit (UISpec A baseedit) -> UISpec A baseedit
ui_dynamic uiref = switchUISpec $ pinaforeImmutableReferenceValue nullUISpec uiref

openwindow ::
       (?pinafore :: PinaforeContext baseedit)
    => PinaforeImmutableReference baseedit Text
    -> UISpec TopType baseedit
    -> PinaforeAction baseedit PinaforeWindow
openwindow title wsContent = let
    wsTitle = clearText . immutableReferenceToFunction title
    in pinaforeNewWindow MkWindowSpec {..}

ui_withselection :: (PinaforeAction baseedit A -> UISpec A baseedit) -> UISpec A baseedit
ui_withselection f =
    withAspectUISpec $ \aspect ->
        f $ do
            ma <- liftIO aspect
            pinaforeActionKnow $ maybeToKnow ma

ui_textarea :: forall baseedit. PinaforeLensValue baseedit (WholeEdit (Know Text)) -> UISpec BottomType baseedit
ui_textarea = valSpecText $ uiUnknownValue mempty $ noSelectionUISpec $ convertUISpec textAreaUISpec

menu_action ::
       forall baseedit. (?pinafore :: PinaforeContext baseedit)
    => PinaforeImmutableReference baseedit Text
    -> PinaforeImmutableReference baseedit (PinaforeAction baseedit TopType)
    -> MenuEntry baseedit
menu_action rlabel raction =
    ActionMenuEntry (funcEditFunction (\kt -> (fromKnow "" kt, Nothing)) . immutableReferenceToFunction rlabel) $
    actionReference raction

ui_menubar :: forall baseedit. [MenuEntry baseedit] -> UISpec BottomType baseedit
ui_menubar = menuBarUISpec

ui_scrolled :: forall baseedit. UISpec A baseedit -> UISpec A baseedit
ui_scrolled = scrolledUISpec

ui_predefinitions ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => [DocTreeEntry (BindDoc baseedit)]
ui_predefinitions =
    [ docTreeEntry
          "UI"
          "A user interface is something that goes inside a window."
          [ mkValEntry "ui_withselection" "User interface with selection." $ ui_withselection @baseedit
          , mkValEntry "ui_map" "Map user interface selection" $ ui_map @baseedit
          , mkValEntry "ui_ignore" "Ignore user interface selection" $ noSelectionUISpec @baseedit @TopType @BottomType
          , mkValEntry "ui_blank" "Blank user-interface" $ nullUISpec @baseedit @BottomType
          , mkValEntry "ui_unitcheckbox" "(TBD)" $ \name val ->
                checkboxUISpec @baseedit @BottomType (clearText . name) $ toEditLens knowBool . val
          , mkValEntry "ui_booleancheckbox" "Checkbox. Use shift-click to set to unknown." $ \name val ->
                maybeCheckboxUISpec @baseedit @BottomType (clearText . name) $ (bijectionWholeEditLens knowMaybe) . val
          , mkValEntry "ui_textentry" "Text entry, empty text is unknown." $
            valSpecText $ uiUnknownValue mempty $ textAreaUISpecEntry @BottomType
          , mkValEntry "ui_textarea" "Text area, empty text is unknown." $ ui_textarea @baseedit
          , mkValEntry "ui_label" "Label." $ ui_label @baseedit
          , mkValEntry
                "ui_horizontal"
                "Items arranged horizontally, each flag is whether to expand into remaining space." $
            horizontalUISpec @baseedit @A
          , mkValEntry "ui_vertical" "Items arranged vertically, each flag is whether to expand into remaining space." $
            verticalUISpec @baseedit @A
          , mkValEntry
                "ui_pages"
                "A notebook of pages. First of each pair is for the page tab (typically a label), second is the content." $
            pagesUISpec @baseedit @A @TopType
                -- CSS
                -- drag
                -- icon
          , mkValEntry
                "ui_button"
                "A button with this text that does this action. Button will be disabled if the action reference is unknown." $
            ui_button
          , mkValEntry "ui_pick" "A drop-down menu." $ ui_pick @baseedit
          , mkValEntry
                "ui_table"
                "A list table. First arg is columns (name, property), second is the window to open for a selection, third is the set of items." $
            ui_table @baseedit
          , mkValEntry "ui_scrolled" "A scrollable container." $ ui_scrolled @baseedit
          , mkValEntry "ui_dynamic" "A UI that can be updated to different UIs." $ ui_dynamic @baseedit
          ]
    , docTreeEntry
          "Menu"
          "Menu items."
          [ mkValEntry "menu_separator" "Separator menu item." SeparatorMenuEntry
          , mkValEntry "menu_submenu" "Submenu menu item." SubMenuEntry
          , mkValEntry "menu_action" "Action menu item. Item will be disabled if the action reference is unknown." $
            menu_action @baseedit
          , mkValEntry "ui_menubar" "Menu bar." $ ui_menubar @baseedit
          ]
    , docTreeEntry
          "Window"
          "User interface windows."
          [ mkValEntry "openwindow" "Open a new window with this title and UI." openwindow
          , mkValEntry "closewindow" "Close a window." $ uiWindowClose . pwWindow
          ]
    ]
