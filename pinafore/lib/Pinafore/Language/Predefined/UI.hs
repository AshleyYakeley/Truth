module Pinafore.Language.Predefined.UI
    ( ui_predefinitions
    ) where

import Pinafore.Base
import Pinafore.Language.Doc
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
valSpecText spec val = uiLens val spec

clearText :: EditFunction (WholeEdit (Know Text)) (WholeEdit Text)
clearText = funcEditFunction (fromKnow mempty)

ui_map :: forall baseedit. (A -> B) -> PinaforeUI baseedit A -> PinaforeUI baseedit B
ui_map = fmap

ui_table ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => [(PinaforeReference baseedit '( BottomType, Text), A -> PinaforeReference baseedit '( BottomType, Text))]
    -> PinaforeSet baseedit '( A, MeetType Entity A)
    -> UISpec A baseedit
ui_table cols val = let
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
    in uiSetSelectionMap meet2 $ uiTable (fmap getColumn cols) $ unPinaforeSet $ contraMapRange meet2 val

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
    in uiOption @baseedit @PickerType opts $ pinaforeReferenceToLens $ contraMapRange meet2 ref

ui_button ::
       (?pinafore :: PinaforeContext baseedit)
    => PinaforeImmutableReference baseedit Text
    -> PinaforeAction baseedit ()
    -> UISpec BottomType baseedit
ui_button text action = uiButton (clearText . immutableReferenceToFunction text) (runPinaforeAction action)

ui_label :: forall baseedit. PinaforeImmutableReference baseedit Text -> UISpec BottomType baseedit
ui_label text = uiLens (immutableReferenceToLens text) $ uiUnknownValue mempty $ uiLabel

ui_dynamic :: forall baseedit. PinaforeImmutableReference baseedit (UISpec A baseedit) -> UISpec A baseedit
ui_dynamic uiref = uiSwitch $ pinaforeImmutableReferenceValue uiNull uiref

openwindow ::
       (?pinafore :: PinaforeContext baseedit)
    => PinaforeImmutableReference baseedit Text
    -> UISpec A baseedit
    -> (A -> PinaforeAction baseedit ())
    -> PinaforeAction baseedit ()
openwindow title uiContent action = let
    uiTitle = clearText . immutableReferenceToFunction title
    uiAction sel = runPinaforeAction $ action sel
    in pinaforeNewWindow MkUIWindow {..}

{-
withSelection :: (NewEntity -> PinaforeAction baseedit ()) -> PinaforeAction baseedit ()
withSelection cont = do
    maspect <- pinaforeGetSelectionAspect
    case maspect of
        Nothing -> return ()
        Just aspect -> do
            e <- pinaforeLiftView $ viewObjectRead $ \_ mr -> editFunctionRead (editLensFunction (uiaLens aspect)) mr ReadWhole
            cont $ MkNewEntity e
-}
ui_textarea :: forall baseedit. PinaforeLensValue baseedit (WholeEdit (Know Text)) -> UISpec BottomType baseedit
ui_textarea = valSpecText $ uiUnknownValue mempty $ uiNoSelectionLens $ uiConvert uiText

ui_predefinitions ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => DocTree (BindDoc baseedit)
ui_predefinitions =
    MkDocTree
        "UI"
        "A user interface is something that goes inside a window."
        [ mkValEntry "openwindow" "Open a new window with this title and UI." openwindow
              -- NYI , mkValEntry "openselection" "Open the item selected in the UI of this window." viewOpenSelection
              {-
              , mkValEntry "withselection" "Act with the item selected in the UI of this window." $
                withSelection @baseedit
              -}
        , mkValEntry "ui_map" "Map user interface selection" $ ui_map @baseedit
        , mkValEntry "ui_ignore" "Ignore user interface selection" $ uiNoSelectionLens @baseedit @TopType @BottomType
        , mkValEntry "ui_blank" "Blank user-interface" $ uiNull @baseedit @BottomType
        , mkValEntry "ui_unitcheckbox" "(TBD)" $ \name val ->
              uiCheckbox @baseedit @BottomType (clearText . name) $ toEditLens knowBool . val
        , mkValEntry "ui_booleancheckbox" "Checkbox. Use shift-click to set to unknown." $ \name val ->
              uiMaybeCheckbox @baseedit @BottomType (clearText . name) $ (bijectionWholeEditLens knowMaybe) . val
        , mkValEntry "ui_textentry" "Text entry, empty text is unknown." $
          valSpecText $ uiUnknownValue mempty $ uiTextEntry @BottomType
        , mkValEntry "ui_textarea" "Text area, empty text is unknown." $ ui_textarea @baseedit
        , mkValEntry "ui_label" "Label." $ ui_label @baseedit
        , mkValEntry "ui_horizontal" "Items arranged horizontally, each flag is whether to expand into remaining space." $
          uiHorizontal @baseedit @A
        , mkValEntry "ui_vertical" "Items arranged vertically, each flag is whether to expand into remaining space." $
          uiVertical @baseedit @A
        , mkValEntry
              "ui_pages"
              "A notebook of pages. First of each pair is for the page tab (typically a label), second is the content." $
          uiPages @baseedit @A @TopType
                -- CSS
                -- drag
                -- icon
        , mkValEntry "ui_button" "A button with this text that does this action." $ ui_button
        , mkValEntry "ui_pick" "A drop-down menu." $ ui_pick @baseedit
        , mkValEntry
              "ui_table"
              "A list table. First arg is columns (name, property), second is the window to open for a selection, third is the set of items." $
          ui_table @baseedit
        , mkValEntry "ui_dynamic" "A UI that can be updated to different UIs." $ ui_dynamic @baseedit
        ]
