{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.MenuEntry
    ( LangMenuEntry(..)
    , LangMenuBar
    , menuEntryStuff
    ) where

import Changes.Core
import Changes.World.GNOME.GTK
import Data.Shim
import Pinafore.API
import Pinafore.Language.Library.GTK.Widget
import Shapes

-- LangMenuEntry
newtype LangMenuEntry =
    MkLangMenuEntry ((View --> IO) -> MenuEntry)

menuItemGroundType :: QGroundType '[] LangMenuEntry
menuItemGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangMenuEntry)|]) "MenuEntry.GTK."

instance HasQGroundType '[] LangMenuEntry where
    qGroundType = menuItemGroundType

type LangMenuBar = [LangMenuEntry]

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

getLabelAccelModel :: ImmutableWholeModel (Text, Maybe Text) -> Model (ROWUpdate (Text, Maybe MenuAccelerator))
getLabelAccelModel labelAccel = let
    ff (Known (t, maccelStr)) =
        ( t
        , do
              accelStr <- maccelStr
              interpretAccelerator $ unpack accelStr)
    ff Unknown = ("", Nothing)
    in unWModel $ eaMapReadOnlyWhole ff $ immutableModelToReadOnlyModel labelAccel

menuAction :: ImmutableWholeModel (Text, Maybe Text) -> ImmutableWholeModel (Action TopType) -> LangMenuEntry
menuAction labelAccel raction =
    MkLangMenuEntry $ \unlift -> ActionMenuEntry (getLabelAccelModel labelAccel) $ unWModel $ actionRef unlift raction

menuChecked :: ImmutableWholeModel (Text, Maybe Text) -> WModel (WholeUpdate (Know Bool)) -> LangMenuEntry
menuChecked labelAccel val =
    MkLangMenuEntry $ \_ ->
        CheckedMenuEntry (getLabelAccelModel labelAccel) $ unWModel $ eaMap (unknownValueChangeLens False) val

menuSubmenu :: Text -> [LangMenuEntry] -> LangMenuEntry
menuSubmenu name entries =
    MkLangMenuEntry $ \unlift -> SubMenuEntry name $ fmap (\(MkLangMenuEntry entry) -> entry unlift) entries

uiMenuBar :: LangMenuBar -> LangWidget
uiMenuBar lmb =
    MkLangWidget $ \MkWidgetContext {..} -> createMenuBar wcAccelGroup $ fmap (\(MkLangMenuEntry me) -> me wcUnlift) lmb

menuEntryStuff :: LibraryStuff ()
menuEntryStuff =
    headingBDS
        "Menu"
        ""
        [ typeBDS "MenuEntry" "A item of a menu." (MkSomeGroundType menuItemGroundType) []
        , namespaceBDS
              "MenuEntry"
              [ valBDS "separator" "Separator menu item." $ MkLangMenuEntry $ \_ -> SeparatorMenuEntry
              , valBDS "submenu" "Submenu menu item." menuSubmenu
              , valBDS "action" "Action menu item. Item will be disabled if the action reference is unknown." menuAction
              , valBDS "checked" "Checked menu item." menuChecked
              ]
        , namespaceBDS "Widget" [valBDS "menuBar" "Menu bar widget" uiMenuBar]
        ]
