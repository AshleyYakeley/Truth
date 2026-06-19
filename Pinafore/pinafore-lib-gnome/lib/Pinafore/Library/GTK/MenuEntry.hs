{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Library.GTK.MenuEntry
    ( LangMenuEntry (..)
    , LangMenuBar
    , menuEntryStuff
    )
where

import Changes.Core
import Changes.World.GNOME.GTK
import Data.Shim
import Pinafore.API
import Shapes

import Pinafore.Library.GTK.Widget

-- LangMenu
newtype LangMenu
    = MkLangMenu ((View --> IO) -> Menu)

instance HasQGroundType '[] LangMenu where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangMenu)|]) "Menu.GTK."

-- LangMenuEntry
newtype LangMenuEntry
    = MkLangMenuEntry ((View --> IO) -> MenuEntry)

instance HasQGroundType '[] LangMenuEntry where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangMenuEntry)|]) "Entry.Menu.GTK."

type LangMenuBar = [LangMenu]

interpretAccelerator :: String -> Maybe MenuAccelerator
interpretAccelerator [c] = Just $ MkMenuAccelerator [] c
interpretAccelerator ('C' : 't' : 'r' : 'l' : '+' : s) = do
    MkMenuAccelerator mods c <- interpretAccelerator s
    return $ MkMenuAccelerator (KMCtrl : mods) c
interpretAccelerator ('S' : 'h' : 'i' : 'f' : 't' : '+' : s) = do
    MkMenuAccelerator mods c <- interpretAccelerator s
    return $ MkMenuAccelerator (KMShift : mods) c
interpretAccelerator ('A' : 'l' : 't' : '+' : s) = do
    MkMenuAccelerator mods c <- interpretAccelerator s
    return $ MkMenuAccelerator (KMAlt : mods) c
interpretAccelerator _ = Nothing

getLabelAccelModel :: ImmutableWholeModel (Text, Maybe Text) -> Model (ROWUpdate (Text, Maybe MenuAccelerator))
getLabelAccelModel labelAccel = let
    ff (Known (t, maccelStr)) =
        ( t
        , do
            accelStr <- maccelStr
            interpretAccelerator $ unpack accelStr
        )
    ff Unknown = ("", Nothing)
    in unWModel $ eaMapReadOnlyWhole ff $ immutableModelToReadOnlyModel labelAccel

menuAction :: ImmutableWholeModel (Text, Maybe Text) -> ImmutableWholeModel (Action TopType) -> LangMenuEntry
menuAction labelAccel raction =
    MkLangMenuEntry $ \unlift -> ActionMenuEntry (getLabelAccelModel labelAccel) $ unWModel $ actionRef unlift raction

menuChecked :: ImmutableWholeModel (Text, Maybe Text) -> WModel (WholeUpdate (Know Bool)) -> LangMenuEntry
menuChecked labelAccel val =
    MkLangMenuEntry $ \_ ->
        CheckedMenuEntry (getLabelAccelModel labelAccel) $ unWModel $ eaMap (unknownValueChangeLens False) val

mkMenu :: Text -> [LangMenuEntry] -> LangMenu
mkMenu name entries =
    MkLangMenu $ \unlift -> MkMenu name $ fmap (\(MkLangMenuEntry entry) -> entry unlift) entries

menuSubmenu :: LangMenu -> LangMenuEntry
menuSubmenu (MkLangMenu menu) =
    MkLangMenuEntry $ \unlift -> SubMenuEntry $ menu unlift

uiMenuBar :: LangMenuBar -> LangWidget
uiMenuBar lmb =
    MkLangWidget $ \MkWidgetContext{..} -> createMenuBar $ fmap (\(MkLangMenu me) -> me wcUnlift) lmb

menuEntryStuff :: LibraryStuff
menuEntryStuff =
    headingBDS
        "Menu"
        ""
        [ typeBDS "Menu" "A menu." (qSomeGroundType @_ @LangMenu) []
        , hasSubtypeRelationBDS @LangMenu @LangMenuEntry Verify "Menu as submenu"
            $ functionToShim "submenu" menuSubmenu
        , namespaceBDS
            "Menu"
            [ typeBDS "Entry" "A entry on a menu." (qSomeGroundType @_ @LangMenuEntry) []
            , valBDS "menu" "A menu (or submenu of a menu)." mkMenu
            , valBDS "separator" "Separator menu entry." $ MkLangMenuEntry $ \_ -> SeparatorMenuEntry
            , valBDS "action" "Action menu entry. Item will be disabled if the action reference is unknown." menuAction
            , valBDS "checked" "Checked menu entry." menuChecked
            ]
        , namespaceBDS "Widget" [valBDS "menuBar" "Menu bar widget" uiMenuBar]
        ]
