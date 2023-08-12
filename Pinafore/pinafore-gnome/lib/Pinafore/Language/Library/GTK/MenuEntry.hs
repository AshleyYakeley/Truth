{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.MenuEntry
    ( LangMenuEntry(..)
    , LangMenuBar
    , menuEntryStuff
    ) where

import Changes.Core
import Changes.World.GNOME.GTK
import Data.Shim
import Pinafore.Base
import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Element
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

menuAction :: Text -> Maybe Text -> ImmutableWholeModel (Action TopType) -> LangMenuEntry
menuAction label maccelStr raction = let
    maccel = do
        accelStr <- maccelStr
        interpretAccelerator $ unpack accelStr
    in MkLangMenuEntry $ \unlift -> ActionMenuEntry label maccel $ unWModel $ actionRef unlift raction

menuSubmenu :: Text -> [LangMenuEntry] -> LangMenuEntry
menuSubmenu name entries =
    MkLangMenuEntry $ \unlift -> SubMenuEntry name $ fmap (\(MkLangMenuEntry entry) -> entry unlift) entries

uiMenuBar :: LangMenuBar -> LangElement
uiMenuBar lmb =
    MkLangElement $ \MkElementContext {..} ->
        gvRunLocked $ createMenuBar ecAccelGroup $ fmap (\(MkLangMenuEntry me) -> me ecUnlift) lmb

menuEntryStuff :: BindDocStuff ()
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
              ]
        , namespaceBDS "Element" [valBDS "menuBar" "Menu bar element" uiMenuBar]
        ]
