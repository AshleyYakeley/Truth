{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.MenuItem
    ( LangMenuItem(..)
    , LangMenuBar
    , menuItemStuff
    ) where

import Changes.Core
import Changes.World.GNOME.GTK
import Data.Shim
import Pinafore.Base
import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Element
import Shapes

-- LangMenuItem
newtype LangMenuItem =
    MkLangMenuItem ((View --> IO) -> MenuEntry)

menuItemGroundType :: QGroundType '[] LangMenuItem
menuItemGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangMenuItem)|]) ".GTK.MenuItem"

instance HasQGroundType '[] LangMenuItem where
    qGroundType = menuItemGroundType

type LangMenuBar = [LangMenuItem]

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

menuAction :: Text -> Maybe Text -> ImmutableWholeModel (Action TopType) -> LangMenuItem
menuAction label maccelStr raction = let
    maccel = do
        accelStr <- maccelStr
        interpretAccelerator $ unpack accelStr
    in MkLangMenuItem $ \unlift -> ActionMenuEntry label maccel $ unWModel $ actionRef unlift raction

menuSubmenu :: Text -> [LangMenuItem] -> LangMenuItem
menuSubmenu name entries =
    MkLangMenuItem $ \unlift -> SubMenuEntry name $ fmap (\(MkLangMenuItem entry) -> entry unlift) entries

uiMenuBar :: LangMenuBar -> LangElement
uiMenuBar lmb =
    MkLangElement $ \MkElementContext {..} ->
        createMenuBar ecAccelGroup $ fmap (\(MkLangMenuItem me) -> me ecUnlift) lmb

menuItemStuff :: BindDocTree ()
menuItemStuff =
    headingBDT
        "Menu"
        ""
        [ typeBDT "MenuItem" "A item of a menu." (MkSomeGroundType menuItemGroundType) []
        , valBDT "menuSeparator" "Separator menu item." $ MkLangMenuItem $ \_ -> SeparatorMenuEntry
        , valBDT "menuSubmenu" "Submenu menu item." menuSubmenu
        , valBDT "menuAction" "Action menu item. Item will be disabled if the action reference is unknown." menuAction
        , valBDT "menuBar" "Menu bar element" uiMenuBar
        ]
