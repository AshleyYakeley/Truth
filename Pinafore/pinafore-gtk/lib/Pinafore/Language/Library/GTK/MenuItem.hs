{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.MenuItem
    ( LangMenuItem(..)
    , LangMenuBar
    , menuItemStuff
    ) where

import Changes.Core
import Changes.UI.GTK
import Data.Shim
import Pinafore.Base
import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Element
import Shapes

-- LangMenuItem
newtype LangMenuItem =
    MkLangMenuItem ((View --> IO) -> MenuEntry)

menuItemGroundType :: PinaforeGroundType '[] LangMenuItem
menuItemGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangMenuItem)|]) "MenuItem"

instance HasPinaforeGroundType '[] LangMenuItem where
    pinaforeGroundType = menuItemGroundType

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

menuAction ::
       (?pinafore :: PinaforeContext)
    => Text
    -> Maybe Text
    -> PinaforeImmutableWholeRef (PinaforeAction TopType)
    -> LangMenuItem
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

menuItemStuff :: DocTreeEntry BindDoc
menuItemStuff =
    docTreeEntry
        "Menu"
        ""
        [ mkTypeEntry "MenuItem" "A item of a menu." $ MkBoundType menuItemGroundType
        , mkValEntry "menuSeparator" "Separator menu item." $ MkLangMenuItem $ \_ -> SeparatorMenuEntry
        , mkValEntry "menuSubmenu" "Submenu menu item." menuSubmenu
        , mkValEntry
              "menuAction"
              "Action menu item. Item will be disabled if the action reference is unknown."
              menuAction
        , mkValEntry "menuBar" "Menu bar element" uiMenuBar
        ]
