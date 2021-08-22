{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.MenuItem
    ( menuItemStuff
    ) where

import Changes.Core
import Changes.UI.GTK
import Data.Shim
import Pinafore.Base
import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Element
import Shapes

-- LangMenuItem
type LangMenuItem = MenuEntry

menuItemGroundType :: PinaforeGroundType '[] LangMenuItem
menuItemGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (HetEqual LangMenuItem)|]) "MenuItem"

instance HasPinaforeGroundType '[] LangMenuItem where
    pinaforeGroundType = menuItemGroundType

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
    in ActionMenuEntry label maccel $ unWModel $ actionRef raction

menuItemStuff :: DocTreeEntry BindDoc
menuItemStuff =
    docTreeEntry
        "Menu"
        ""
        [ mkTypeEntry "MenuItem" "A item of a menu." $ MkBoundType menuItemGroundType
        , mkValEntry "menuSeparator" "Separator menu item." SeparatorMenuEntry
        , mkValEntry "menuSubmenu" "Submenu menu item." SubMenuEntry
        , mkValEntry
              "menuAction"
              "Action menu item. Item will be disabled if the action reference is unknown."
              menuAction
        ]
