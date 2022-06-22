module Changes.UI.GTK.MenuBar
    ( KeyboardModifier(..)
    , KeyboardKey
    , MenuAccelerator(..)
    , MenuEntry(..)
    , simpleActionMenuItem
    , MenuBar
    , createMenuBar
    ) where

import Changes.Core
import Changes.GI
import Data.IORef
import GI.Gdk
import GI.Gtk hiding (MenuBar)
import qualified GI.Gtk as Gtk
import Shapes

data KeyboardModifier
    = KMShift
    | KMCtrl
    | KMAlt

type KeyboardKey = Char

data MenuAccelerator =
    MkMenuAccelerator [KeyboardModifier]
                      KeyboardKey

data MenuEntry
    = SeparatorMenuEntry
    | ActionMenuEntry Text
                      (Maybe MenuAccelerator)
                      (Model (ROWUpdate (Maybe (GView 'Locked ()))))
    | SubMenuEntry Text
                   [MenuEntry]

type MenuBar = [MenuEntry]

simpleActionMenuItem :: Text -> Maybe MenuAccelerator -> GView 'Locked () -> MenuEntry
simpleActionMenuItem label maccel action = ActionMenuEntry label maccel $ constantModel $ Just action

toModifierType :: KeyboardModifier -> ModifierType
toModifierType KMShift = ModifierTypeShiftMask
toModifierType KMCtrl = ModifierTypeControlMask
toModifierType KMAlt = ModifierTypeMod1Mask

accelGroupConnection :: IsAccelGroup ag => ag -> Word32 -> [ModifierType] -> [AccelFlags] -> IO () -> GView 'Locked ()
accelGroupConnection ag key mods flags action = do
    closure <-
        genClosure_AccelGroupActivate $ \_ _ _ _ -> do
            action
            return True
    accelGroupConnect ag key mods flags closure
    gvOnClose $ do
        _ <- gvLiftIO $ accelGroupDisconnect ag $ Just closure
        return ()

attachMenuEntry :: (IsMenuShell menushell, IsAccelGroup ag) => ag -> menushell -> MenuEntry -> GView 'Locked ()
attachMenuEntry ag ms (ActionMenuEntry label maccel raction) = do
    aref <- liftIO $ newIORef Nothing
    item <- menuItemNew
    menuShellAppend ms item
    let
        meaction :: GView 'Locked ()
        meaction = do
            maction <- liftIO $ readIORef aref
            case maction of
                Nothing -> return ()
                Just action -> action
    set item [#label := label] -- creates child if not present
    mc <- binGetChild item
    MkWMFunction unliftView <- gvGetUnlift
    for_ mc $ \c -> do
        ml <- liftIO $ castTo AccelLabel c
        for_ ml $ \l -> do
            case maccel of
                Nothing -> accelLabelSetAccel l 0 []
                Just (MkMenuAccelerator mods key) -> do
                    let
                        keyw :: Word32
                        keyw = fromIntegral $ ord key
                        gmods :: [ModifierType]
                        gmods = fmap toModifierType mods
                    accelLabelSetAccel l keyw gmods
                    accelGroupConnection ag keyw gmods [AccelFlagsVisible] $ unliftView $ gvRunLocked meaction
    gvBindReadOnlyWholeModel raction $ \maction ->
        gvRunLocked $ do
            liftIO $ writeIORef aref maction
            set item [#sensitive := isJust maction]
    _ <- gvOnSignal item #activate meaction
    return ()
attachMenuEntry ag ms (SubMenuEntry name entries) = do
    item <- menuItemNewWithLabel name
    menuShellAppend ms item
    menu <- menuNew
    menuItemSetSubmenu item $ Just menu
    attachMenuEntries ag menu entries
attachMenuEntry _ ms SeparatorMenuEntry = do
    item <- gvNew SeparatorMenuItem []
    menuShellAppend ms item

attachMenuEntries :: (IsMenuShell menushell, IsAccelGroup ag) => ag -> menushell -> [MenuEntry] -> GView 'Locked ()
attachMenuEntries ag menu mm = for_ mm $ attachMenuEntry ag menu

createMenuBar :: IsAccelGroup ag => ag -> MenuBar -> GView 'Locked Gtk.MenuBar
createMenuBar ag menu = do
    mbar <- menuBarNew
    attachMenuEntries ag mbar menu
    return mbar
