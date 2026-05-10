module Changes.World.GNOME.GTK.Widget.MenuBar
    ( KeyboardModifier (..)
    , KeyboardKey
    , MenuAccelerator (..)
    , MenuEntry (..)
    , simpleActionMenuEntry
    , MenuBar
    , createMenuBar
    )
where

import Data.IORef
import Data.List qualified as List

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

data KeyboardModifier
    = KMShift
    | KMCtrl
    | KMAlt

type KeyboardKey = Char

data MenuAccelerator
    = MkMenuAccelerator
        [KeyboardModifier]
        KeyboardKey

data MenuEntry
    = SeparatorMenuEntry
    | ActionMenuEntry
        (Model (ROWUpdate (Text, Maybe MenuAccelerator)))
        (Model (ROWUpdate (Maybe (GView 'Locked ()))))
    | CheckedMenuEntry
        (Model (ROWUpdate (Text, Maybe MenuAccelerator)))
        (Model (WholeUpdate Bool))
    | SubMenuEntry
        Text
        [MenuEntry]

type MenuBar = [MenuEntry]

simpleActionMenuEntry :: Text -> Maybe MenuAccelerator -> GView 'Locked () -> MenuEntry
simpleActionMenuEntry label maccel action =
    ActionMenuEntry (constantModel (label, maccel)) (constantModel $ Just action)

data MenuContext = MkMenuContext
    { mcActionGroup :: GI.SimpleActionGroup
    , mcShortcutController :: GI.ShortcutController
    , mcNextActionId :: MVar Word
    }

nextActionName :: MenuContext -> GView 'Unlocked Text
nextActionName MkMenuContext{..} =
    gvLiftIOTrustMeNoUI $ do
        i <- modifyMVar mcNextActionId $ \i -> return (succ i, i)
        return $ "action-" <> pack (show i)

separatorSplit :: [MenuEntry] -> [[MenuEntry]]
separatorSplit [] = []
separatorSplit entries =
    let
        isSeparatorEntry SeparatorMenuEntry = True
        isSeparatorEntry _ = False
        (before, after) = List.break isSeparatorEntry entries
        in case after of
            [] -> [before | not $ null before]
            (_ : rest) -> (if null before then [] else [before]) <> separatorSplit rest

toModifierType :: KeyboardModifier -> GI.ModifierType
toModifierType KMShift = GI.ModifierTypeShiftMask
toModifierType KMCtrl = GI.ModifierTypeControlMask
toModifierType KMAlt = GI.ModifierTypeAltMask

acceleratorText :: MenuAccelerator -> Text
acceleratorText (MkMenuAccelerator mods key) =
    let
        modifierText :: KeyboardModifier -> Text
        modifierText KMShift = "<Shift>"
        modifierText KMCtrl = "<Control>"
        modifierText KMAlt = "<Alt>"
        in mconcat (fmap modifierText mods) <> singleton key

setMenuItemText :: GI.IsMenuItem item => item -> Text -> Maybe MenuAccelerator -> GView 'Locked ()
setMenuItemText item label maccel = do
    GI.menuItemSetLabel item $ Just label
    maccelVariant <- liftIO $ traverse GI.toGVariant $ fmap acceleratorText maccel
    GI.menuItemSetAttributeValue item "accel" maccelVariant

setAcceleratorShortcut :: MenuContext -> Text -> IORef (Maybe GI.Shortcut) -> Maybe MenuAccelerator -> GView 'Locked ()
setAcceleratorShortcut MkMenuContext{..} detailedActionName currentShortcutRef maccel = do
    oldShortcut <- liftIO $ readIORef currentShortcutRef
    for_ oldShortcut $ #removeShortcut mcShortcutController
    newShortcut <- case maccel of
        Nothing -> return Nothing
        Just (MkMenuAccelerator mods key) -> do
            keyval <- GI.unicodeToKeyval $ fromIntegral $ ord key
            trigger <- GI.keyvalTriggerNew keyval $ fmap toModifierType mods
            action <- GI.namedActionNew detailedActionName
            shortcut <- GI.shortcutNew (Just trigger) (Just action)
            gvBind shortcut
            shortcut' <- gvDuplicateUnbound GI.Shortcut shortcut
            #addShortcut mcShortcutController shortcut'
            return $ Just shortcut
    liftIO $ writeIORef currentShortcutRef newShortcut

makeActionMenuItem :: MenuContext -> Model (ROWUpdate (Text, Maybe MenuAccelerator)) -> Model (ROWUpdate (Maybe (GView 'Locked ()))) -> GView 'Unlocked GI.MenuItem
makeActionMenuItem mc@MkMenuContext{..} rtext raction = do
    actionRef <- gvLiftIOTrustMeNoUI $ newIORef Nothing
    shortcutRef <- gvLiftIOTrustMeNoUI $ newIORef Nothing
    actionName <- nextActionName mc
    let
        detailedActionName = "menubar." <> actionName
    (item, action) <-
        gvRunLocked $ do
            item <- GI.menuItemNew Nothing Nothing
            GI.menuItemSetDetailedAction item detailedActionName
            action <- GI.simpleActionNew actionName Nothing
            GI.actionMapAddAction mcActionGroup action
            return (item, action)
    gvOnClose
        $ gsvRunLocked
        $ do
            GI.actionMapRemoveAction mcActionGroup actionName
            oldShortcut <- liftIO $ readIORef shortcutRef
            for_ oldShortcut $ GI.shortcutControllerRemoveShortcut mcShortcutController
    _ <-
        gvRunLocked
            $ gvOnSignal () action #activate
            $ \_ -> do
                maction <- liftIO $ readIORef actionRef
                for_ maction id
    gvBindReadOnlyWholeModel raction $ \maction -> do
        gvLiftIOTrustMeNoUI $ writeIORef actionRef maction
        gvRunLocked $ GI.simpleActionSetEnabled action $ isJust maction
    gvBindReadOnlyWholeModel rtext $ \(label, maccel) ->
        gvRunLocked $ do
            setMenuItemText item label maccel
            setAcceleratorShortcut mc detailedActionName shortcutRef maccel
    return item

makeCheckedMenuItem :: MenuContext -> Model (ROWUpdate (Text, Maybe MenuAccelerator)) -> Model (WholeUpdate Bool) -> GView 'Unlocked GI.MenuItem
makeCheckedMenuItem mc@MkMenuContext{..} rtext rchecked = do
    esrc <- gvNewEditSource
    shortcutRef <- gvLiftIOTrustMeNoUI $ newIORef Nothing
    actionName <- nextActionName mc
    let
        detailedActionName = "menubar." <> actionName
    initialState <- gvLiftIOTrustMeNoUI $ GI.toGVariant False
    (item, action, changedSignal) <-
        gvRunLocked $ do
            item <- GI.menuItemNew Nothing Nothing
            GI.menuItemSetDetailedAction item detailedActionName
            action <- GI.simpleActionNewStateful actionName Nothing initialState
            GI.actionMapAddAction mcActionGroup action
            changedSignal <-
                gvOnSignal () action #changeState $ \mstate ->
                    for_ mstate $ \vstate -> do
                        mchecked <- liftIO $ GI.fromGVariant vstate
                        for_ mchecked $ \checked -> do
                            GI.simpleActionSetState action vstate
                            _ <- gvRunUnlocked $ gvSetWholeModel rchecked esrc checked
                            return ()
            return (item, action, changedSignal)
    gvOnClose
        $ gsvRunLocked
        $ do
            GI.actionMapRemoveAction mcActionGroup actionName
            oldShortcut <- liftIO $ readIORef shortcutRef
            for_ oldShortcut $ GI.shortcutControllerRemoveShortcut mcShortcutController
    gvBindWholeModel rchecked (Just esrc) $ \checked ->
        gvRunLocked $ withSignalBlocked action changedSignal $ do
            checkedState <- liftIO $ GI.toGVariant checked
            GI.simpleActionSetState action checkedState
    gvBindReadOnlyWholeModel rtext $ \(label, maccel) ->
        gvRunLocked $ do
            setMenuItemText item label maccel
            setAcceleratorShortcut mc detailedActionName shortcutRef maccel
    return item

appendMenuEntry :: MenuContext -> GI.Menu -> MenuEntry -> GView 'Unlocked ()
appendMenuEntry _ _ SeparatorMenuEntry = return ()
appendMenuEntry mc menu (ActionMenuEntry rtext raction) = do
    item <- makeActionMenuItem mc rtext raction
    gvRunLocked $ GI.menuAppendItem menu item
appendMenuEntry mc menu (CheckedMenuEntry rtext rchecked) = do
    item <- makeCheckedMenuItem mc rtext rchecked
    gvRunLocked $ GI.menuAppendItem menu item
appendMenuEntry mc menu (SubMenuEntry name entries) = do
    submenu <- makeMenuModel mc entries
    gvRunLocked $ GI.menuAppendSubmenu menu (Just name) submenu

appendSection :: MenuContext -> GI.Menu -> [MenuEntry] -> GView 'Unlocked ()
appendSection mc menu entries = do
    section <- gvRunLocked GI.menuNew
    for_ entries $ appendMenuEntry mc section
    gvRunLocked $ GI.menuAppendSection menu Nothing section

makeMenuModel :: MenuContext -> [MenuEntry] -> GView 'Unlocked GI.Menu
makeMenuModel mc entries = do
    menu <- gvRunLocked GI.menuNew
    for_ (separatorSplit entries) $ appendSection mc menu
    return menu

createMenuBar :: MenuBar -> GView 'Unlocked GI.Widget
createMenuBar menuEntries = do
    actionGroup <- gvRunLocked GI.simpleActionGroupNew
    shortcutController <-
        gvRunLocked $ do
            controller <- GI.shortcutControllerNew
            gvBind controller
            #setScope controller GI.ShortcutScopeGlobal
            return controller
    nextIdRef <- gvLiftIOTrustMeNoUI $ newMVar 0
    let
        mc = MkMenuContext{mcActionGroup = actionGroup, mcShortcutController = shortcutController, mcNextActionId = nextIdRef}
    menuModel <- makeMenuModel mc menuEntries
    gvRunLocked $ do
        menubar <- GI.popoverMenuBarNewFromModel $ Just menuModel
        #insertActionGroup menubar "menubar" $ Just actionGroup
        shortcutController' <- gvDuplicateUnbound GI.ShortcutController shortcutController
        #addController menubar shortcutController'
        GI.toWidget menubar
