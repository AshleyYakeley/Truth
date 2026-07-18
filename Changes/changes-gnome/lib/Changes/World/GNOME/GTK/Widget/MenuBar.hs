module Changes.World.GNOME.GTK.Widget.MenuBar
    ( KeyboardModifier (..)
    , KeyboardKey
    , MenuAccelerator (..)
    , MenuEntry (..)
    , Menu (..)
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
    | SubMenuEntry Menu

data Menu = MkMenu Text [MenuEntry]

type MenuBar = [Menu]

simpleActionMenuEntry :: Text -> Maybe MenuAccelerator -> GView 'Locked () -> MenuEntry
simpleActionMenuEntry label maccel action =
    ActionMenuEntry (constantModel (label, maccel)) (constantModel $ Just action)

data MenuContext = MkMenuContext
    { mcActionGroup :: GI.SimpleActionGroup
    , mcShortcutControllerRef :: IORef (Maybe GI.ShortcutController)
    , mcNextActionId :: MVar Word
    }

nextActionName :: MenuContext -> GView 'Unlocked Text
nextActionName MkMenuContext{..} =
    gvLiftIOTrustMeNoUI $ do
        i <- modifyMVar mcNextActionId $ \i -> return (succ i, i)
        return $ "action-" <> pack (show i)

withShortcutController :: MenuContext -> (GI.ShortcutController -> IO ()) -> IO ()
withShortcutController MkMenuContext{..} call = do
    mcontroller <- readIORef mcShortcutControllerRef
    for_ mcontroller call

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
    liftIO $ writeIORef currentShortcutRef Nothing
    mcontroller <- liftIO $ readIORef mcShortcutControllerRef
    newShortcut <- for mcontroller $ \controller -> do
        for_ oldShortcut $ #removeShortcut controller
        for maccel $ \(MkMenuAccelerator mods key) -> do
            keyval <- GI.unicodeToKeyval $ fromIntegral $ ord key
            trigger <- GI.keyvalTriggerNew keyval $ fmap toModifierType mods
            action <- GI.namedActionNew detailedActionName
            shortcut <- GI.shortcutNew (Just trigger) (Just action)
            gvBind shortcut
            -- Clear refs before disowning so later close actions skip closed wrappers.
            gvOnClose $ gsvLiftIO $ writeIORef currentShortcutRef Nothing
            shortcut' <- gvDuplicateUnbound GI.Shortcut shortcut
            #addShortcut controller shortcut'
            return shortcut
    liftIO $ writeIORef currentShortcutRef $ exec newShortcut

clearAcceleratorShortcut :: MenuContext -> IORef (Maybe GI.Shortcut) -> IO ()
clearAcceleratorShortcut mc currentShortcutRef = do
    oldShortcut <- readIORef currentShortcutRef
    writeIORef currentShortcutRef Nothing
    for_ oldShortcut $ \shortcut ->
        withShortcutController mc $ \controller ->
            GI.shortcutControllerRemoveShortcut controller shortcut

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
        $ liftIO
        $ do
            GI.actionMapRemoveAction mcActionGroup actionName
            clearAcceleratorShortcut mc shortcutRef
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
        $ liftIO
        $ do
            GI.actionMapRemoveAction mcActionGroup actionName
            clearAcceleratorShortcut mc shortcutRef
    gvBindWholeModel rchecked (Just esrc) $ \checked ->
        gvRunLocked $ withSignalBlocked action changedSignal $ do
            checkedState <- liftIO $ GI.toGVariant checked
            GI.simpleActionSetState action checkedState
    gvBindReadOnlyWholeModel rtext $ \(label, maccel) ->
        gvRunLocked $ do
            setMenuItemText item label maccel
            setAcceleratorShortcut mc detailedActionName shortcutRef maccel
    return item

appendMenu :: MenuContext -> GI.Menu -> Menu -> GView 'Unlocked ()
appendMenu mc parent (MkMenu name entries) = do
    submenu <- makeMenuModel mc entries
    gvRunLocked $ GI.menuAppendSubmenu parent (Just name) submenu

appendMenuEntry :: MenuContext -> GI.Menu -> MenuEntry -> GView 'Unlocked ()
appendMenuEntry _ _ SeparatorMenuEntry = return ()
appendMenuEntry mc parent (ActionMenuEntry rtext raction) = do
    item <- makeActionMenuItem mc rtext raction
    gvRunLocked $ GI.menuAppendItem parent item
appendMenuEntry mc parent (CheckedMenuEntry rtext rchecked) = do
    item <- makeCheckedMenuItem mc rtext rchecked
    gvRunLocked $ GI.menuAppendItem parent item
appendMenuEntry mc parent (SubMenuEntry menu) = appendMenu mc parent menu

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

makeMenuBarModel :: MenuContext -> [Menu] -> GView 'Unlocked GI.Menu
makeMenuBarModel mc menus = do
    menuBar <- gvRunLocked GI.menuNew
    for_ menus $ appendMenu mc menuBar
    return menuBar

createMenuBar :: MenuBar -> GView 'Unlocked GI.Widget
createMenuBar menus = do
    actionGroup <- gvRunLocked GI.simpleActionGroupNew
    (shortcutController, shortcutControllerRef) <-
        gvRunLocked $ do
            controller <- gvNew GI.ShortcutController []
            controllerRef <- liftIO $ newIORef $ Just controller
            -- Clear refs before disowning so later close actions skip closed wrappers.
            gvOnClose $ gsvLiftIO $ writeIORef controllerRef Nothing
            #setScope controller GI.ShortcutScopeGlobal
            return (controller, controllerRef)
    nextIdRef <- gvLiftIOTrustMeNoUI $ newMVar 0
    let
        mc = MkMenuContext{mcActionGroup = actionGroup, mcShortcutControllerRef = shortcutControllerRef, mcNextActionId = nextIdRef}
    menuModel <- makeMenuBarModel mc menus
    gvRunLocked $ do
        menubar <- GI.popoverMenuBarNewFromModel $ Just menuModel
        #insertActionGroup menubar "menubar" $ Just actionGroup
        shortcutController' <- gvDuplicateUnbound GI.ShortcutController shortcutController
        #addController menubar shortcutController'
        GI.toWidget menubar
