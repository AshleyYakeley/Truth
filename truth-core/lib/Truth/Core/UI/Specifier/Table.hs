module Truth.Core.UI.Specifier.Table where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types
import Truth.Core.UI.Specifier.Button
import Truth.Core.UI.Specifier.Lens
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.View

data TableCellProps = MkTableCellProps
    { tcItalic :: Bool
    }

tableCellPlain :: TableCellProps
tableCellPlain = let
    tcItalic = False
    in MkTableCellProps {..}

data KeyColumn tedit key = MkKeyColumn
    { kcName :: EditFunction tedit (WholeEdit Text)
    , kcContents :: key -> IO (EditLens tedit (WholeEdit Text), EditFunction tedit (WholeEdit TableCellProps))
    }

readOnlyKeyColumn ::
       EditFunction tedit (WholeEdit Text)
    -> (key -> IO (EditFunction tedit (WholeEdit (Text, TableCellProps))))
    -> KeyColumn tedit key
readOnlyKeyColumn kcName getter = let
    kcContents key = do
        func <- getter key
        return (readOnlyEditLens $ funcEditFunction fst . func, funcEditFunction snd . func)
    in MkKeyColumn {..}

data UITable seledit tedit where
    MkUITable
        :: forall cont tedit iedit.
           (KeyContainer cont, FullSubjectReader (EditReader iedit), HasKeyReader cont (EditReader iedit))
        => [KeyColumn tedit (ContainerKey cont)]
        -> (ContainerKey cont -> IO (UIWindow tedit))
        -> EditLens tedit (KeyEdit cont iedit)
        -> UITable (ConstEdit (ContainerKey cont)) tedit

uiTable ::
       forall cont tedit iedit.
       (KeyContainer cont, FullSubjectReader (EditReader iedit), HasKeyReader cont (EditReader iedit))
    => [KeyColumn tedit (ContainerKey cont)]
    -> (ContainerKey cont -> IO (UIWindow tedit))
    -> EditLens tedit (KeyEdit cont iedit)
    -> UISpec (ConstEdit (ContainerKey cont)) tedit
uiTable cols getaspect lens = MkUISpec $ MkUITable cols getaspect lens

uiSimpleTable ::
       forall cont iedit.
       ( KeyContainer cont
       , FullSubjectReader (EditReader iedit)
       , ApplicableEdit iedit
       , HasKeyReader cont (EditReader iedit)
       )
    => [KeyColumn (KeyEdit cont iedit) (ContainerKey cont)]
    -> UIWindow (MaybeEdit iedit)
    -> UISpec (ConstEdit (ContainerKey cont)) (KeyEdit cont iedit)
uiSimpleTable cols uiw =
    uiTable
        cols
        (\key -> do
             elemlens <- getKeyElementEditLens key
             return $ uiWindowMapEdit elemlens uiw)
        id

instance Show (UITable seledit edit) where
    show (MkUITable _ _ _) = "table"

instance UIType UITable where
    uiWitness = $(iowitness [t|UITable|])

tableNewItem ::
       forall tedit cont iedit seledit. IONewItemKeyContainer cont
    => EditLens tedit (KeyEdit cont iedit)
    -> View seledit tedit ()
tableNewItem tableLens =
    viewMapEdit tableLens $
    viewObjectPushEdit $ \_ push -> do
        item <- liftIO $ newKeyContainerItem @cont
        push [KeyInsertReplaceItem item]

uiTableNewItemButton ::
       forall tedit cont iedit seledit. IONewItemKeyContainer cont
    => EditFunction tedit (WholeEdit Text)
    -> EditLens tedit (KeyEdit cont iedit)
    -> UISpec seledit tedit
uiTableNewItemButton label tableLens = uiButton label $ tableNewItem tableLens
