module Truth.Core.UI.Table where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types
import Truth.Core.UI.Button
import Truth.Core.UI.Lens
import Truth.Core.UI.Specifier
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

data UITable tedit where
    MkUITable
        :: forall cont tedit iedit.
           (KeyContainer cont, FullSubjectReader (EditReader iedit), HasKeyReader cont (EditReader iedit))
        => [KeyColumn tedit (ContainerKey cont)]
        -> (ContainerKey cont -> Aspect tedit)
        -> EditLens tedit (KeyEdit cont iedit)
        -> UITable tedit

uiTable ::
       forall cont tedit iedit.
       (KeyContainer cont, FullSubjectReader (EditReader iedit), HasKeyReader cont (EditReader iedit))
    => [KeyColumn tedit (ContainerKey cont)]
    -> (ContainerKey cont -> Aspect tedit)
    -> EditLens tedit (KeyEdit cont iedit)
    -> UISpec tedit
uiTable cols getaspect lens = MkUISpec $ MkUITable cols getaspect lens

uiSimpleTable ::
       forall cont iedit.
       ( KeyContainer cont
       , FullSubjectReader (EditReader iedit)
       , ApplicableEdit iedit
       , HasKeyReader cont (EditReader iedit)
       )
    => [KeyColumn (KeyEdit cont iedit) (ContainerKey cont)]
    -> Aspect (MaybeEdit iedit)
    -> UISpec (KeyEdit cont iedit)
uiSimpleTable cols aspect = uiTable cols (\key -> ioMapAspect (getKeyElementEditLens key) aspect) id

instance Show (UITable edit) where
    show (MkUITable _ _ _) = "table"

instance UIType UITable where
    uiWitness = $(iowitness [t|UITable|])

uiTableNewItemButton ::
       forall tedit cont iedit. IONewItemKeyContainer cont
    => EditFunction tedit (WholeEdit Text)
    -> EditLens tedit (KeyEdit cont iedit)
    -> UISpec tedit
uiTableNewItemButton label tableLens =
    uiButton label $
    mapViewEdit tableLens $
    viewObjectPushEdit $ \_ push -> do
        item <- liftIO $ newKeyContainerItem @cont
        push [KeyInsertReplaceItem item]
