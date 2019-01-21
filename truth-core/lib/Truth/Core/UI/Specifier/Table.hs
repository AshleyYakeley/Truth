module Truth.Core.UI.Specifier.Table where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types
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

data UITable sel tedit where
    MkUITable
        :: forall cont tedit iedit.
           (KeyContainer cont, FullSubjectReader (EditReader iedit), HasKeyReader cont (EditReader iedit))
        => [KeyColumn tedit (ContainerKey cont)]
        -> EditLens tedit (KeyEdit cont iedit)
        -> UITable (ContainerKey cont) tedit

uiTable ::
       forall cont tedit iedit.
       (KeyContainer cont, FullSubjectReader (EditReader iedit), HasKeyReader cont (EditReader iedit))
    => [KeyColumn tedit (ContainerKey cont)]
    -> EditLens tedit (KeyEdit cont iedit)
    -> UISpec (ContainerKey cont) tedit
uiTable cols lens = MkUISpec $ MkUITable cols lens

uiSimpleTable ::
       forall cont iedit.
       ( KeyContainer cont
       , FullSubjectReader (EditReader iedit)
       , ApplicableEdit iedit
       , HasKeyReader cont (EditReader iedit)
       )
    => [KeyColumn (KeyEdit cont iedit) (ContainerKey cont)]
    -> UISpec (ContainerKey cont) (KeyEdit cont iedit)
uiSimpleTable cols = uiTable cols id

instance Show (UITable sel edit) where
    show (MkUITable _ _) = "table"

instance UIType UITable where
    uiWitness = $(iowitness [t|UITable|])

tableNewItem ::
       forall tedit cont iedit sel. IONewItemKeyContainer cont
    => EditLens tedit (KeyEdit cont iedit)
    -> View sel tedit ()
tableNewItem tableLens =
    viewMapEdit tableLens $
    viewObjectPushEdit $ \_ push -> do
        item <- liftIO $ newKeyContainerItem @cont
        push [KeyInsertReplaceItem item]
