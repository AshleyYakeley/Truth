module Truth.Core.UI.Specifier.Table where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object
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

data TableUISpec sel tedit where
    MkTableUISpec
        :: forall cont o tedit iedit.
           (KeyContainer cont, FullSubjectReader (EditReader iedit), HasKeyReader cont (EditReader iedit))
        => [KeyColumn tedit (ContainerKey cont)]
        -> (o -> o -> Ordering)
        -> (ContainerKey cont -> EditFunction tedit (WholeEdit o))
        -> EditLens tedit (KeyEdit cont iedit)
        -> (ContainerKey cont -> IO ())
        -> TableUISpec (ContainerKey cont) tedit

tableUISpec ::
       forall cont o tedit iedit.
       (KeyContainer cont, FullSubjectReader (EditReader iedit), HasKeyReader cont (EditReader iedit))
    => [KeyColumn tedit (ContainerKey cont)]
    -> (o -> o -> Ordering)
    -> (ContainerKey cont -> EditFunction tedit (WholeEdit o))
    -> EditLens tedit (KeyEdit cont iedit)
    -> (ContainerKey cont -> IO ())
    -> UISpec (ContainerKey cont) tedit
tableUISpec cols order geto lens onDoubleClick = MkUISpec $ MkTableUISpec cols order geto lens onDoubleClick

instance Show (TableUISpec sel edit) where
    show (MkTableUISpec _ _ _ _ _) = "table"

instance UIType TableUISpec where
    uiWitness = $(iowitness [t|TableUISpec|])

tableNewItem ::
       forall tedit cont iedit sel. IONewItemKeyContainer cont
    => EditSource
    -> EditLens tedit (KeyEdit cont iedit)
    -> View sel tedit Bool
tableNewItem esrc tableLens =
    viewMapEdit tableLens $
    viewObjectPushEdit $ \_ push -> do
        item <- liftIO $ newKeyContainerItem @cont
        push esrc [KeyInsertReplaceItem item]
