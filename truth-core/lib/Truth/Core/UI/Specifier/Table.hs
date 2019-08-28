module Truth.Core.UI.Specifier.Table where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data TableCellProps = MkTableCellProps
    { tcItalic :: Bool
    }

tableCellPlain :: TableCellProps
tableCellPlain = let
    tcItalic = False
    in MkTableCellProps {..}

data KeyColumn tedit key = MkKeyColumn
    { kcName :: UpdateFunction tedit (WholeEdit Text)
    , kcContents :: key -> IO (EditLens tedit (WholeEdit Text), UpdateFunction tedit (WholeEdit TableCellProps))
    }

readOnlyKeyColumn ::
       UpdateFunction tedit (WholeEdit Text)
    -> (key -> IO (UpdateFunction tedit (WholeEdit (Text, TableCellProps))))
    -> KeyColumn tedit key
readOnlyKeyColumn kcName getter = let
    kcContents key = do
        func <- getter key
        return (readOnlyEditLens $ funcUpdateFunction fst . func, funcUpdateFunction snd . func)
    in MkKeyColumn {..}

data TableUISpec sel tedit where
    MkTableUISpec
        :: forall cont o tedit iedit.
           (KeyContainer cont, FullSubjectReader (EditReader iedit), HasKeyReader cont (EditReader iedit))
        => [KeyColumn tedit (ContainerKey cont)]
        -> (o -> o -> Ordering)
        -> (ContainerKey cont -> UpdateFunction tedit (WholeEdit o))
        -> EditLens tedit (KeyEdit cont iedit)
        -> (ContainerKey cont -> IO ())
        -> TableUISpec (ContainerKey cont) tedit

tableUISpec ::
       forall cont o tedit iedit.
       (KeyContainer cont, FullSubjectReader (EditReader iedit), HasKeyReader cont (EditReader iedit))
    => [KeyColumn tedit (ContainerKey cont)]
    -> (o -> o -> Ordering)
    -> (ContainerKey cont -> UpdateFunction tedit (WholeEdit o))
    -> EditLens tedit (KeyEdit cont iedit)
    -> (ContainerKey cont -> IO ())
    -> UISpec (ContainerKey cont) tedit
tableUISpec cols order geto lens onDoubleClick = MkUISpec $ MkTableUISpec cols order geto lens onDoubleClick

instance Show (TableUISpec sel edit) where
    show (MkTableUISpec _ _ _ _ _) = "table"

instance UIType TableUISpec where
    uiWitness = $(iowitness [t|TableUISpec|])
