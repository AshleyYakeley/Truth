module Truth.Core.UI.Specifier.Table where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.TextStyle

data TableCellProps = MkTableCellProps
    { tcStyle :: TextStyle
    }

plainTableCellProps :: TableCellProps
plainTableCellProps = let
    tcStyle = plainTextStyle
    in MkTableCellProps {..}

data KeyColumn updateT key = MkKeyColumn
    { kcName :: UpdateFunction updateT (WholeUpdate Text)
    , kcContents :: key -> IO (EditLens updateT (WholeUpdate Text), UpdateFunction updateT (WholeUpdate TableCellProps))
    }

readOnlyKeyColumn ::
       UpdateFunction updateT (WholeUpdate Text)
    -> (key -> IO (UpdateFunction updateT (WholeUpdate (Text, TableCellProps))))
    -> KeyColumn updateT key
readOnlyKeyColumn kcName getter = let
    kcContents key = do
        func <- getter key
        return (readOnlyEditLens $ funcUpdateFunction fst . func, funcUpdateFunction snd . func)
    in MkKeyColumn {..}

data TableUISpec sel updateT where
    MkTableUISpec
        :: forall cont o updateT updateI.
           (KeyContainer cont, FullSubjectReader (UpdateReader updateI), HasKeyReader cont (UpdateReader updateI))
        => [KeyColumn updateT (ContainerKey cont)]
        -> (o -> o -> Ordering)
        -> (ContainerKey cont -> UpdateFunction updateT (WholeUpdate o))
        -> EditLens updateT (KeyUpdate cont updateI)
        -> (ContainerKey cont -> IO ())
        -> TableUISpec (ContainerKey cont) updateT

tableUISpec ::
       forall cont o updateT updateI.
       (KeyContainer cont, FullSubjectReader (UpdateReader updateI), HasKeyReader cont (UpdateReader updateI))
    => [KeyColumn updateT (ContainerKey cont)]
    -> (o -> o -> Ordering)
    -> (ContainerKey cont -> UpdateFunction updateT (WholeUpdate o))
    -> EditLens updateT (KeyUpdate cont updateI)
    -> (ContainerKey cont -> IO ())
    -> UISpec (ContainerKey cont) updateT
tableUISpec cols order geto lens onDoubleClick = MkUISpec $ MkTableUISpec cols order geto lens onDoubleClick

instance Show (TableUISpec sel edit) where
    show (MkTableUISpec _ _ _ _ _) = "table"

instance UIType TableUISpec where
    uiWitness = $(iowitness [t|TableUISpec|])
