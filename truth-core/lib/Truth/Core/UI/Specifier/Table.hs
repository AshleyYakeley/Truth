module Truth.Core.UI.Specifier.Table where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object
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

data KeyColumn key = MkKeyColumn
    { kcName :: ReadOnlyOpenSubscriber (WholeUpdate Text)
    , kcContents :: key -> IO (OpenSubscriber (WholeUpdate Text), ReadOnlyOpenSubscriber (WholeUpdate TableCellProps))
    }

readOnlyKeyColumn ::
       forall key.
       ReadOnlyOpenSubscriber (WholeUpdate Text)
    -> (key -> IO (ReadOnlyOpenSubscriber (WholeUpdate (Text, TableCellProps))))
    -> KeyColumn key
readOnlyKeyColumn kcName getter = let
    kcContents :: key -> IO (OpenSubscriber (WholeUpdate Text), ReadOnlyOpenSubscriber (WholeUpdate TableCellProps))
    kcContents key = do
        pairSub <- getter key
        let
            textSub :: OpenSubscriber (WholeUpdate Text)
            textSub =
                mapOpenSubscriber (fromReadOnlyRejectingEditLens . liftReadOnlyEditLens (funcEditLens fst)) pairSub
            propsSub :: ReadOnlyOpenSubscriber (WholeUpdate TableCellProps)
            propsSub = mapReadOnlyWholeOpenSubscriber snd pairSub
        return (textSub, propsSub)
    in MkKeyColumn {..}

data TableUISpec sel where
    MkTableUISpec
        :: forall cont o updateI. (FullSubjectReader (UpdateReader updateI), HasKeyReader cont (UpdateReader updateI))
        => [KeyColumn (ContainerKey cont)]
        -> (o -> o -> Ordering)
        -> (ContainerKey cont -> ReadOnlyOpenSubscriber (WholeUpdate o))
        -> OpenSubscriber (KeyUpdate cont updateI)
        -> (ContainerKey cont -> IO ())
        -> TableUISpec (ContainerKey cont)

tableUISpec ::
       forall cont o updateI. (FullSubjectReader (UpdateReader updateI), HasKeyReader cont (UpdateReader updateI))
    => [KeyColumn (ContainerKey cont)]
    -> (o -> o -> Ordering)
    -> (ContainerKey cont -> ReadOnlyOpenSubscriber (WholeUpdate o))
    -> OpenSubscriber (KeyUpdate cont updateI)
    -> (ContainerKey cont -> IO ())
    -> LUISpec (ContainerKey cont)
tableUISpec cols order geto lens onDoubleClick = mkLUISpec $ MkTableUISpec cols order geto lens onDoubleClick

instance Show (TableUISpec sel) where
    show (MkTableUISpec _ _ _ _ _) = "table"

instance UIType TableUISpec where
    uiWitness = $(iowitness [t|TableUISpec|])
