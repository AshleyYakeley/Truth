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
    { kcName :: ReadOnlySubscriber (WholeUpdate Text)
    , kcContents :: key -> IO (Subscriber (WholeUpdate Text), ReadOnlySubscriber (WholeUpdate TableCellProps))
    }

readOnlyKeyColumn ::
       forall key.
       ReadOnlySubscriber (WholeUpdate Text)
    -> (key -> IO (ReadOnlySubscriber (WholeUpdate (Text, TableCellProps))))
    -> KeyColumn key
readOnlyKeyColumn kcName getter = let
    kcContents :: key -> IO (Subscriber (WholeUpdate Text), ReadOnlySubscriber (WholeUpdate TableCellProps))
    kcContents key = do
        func <- getter key
        return
            ( mapPureSubscriber
                  (updateFunctionToRejectingEditLens $ funcUpdateFunction fst . fromReadOnlyUpdateFunction)
                  func
            , mapReadOnlySubscriber (funcUpdateFunction snd) func)
    in MkKeyColumn {..}

data TableUISpec sel where
    MkTableUISpec
        :: forall cont o updateI.
           (KeyContainer cont, FullSubjectReader (UpdateReader updateI), HasKeyReader cont (UpdateReader updateI))
        => [KeyColumn (ContainerKey cont)]
        -> (o -> o -> Ordering)
        -> (ContainerKey cont -> ReadOnlySubscriber (WholeUpdate o))
        -> Subscriber (KeyUpdate cont updateI)
        -> (ContainerKey cont -> IO ())
        -> TableUISpec (ContainerKey cont)

tableUISpec ::
       forall cont o updateI.
       (KeyContainer cont, FullSubjectReader (UpdateReader updateI), HasKeyReader cont (UpdateReader updateI))
    => [KeyColumn (ContainerKey cont)]
    -> (o -> o -> Ordering)
    -> (ContainerKey cont -> ReadOnlySubscriber (WholeUpdate o))
    -> Subscriber (KeyUpdate cont updateI)
    -> (ContainerKey cont -> IO ())
    -> UISpec (ContainerKey cont)
tableUISpec cols order geto lens onDoubleClick = MkUISpec $ MkTableUISpec cols order geto lens onDoubleClick

instance Show (TableUISpec sel) where
    show (MkTableUISpec _ _ _ _ _) = "table"

instance UIType TableUISpec where
    uiWitness = $(iowitness [t|TableUISpec|])
