module Truth.Core.UI.Specifier.Table where

--import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object

--import Truth.Core.Read
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

data KeyColumn update = MkKeyColumn
    { kcName :: OpenSubscriber (ROWUpdate Text)
    , kcContents :: OpenSubscriber update -> IO ( OpenSubscriber (WholeUpdate Text)
                                                , OpenSubscriber (ROWUpdate TableCellProps))
    }

readOnlyKeyColumn ::
       forall update.
       OpenSubscriber (ROWUpdate Text)
    -> (OpenSubscriber update -> IO (OpenSubscriber (ROWUpdate (Text, TableCellProps))))
    -> KeyColumn update
readOnlyKeyColumn kcName getter = let
    kcContents ::
           OpenSubscriber update -> IO (OpenSubscriber (WholeUpdate Text), OpenSubscriber (ROWUpdate TableCellProps))
    kcContents rowSub = do
        cellSub <- getter rowSub
        let
            textSub :: OpenSubscriber (WholeUpdate Text)
            textSub =
                mapOpenSubscriber (fromReadOnlyRejectingEditLens . liftReadOnlyEditLens (funcEditLens fst)) cellSub
            propsSub :: OpenSubscriber (ROWUpdate TableCellProps)
            propsSub = mapReadOnlyWholeOpenSubscriber snd cellSub
        return (textSub, propsSub)
    in MkKeyColumn {..}

data TableUISpec sel where
    MkTableUISpec
        :: forall seq update. ()
        => [KeyColumn update]
        -> OpenSubscriber (OrderedListUpdate seq update)
        -> (OpenSubscriber update -> IO ())
        -> TableUISpec (Subscriber update)

tableUISpec ::
       forall seq update. ()
    => [KeyColumn update]
    -> OpenSubscriber (OrderedListUpdate seq update)
    -> (OpenSubscriber update -> IO ())
    -> LUISpec (Subscriber update)
tableUISpec cols rows onDoubleClick = mkLUISpec $ MkTableUISpec cols rows onDoubleClick

instance Show (TableUISpec sel) where
    show (MkTableUISpec _ _ _) = "table"

instance UIType TableUISpec where
    uiWitness = $(iowitness [t|TableUISpec|])
