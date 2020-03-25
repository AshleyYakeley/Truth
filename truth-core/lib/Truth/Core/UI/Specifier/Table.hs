module Truth.Core.UI.Specifier.Table where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Read
import Truth.Core.Types
import Truth.Core.UI.Specifier.Selection
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.TextStyle
import Truth.Core.UI.View.CreateView
import Truth.Core.UI.View.View

data TableCellProps = MkTableCellProps
    { tcStyle :: TextStyle
    }

plainTableCellProps :: TableCellProps
plainTableCellProps = let
    tcStyle = plainTextStyle
    in MkTableCellProps {..}

data KeyColumn update = MkKeyColumn
    { kcName :: Model (ROWUpdate Text)
    , kcContents :: Model update -> CreateView (Model (WholeUpdate Text), Model (ROWUpdate TableCellProps))
    }

readOnlyKeyColumn ::
       forall update.
       Model (ROWUpdate Text)
    -> (Model update -> CreateView (Model (ROWUpdate (Text, TableCellProps))))
    -> KeyColumn update
readOnlyKeyColumn kcName getter = let
    kcContents :: Model update -> CreateView (Model (WholeUpdate Text), Model (ROWUpdate TableCellProps))
    kcContents rowSub = do
        cellSub <- getter rowSub
        let
            textSub :: Model (WholeUpdate Text)
            textSub = mapModel (fromReadOnlyRejectingChangeLens . liftReadOnlyChangeLens (funcChangeLens fst)) cellSub
            propsSub :: Model (ROWUpdate TableCellProps)
            propsSub = mapModel (liftReadOnlyChangeLens $ funcChangeLens snd) cellSub
        return (textSub, propsSub)
    in MkKeyColumn {..}

data TableUISpec where
    MkTableUISpec
        :: forall seq update.
           ( IsSequence seq
           , Integral (Index seq)
           , IsUpdate update
           , ApplicableEdit (UpdateEdit update)
           , FullSubjectReader (UpdateReader update)
           , UpdateSubject update ~ Element seq
           )
        => [KeyColumn update]
        -> Model (OrderedListUpdate seq update)
        -> (Model update -> View ())
        -> SelectNotify (Model update)
        -> TableUISpec

tableUISpec ::
       forall seq update.
       ( IsSequence seq
       , Integral (Index seq)
       , IsUpdate update
       , ApplicableEdit (UpdateEdit update)
       , FullSubjectReader (UpdateReader update)
       , UpdateSubject update ~ Element seq
       )
    => [KeyColumn update]
    -> Model (OrderedListUpdate seq update)
    -> (Model update -> View ())
    -> SelectNotify (Model update)
    -> CVUISpec
tableUISpec cols rows onDoubleClick sel = mkCVUISpec $ MkTableUISpec cols rows onDoubleClick sel

instance Show TableUISpec where
    show (MkTableUISpec _ _ _ _) = "table"

instance UIType TableUISpec where
    uiWitness = $(iowitness [t|TableUISpec|])
