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
    { kcName :: Subscriber (ROWUpdate Text)
    , kcContents :: Subscriber update -> CreateView ( Subscriber (WholeUpdate Text)
                                                    , Subscriber (ROWUpdate TableCellProps))
    }

readOnlyKeyColumn ::
       forall update.
       Subscriber (ROWUpdate Text)
    -> (Subscriber update -> CreateView (Subscriber (ROWUpdate (Text, TableCellProps))))
    -> KeyColumn update
readOnlyKeyColumn kcName getter = let
    kcContents :: Subscriber update -> CreateView (Subscriber (WholeUpdate Text), Subscriber (ROWUpdate TableCellProps))
    kcContents rowSub = do
        cellSub <- getter rowSub
        let
            textSub :: Subscriber (WholeUpdate Text)
            textSub = mapSubscriber (fromReadOnlyRejectingEditLens . liftReadOnlyEditLens (funcEditLens fst)) cellSub
            propsSub :: Subscriber (ROWUpdate TableCellProps)
            propsSub = mapSubscriber (liftReadOnlyEditLens $ funcEditLens snd) cellSub
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
        -> Subscriber (OrderedListUpdate seq update)
        -> (Subscriber update -> View ())
        -> SelectNotify (Subscriber update)
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
    -> Subscriber (OrderedListUpdate seq update)
    -> (Subscriber update -> View ())
    -> SelectNotify (Subscriber update)
    -> CVUISpec
tableUISpec cols rows onDoubleClick sel = mkCVUISpec $ MkTableUISpec cols rows onDoubleClick sel

instance Show TableUISpec where
    show (MkTableUISpec _ _ _ _) = "table"

instance UIType TableUISpec where
    uiWitness = $(iowitness [t|TableUISpec|])
