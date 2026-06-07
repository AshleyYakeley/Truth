module Changes.World.GNOME.GTK.Widget.Table
    ( TableCellProps (..)
    , plainTableCellProps
    , KeyColumn (..)
    , readOnlyKeyColumn
    , createListTable
    )
where

import Changes.World.GNOME.GI
import Changes.World.GNOME.GTK.Widget.TextStyle
import Import
import Import.GI qualified as GI

data TableCellProps = MkTableCellProps
    { tcStyle :: TextStyle
    }

plainTableCellProps :: TableCellProps
plainTableCellProps = let
    tcStyle = plainTextStyle
    in MkTableCellProps{..}

data KeyColumn update = MkKeyColumn
    { kcName :: Model (ROWUpdate Text)
    , kcContents :: Model update -> GView 'Unlocked (Model (WholeUpdate Text), Model (ROWUpdate TableCellProps))
    }

readOnlyKeyColumn ::
    forall update.
    Model (ROWUpdate Text) ->
    (Model update -> GView 'Unlocked (Model (ROWUpdate (Text, TableCellProps)))) ->
    KeyColumn update
readOnlyKeyColumn kcName getter = let
    kcContents :: Model update -> GView 'Unlocked (Model (WholeUpdate Text), Model (ROWUpdate TableCellProps))
    kcContents rowSub = do
        cellSub <- getter rowSub
        let
            textSub :: Model (WholeUpdate Text)
            textSub = mapModel (fromReadOnlyRejectingChangeLens . liftReadOnlyChangeLens (funcChangeLens fst)) cellSub
            propsSub :: Model (ROWUpdate TableCellProps)
            propsSub = mapModel (liftReadOnlyChangeLens $ funcChangeLens snd) cellSub
        return (textSub, propsSub)
    in MkKeyColumn{..}

createListTable ::
    forall update.
    [KeyColumn update] ->
    Model (OrderedListUpdate update) ->
    (Model update -> GView 'Locked ()) ->
    SelectNotify (Model update) ->
    GView 'Unlocked (GI.Widget, Maybe (ReadM (UpdateReader update) Bool) -> GView 'Unlocked ())
createListTable _ _ _ _ = do
    widget <-
        gvRunLocked $ do
            box <- gvNew GI.Box [#orientation GI.:= GI.OrientationVertical]
            GI.toWidget box
    return (widget, const $ pure ())
