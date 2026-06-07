module Changes.World.GNOME.GTK.Widget.Option
    ( ComboBoxCell (..)
    , plainComboBoxCell
    , createComboBox
    )
where

import Changes.World.GNOME.GI
import Changes.World.GNOME.GTK.Widget.TextStyle
import Import
import Import.GI qualified as GI

data ComboBoxCell = MkComboBoxCell
    { cbcText :: Text
    , cbcStyle :: TextStyle
    , cbcDefault :: Bool
    }
    deriving stock Eq

plainComboBoxCell :: Text -> ComboBoxCell
plainComboBoxCell cbcText = let
    cbcStyle = plainTextStyle
    cbcDefault = False
    in MkComboBoxCell{..}

createComboBox ::
    forall update t.
    Model (ReadOnlyUpdate (OrderedListUpdate update)) ->
    Model (WholeUpdate t) ->
    GView 'Unlocked GI.Widget
createComboBox _ _ =
    gvRunLocked $ do
        box <- gvNew GI.Box [#orientation GI.:= GI.OrientationHorizontal]
        GI.toWidget box
