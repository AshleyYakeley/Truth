module Changes.World.GNOME.GTK.Widget.Layout
    ( Orientation (..)
    , LayoutOptions (..)
    , defaultLayoutOptions
    , createLayout
    )
where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

data LayoutOptions = MkLayoutOptions
    { loGrow :: Bool
    }

defaultLayoutOptions :: LayoutOptions
defaultLayoutOptions = let
    loGrow = False
    in MkLayoutOptions{..}

packLayout :: MonadIO m => GI.Box -> (LayoutOptions, GI.Widget) -> m ()
packLayout box (MkLayoutOptions{..}, widget) = #packStart box widget loGrow loGrow 0

createLayout :: GI.Orientation -> [(LayoutOptions, GI.Widget)] -> GView 'Unlocked GI.Widget
createLayout orientation contents =
    gvRunLocked $ do
        box <- gvNew GI.Box [#orientation GI.:= orientation]
        for_ contents $ packLayout box
        GI.toWidget box
