module Changes.World.GNOME.GTK.Element.Layout
    ( Orientation(..)
    , LayoutOptions(..)
    , defaultLayoutOptions
    , createLayout
    ) where

import Changes.World.GNOME.GI
import GI.Gtk
import Shapes

data LayoutOptions = MkLayoutOptions
    { loGrow :: Bool
    }

defaultLayoutOptions :: LayoutOptions
defaultLayoutOptions = let
    loGrow = False
    in MkLayoutOptions {..}

packLayout :: MonadIO m => Box -> (LayoutOptions, Widget) -> m ()
packLayout box (MkLayoutOptions {..}, widget) = #packStart box widget loGrow loGrow 0

createLayout :: Orientation -> [(LayoutOptions, Widget)] -> GView 'Locked Widget
createLayout orientation contents = do
    box <- gvNew Box [#orientation := orientation]
    for_ contents $ packLayout box
    toWidget box
