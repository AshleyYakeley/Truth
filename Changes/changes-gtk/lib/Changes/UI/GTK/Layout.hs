module Changes.UI.GTK.Layout
    ( Orientation(..)
    , LayoutOptions(..)
    , defaultLayoutOptions
    , createLayout
    ) where

import Changes.Core
import Changes.UI.GTK.Useful
import GI.Gtk
import Shapes
import Changes.Debug.Reference

data LayoutOptions = MkLayoutOptions
    { loGrow :: Bool
    }

defaultLayoutOptions :: LayoutOptions
defaultLayoutOptions = let
    loGrow = False
    in MkLayoutOptions {..}

packLayout :: MonadIO m => Box -> (LayoutOptions, Widget) -> m ()
packLayout box (MkLayoutOptions {..}, widget) = #packStart box widget loGrow loGrow 0

createLayout :: Orientation -> [(LayoutOptions, Widget)] -> CreateView Widget
createLayout orientation contents = traceBracket "GTK.Layout:create" $ do
    box <- cvNew Box [#orientation := orientation]
    for_ contents $ packLayout box
    toWidget box
