module Pinafore.Packages (appPackages) where

import Pinafore.API
import Pinafore.Library.GNOME qualified
import Pinafore.Library.Media qualified
import Pinafore.Library.Script qualified
import Pinafore.Main qualified
import Shapes

appPackages :: Packages
appPackages =
    mconcat
        [ Pinafore.Main.pinaforePackages
        , Pinafore.Library.Media.mediaPackages
        , Pinafore.Library.GNOME.gnomePackages
        , Pinafore.Library.Script.scriptPackages
        ]
