module Pinafore.Library.GNOME
    ( gnomeLibrary
    , LangFile
    , LangContext(..)
    ) where

import Pinafore.API
import Pinafore.Library.GIO
import Pinafore.Library.GTK
import Pinafore.Library.WebKit
import Shapes

gnomeLibrary :: [LibraryModule]
gnomeLibrary = pure $ MkLibraryModule "gnome" $ mconcat $ [gioStuff] <> allGTKStuff <> [webKitStuff]
