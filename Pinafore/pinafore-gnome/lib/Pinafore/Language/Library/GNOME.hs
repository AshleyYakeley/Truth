module Pinafore.Language.Library.GNOME
    ( gnomeLibrary
    , LangFile
    , LangContext(..)
    ) where

import Pinafore.API
import Pinafore.Language.Library.GIO
import Pinafore.Language.Library.GTK
import Pinafore.Language.Library.WebKit
import Shapes

gnomeLibrary :: [LibraryModule ()]
gnomeLibrary = pure $ MkLibraryModule "pinafore-gnome" $ mconcat $ [gioStuff] <> allGTKStuff <> [webKitStuff]
