module Pinafore.Language.Library.GNOME
    ( gnomeLibrary
    , File
    , LangContext(..)
    ) where

import Pinafore.Language.API
import Pinafore.Language.Library.GIO
import Pinafore.Language.Library.GTK
import Shapes

gnomeLibrary :: [LibraryModule]
gnomeLibrary = [gioLibraryModule] <> gtkLibrary