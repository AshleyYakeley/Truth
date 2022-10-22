module Pinafore.Language.Library.GNOME
    ( gnomeLibrary
    , File
    , LangContext(..)
    ) where

import Pinafore.Language.API
import Pinafore.Language.Library.GIO
import Pinafore.Language.Library.GTK

gnomeLibrary :: [LibraryModule ()]
gnomeLibrary = gioLibraryModule : gtkLibrary
