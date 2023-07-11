module Pinafore.Language.Library.GNOME
    ( gnomeLibrary
    , LangFile
    , LangContext(..)
    ) where

import Pinafore.Language.API
import Pinafore.Language.Library.GIO
import Pinafore.Language.Library.GTK
import Shapes

gnomeLibrary :: [LibraryModule ()]
gnomeLibrary = pure $ MkLibraryModule "pinafore-gnome" $ headingBDS "pinafore-gnome" "" $ [gioStuff] <> allGTKStuff
