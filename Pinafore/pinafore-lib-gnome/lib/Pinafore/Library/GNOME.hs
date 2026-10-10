module Pinafore.Library.GNOME
    ( gnomePackages
    , LangFile
    , LangContext (..)
    , runLangContext
    )
where

import Pinafore.API
import Shapes

import Pinafore.Library.GIO
import Pinafore.Library.GTK
import Pinafore.Library.WebKit

gnomePackages :: Packages
gnomePackages = libraryModulePackages $ MkLibraryModule "gnome" $ mconcat $ [gioStuff] <> allGTKStuff <> [webKitStuff]
