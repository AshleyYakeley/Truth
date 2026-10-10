module Pinafore.Language.Packages where

import Import
import Pinafore.Language.Library

data Packages = MkPackages
    { packageIncludeDirs :: [FilePath]
    , packageLibraryModules :: [LibraryModule]
    }

instance Semigroup Packages where
    MkPackages ia la <> MkPackages ib lb = MkPackages (ia <> ib) (la <> lb)

instance Monoid Packages where
    mempty = MkPackages mempty mempty

libraryModulePackages :: LibraryModule -> Packages
libraryModulePackages lm = mempty{packageLibraryModules = [lm]}

includeDirsPackages :: [FilePath] -> Packages
includeDirsPackages dirs = mempty{packageIncludeDirs = dirs}

pinaforePackages :: Packages
pinaforePackages = mempty{packageLibraryModules = pinaforeLibrary}
