module Pinafore.Library.Script (scriptPackages) where

import Pinafore.API
import Shapes
import Shapes.Unsafe

import Paths_pinafore_lib_script

scriptPackages :: Packages
scriptPackages = includeDirsPackages $ pure $ unsafePerformIO getDataDir
