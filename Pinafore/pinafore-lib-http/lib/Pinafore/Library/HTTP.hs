module Pinafore.Library.HTTP
    ( httpLibrary
    ) where

import Pinafore.API
import Shapes

httpLibrary :: [LibraryModule ()]
httpLibrary = pure $ MkLibraryModule "http" $ mconcat []
