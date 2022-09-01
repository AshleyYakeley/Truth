module Pinafore
    ( module I
    , module Pinafore
    ) where

import Pinafore.Base as I
import Pinafore.Context as I
import Pinafore.Language as I
import Pinafore.Main as I
import Pinafore.Storage as I

runWithContext ::
       PinaforeContext -> FetchModule -> ((?pinafore :: PinaforeContext, ?library :: LibraryContext) => a) -> a
runWithContext context fetchModule call = let
    ?pinafore = context
    in let
           ?library = mkLibraryContext fetchModule
           in call
