module Pinafore
    ( module I
    , module Pinafore
    ) where

import Pinafore.Base as I
import Pinafore.Context as I
import Pinafore.Language as I
import Pinafore.Main as I
import Pinafore.Storage as I

runWithContext :: QContext -> FetchModule -> ((?qcontext :: QContext, ?library :: LibraryContext) => a) -> a
runWithContext context fetchModule call = let
    ?qcontext = context
    in let
           ?library = mkLibraryContext fetchModule
           in call
