module Pinafore
    ( module I
    , module Pinafore
    ) where

import Pinafore.Base as I
import Pinafore.Context as I
import Pinafore.Language as I
import Pinafore.Main as I
import Pinafore.Storage as I
import Shapes

runWithContext ::
       MonadIO m
    => PinaforeContext
    -> FetchModule
    -> ((?pinafore :: PinaforeContext, ?library :: LibraryContext) => m a)
    -> m a
runWithContext context fetchModule call = do
    let ?pinafore = context
    lc <- liftIO $ mkLibraryContext fetchModule
    let ?library = lc
    call
