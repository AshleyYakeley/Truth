module Changes.Core.Lens.Delay where

import Changes.Core.Import
import Changes.Core.Lens.Lens

-- | Mostly for debugging
delayUpdateLens :: Int -> GenChangeLens lin update update
delayUpdateLens mus =
    MkChangeLens
        { clRead = \rd -> rd
        , clUpdate =
            \update _ -> do
                liftIO $ threadDelay mus
                return [update]
        , clPutEdits = \edits _ -> return $ Just edits
        }
