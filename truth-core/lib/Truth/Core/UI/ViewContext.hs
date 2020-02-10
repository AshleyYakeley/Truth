module Truth.Core.UI.ViewContext where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

mapSelectionAspect :: (sela -> LifeCycleIO selb) -> Aspect sela -> Aspect selb
mapSelectionAspect f aspect = do
    msel <- aspect
    for msel f

data ViewContext sel = MkViewContext
    { vcSetSelection :: Aspect sel -> IO ()
    , vcRequest :: forall t. IOWitness t -> Maybe t
    , vcWithUILock :: IO () -> IO ()
    }

vcMapSetSelection :: ((Aspect sela -> IO ()) -> (Aspect selb -> IO ())) -> ViewContext sela -> ViewContext selb
vcMapSetSelection f (MkViewContext setSelectA oG tb) = MkViewContext (f setSelectA) oG tb

vcMapSelection :: (sela -> LifeCycleIO selb) -> ViewContext selb -> ViewContext sela
vcMapSelection f = vcMapSetSelection $ \ss aspa -> ss $ mapSelectionAspect f aspa

vcNoAspect :: ViewContext selb -> ViewContext sela
vcNoAspect (MkViewContext _ oG tb) = MkViewContext (\_ -> return ()) oG tb
