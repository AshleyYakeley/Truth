module Truth.Core.UI.ViewContext where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

ioMapSelectionAspect :: LifeCycleIO (sela -> selb) -> Aspect sela -> Aspect selb
ioMapSelectionAspect iof aspect = do
    f <- iof
    msel <- aspect
    return $ do
        sel <- msel
        return $ f sel

mapSelectionAspect :: (sela -> selb) -> Aspect sela -> Aspect selb
mapSelectionAspect f = ioMapSelectionAspect $ return f

data ViewContext sel = MkViewContext
    { vcSetSelection :: Aspect sel -> IO ()
    , vcRequest :: forall t. IOWitness t -> Maybe t
    , vcWithUILock :: IO () -> IO ()
    }

vcMapSetSelection :: ((Aspect sela -> IO ()) -> (Aspect selb -> IO ())) -> ViewContext sela -> ViewContext selb
vcMapSetSelection f (MkViewContext setSelectA oG tb) = MkViewContext (f setSelectA) oG tb

vcMapSelection :: (sela -> selb) -> ViewContext selb -> ViewContext sela
vcMapSelection f = vcMapSetSelection $ \ss aspa -> ss $ mapSelectionAspect f aspa

vcNoAspect :: ViewContext selb -> ViewContext sela
vcNoAspect (MkViewContext _ oG tb) = MkViewContext (\_ -> return ()) oG tb
