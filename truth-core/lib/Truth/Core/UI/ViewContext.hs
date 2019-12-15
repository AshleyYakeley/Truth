module Truth.Core.UI.ViewContext where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object
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

data ViewContext sel update = MkViewContext
    { vcSubscriber :: Subscriber update
    , vcSetSelection :: Aspect sel -> IO ()
    , vcRequest :: forall t. IOWitness t -> Maybe t
    , vcWithUILock :: IO () -> IO ()
    }

vcMapEdit ::
       forall sel edita editb. ()
    => LifeCycleIO (EditLens edita editb)
    -> ViewContext sel edita
    -> LifeCycleIO (ViewContext sel editb)
vcMapEdit getlens (MkViewContext subA setSelect oG tb) = do
    subB <- mapSubscriber getlens subA
    return $ MkViewContext subB setSelect oG tb

vcMapSetSelection ::
       ((Aspect sela -> IO ()) -> (Aspect selb -> IO ())) -> ViewContext sela update -> ViewContext selb update
vcMapSetSelection f (MkViewContext sub setSelectA oG tb) = MkViewContext sub (f setSelectA) oG tb

vcMapSelection :: (sela -> selb) -> ViewContext selb update -> ViewContext sela update
vcMapSelection f = vcMapSetSelection $ \ss aspa -> ss $ mapSelectionAspect f aspa

vcNoAspect :: ViewContext selb update -> ViewContext sela update
vcNoAspect (MkViewContext sub _ oG tb) = MkViewContext sub (\_ -> return ()) oG tb
