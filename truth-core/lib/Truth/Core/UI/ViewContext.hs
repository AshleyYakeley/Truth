module Truth.Core.UI.ViewContext where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.UI.Specifier.Specifier

ioMapSelectionAspect :: IO (sela -> selb) -> Aspect sela -> Aspect selb
ioMapSelectionAspect iof aspect = do
    f <- iof
    msel <- aspect
    return $ do
        sel <- msel
        return $ f sel

mapSelectionAspect :: (sela -> selb) -> Aspect sela -> Aspect selb
mapSelectionAspect f = ioMapSelectionAspect $ return f

data ViewContext sel edit = MkViewContext
    { vcSubscriber :: Subscriber edit
    , vcSetSelection :: Aspect sel -> IO ()
    , vcRequest :: forall t. IOWitness t -> Maybe t
    , vcWithUILock :: UpdateTiming -> IO () -> IO ()
    }

vcMapEdit ::
       forall sel edita editb. ()
    => EditLens edita editb
    -> ViewContext sel edita
    -> ViewContext sel editb
vcMapEdit lens (MkViewContext subA setSelect oG tb) = let
    subB :: Subscriber editb
    subB = mapSubscriber lens subA
    in MkViewContext subB setSelect oG tb

vcMapSetSelection ::
       ((Aspect sela -> IO ()) -> (Aspect selb -> IO ())) -> ViewContext sela edit -> ViewContext selb edit
vcMapSetSelection f (MkViewContext sub setSelectA oG tb) = MkViewContext sub (f setSelectA) oG tb

vcMapSelection :: (sela -> selb) -> ViewContext selb edit -> ViewContext sela edit
vcMapSelection f = vcMapSetSelection $ \ss aspa -> ss $ mapSelectionAspect f aspa

vcNoAspect :: ViewContext selb edit -> ViewContext sela edit
vcNoAspect (MkViewContext sub _ oG tb) = MkViewContext sub (\_ -> return ()) oG tb
