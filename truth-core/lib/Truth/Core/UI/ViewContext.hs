module Truth.Core.UI.ViewContext where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.UI.Specifier.Selection
import Truth.Core.UI.Specifier.Specifier

data ViewContext sel edit = MkViewContext
    { vcObject :: Object edit
    , vcSetSelection :: Aspect sel -> IO ()
    , vcRequest :: forall t. IOWitness t -> Maybe t
    }

vcMapEdit ::
       forall sel edita editb. ()
    => EditLens edita editb
    -> ViewContext sel edita
    -> ViewContext sel editb
vcMapEdit lens (MkViewContext objectA setSelect oG) = let
    objectB :: Object editb
    objectB = lensObject True lens objectA
    in MkViewContext objectB setSelect oG

vcMapSetSelection ::
       ((Aspect sela -> IO ()) -> (Aspect selb -> IO ())) -> ViewContext sela edit -> ViewContext selb edit
vcMapSetSelection f (MkViewContext object setSelectA oG) = MkViewContext object (f setSelectA) oG

vcMapSelection :: (sela -> selb) -> ViewContext selb edit -> ViewContext sela edit
vcMapSelection f = vcMapSetSelection $ \ss aspa -> ss $ mapSelectionAspect f aspa

vcNoAspect :: ViewContext selb edit -> ViewContext sela edit
vcNoAspect (MkViewContext object _ oG) = MkViewContext object (\_ -> return ()) oG
