module Truth.Core.UI.ViewContext where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.UI.Specifier.SelectionLens
import Truth.Core.UI.Specifier.Specifier

data ViewContext sel edit = MkViewContext
    { vcObject :: Object edit
    , vcSetSelection :: Aspect sel -> IO ()
    , vcOpenSelection :: IO ()
    , vcRequest :: forall t. IOWitness t -> Maybe t
    }

vcMapEdit ::
       forall sel edita editb. ()
    => EditLens edita editb
    -> ViewContext sel edita
    -> ViewContext sel editb
vcMapEdit lens (MkViewContext objectA setSelect os oG) = let
    objectB :: Object editb
    objectB = lensObject True lens objectA
    in MkViewContext objectB setSelect os oG

vcMapSetSelection :: (sela -> selb) -> ViewContext selb edit -> ViewContext sela edit
vcMapSetSelection f (MkViewContext object setSelectA os oG) = let
    setSelectB selB = setSelectA $ aspectMapSelection f selB
    in MkViewContext object setSelectB os oG

vcNoAspect :: ViewContext selb edit -> ViewContext sela edit
vcNoAspect (MkViewContext object _ os oG) = MkViewContext object (\_ -> return ()) os oG
