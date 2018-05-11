module Truth.Core.UI.ViewContext where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.UI.Specifier.Lens
import Truth.Core.UI.Specifier.SelectionLens
import Truth.Core.UI.Specifier.Specifier

data ViewContext seledit edit = MkViewContext
    { vcObject :: Object edit
    , vcSetSelect :: Aspect seledit edit -> IO ()
    , vcOpenSelection :: IO ()
    , vcOpenWindow :: UIWindow edit -> IO ()
    , vcRequest :: forall t. IOWitness t -> Maybe t
    }

vcMapEdit ::
       forall seledit edita editb. ()
    => EditLens edita editb
    -> ViewContext seledit edita
    -> ViewContext seledit editb
vcMapEdit lens (MkViewContext objectA setSelectA os owA oG) = let
    objectB :: Object editb
    objectB = lensObject True lens objectA
    setSelectB selB = setSelectA $ aspectMapEdit lens selB
    owB window = owA $ uiWindowMapEdit lens window
    in MkViewContext objectB setSelectB os owB oG

vcMapSelectionEdit :: EditLens seledita seleditb -> ViewContext seleditb edit -> ViewContext seledita edit
vcMapSelectionEdit lens (MkViewContext object setSelectA os ow oG) = let
    setSelectB selB = setSelectA $ aspectMapSelectionEdit lens selB
    in MkViewContext object setSelectB os ow oG

vcNoAspect :: ViewContext seleditb edit -> ViewContext seledita edit
vcNoAspect (MkViewContext object _ os ow oG) = MkViewContext object (\_ -> return ()) os ow oG
