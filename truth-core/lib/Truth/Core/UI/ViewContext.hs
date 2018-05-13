module Truth.Core.UI.ViewContext where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.UI.Specifier.Lens
import Truth.Core.UI.Specifier.SelectionLens
import Truth.Core.UI.Specifier.Specifier

data ViewContext seledit edit = MkViewContext
    { vcObject :: Object edit
    , vcSetSelection :: Aspect seledit edit -> IO ()
    , vcGetSelection :: IO (Maybe (Object seledit))
    , vcOpenSelection :: IO ()
    , vcOpenWindow :: UIWindow edit -> IO ()
    , vcRequest :: forall t. IOWitness t -> Maybe t
    }

vcMapEdit ::
       forall seledit edita editb. ()
    => EditLens edita editb
    -> ViewContext seledit edita
    -> ViewContext seledit editb
vcMapEdit lens (MkViewContext objectA setSelectA getSelect os owA oG) = let
    objectB :: Object editb
    objectB = lensObject True lens objectA
    setSelectB selB = setSelectA $ aspectMapEdit lens selB
    owB window = owA $ uiWindowMapEdit lens window
    in MkViewContext objectB setSelectB getSelect os owB oG

-- | The new context's get-selection will return Nothing
vcMapSetSelectionEdit :: EditLens seledita seleditb -> ViewContext seleditb edit -> ViewContext seledita edit
vcMapSetSelectionEdit lens (MkViewContext object setSelectA _ os ow oG) = let
    setSelectB selB = setSelectA $ aspectMapSelectionEdit lens selB
    in MkViewContext object setSelectB (return Nothing) os ow oG

-- | The new context's set-selection will do nothing
vcMapGetSelectionEdit :: EditLens seledita seleditb -> ViewContext seledita edit -> ViewContext seleditb edit
vcMapGetSelectionEdit lens (MkViewContext object _ getSelectA os ow oG) = let
    getSelectB = (fmap $ fmap $ mapObject lens) getSelectA
    in MkViewContext object (\_ -> return ()) getSelectB os ow oG

vcNoAspect :: ViewContext seleditb edit -> ViewContext seledita edit
vcNoAspect (MkViewContext object _ _ os ow oG) = MkViewContext object (\_ -> return ()) (return Nothing) os ow oG
