module Truth.Core.UI.ViewContext where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.UI.Specifier.Lens
import Truth.Core.UI.Specifier.Specifier

data ViewContext edit = MkViewContext
    { vcObject :: Object edit
    , vcSetSelect :: Aspect edit -> IO ()
    , vcOpenSelection :: IO ()
    , vcOpenWindow :: UIWindow edit -> IO ()
    , vcRequest :: forall t. IOWitness t -> Maybe t
    }

mapViewContextEdit ::
       forall edita editb. ()
    => EditLens edita editb
    -> ViewContext edita
    -> ViewContext editb
mapViewContextEdit lens (MkViewContext objectA setSelectA os owA oGA) = let
    objectB :: Object editb
    objectB = lensObject True lens objectA
    setSelectB selB = setSelectA $ mapAspect lens selB
    owB window = owA $ mapUIWindow lens window
    in MkViewContext objectB setSelectB os owB oGA
