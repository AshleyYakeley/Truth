module Truth.Core.UI.View
    ( View(..)
    , liftIOView
    , viewSetSelection
    , viewRequest
    , viewMapSetSelectionEdit
    , viewNoAspect
    ) where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.ViewContext

newtype View sel a =
    MkView (ReaderT (ViewContext sel) IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadTunnelIO, MonadFix, MonadUnliftIO, MonadAskUnliftIO)

liftIOView :: forall sel a. ((forall r. View sel r -> IO r) -> IO a) -> View sel a
liftIOView = liftIOWithUnlift

viewSetSelection :: Aspect sel -> View sel ()
viewSetSelection aspect = do
    setSelect <- MkView $ asks vcSetSelection
    liftIO $ setSelect aspect

viewRequest :: IOWitness t -> View sel (Maybe t)
viewRequest wit = MkView $ asks (\vc -> vcRequest vc wit)

viewMapSetSelectionEdit ::
       forall sela selb a. ()
    => (sela -> LifeCycleIO selb)
    -> View sela a
    -> View selb a
viewMapSetSelectionEdit f (MkView view) = MkView $ withReaderT (vcMapSelection f) view

viewNoAspect :: View sela a -> View selb a
viewNoAspect (MkView view) = MkView $ withReaderT vcNoAspect view
