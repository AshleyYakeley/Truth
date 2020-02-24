module Truth.Core.UI.View.View
    ( View(..)
    , liftIOView
    , viewRunResource
    , viewRequest
    , viewWithContext
    , viewGetResourceContext
    , runView
    ) where

import Truth.Core.Import
import Truth.Core.Resource
import Truth.Core.UI.View.Context

newtype View a =
    MkView (ReaderT ViewContext IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadTunnelIO, MonadFix, MonadUnliftIO, MonadAskUnliftIO)

liftIOView :: forall a. ((forall r. View r -> IO r) -> IO a) -> View a
liftIOView = liftIOWithUnlift

viewRunResource ::
       forall f r.
       Resource f
    -> (forall tt. (MonadTransStackUnliftAll tt, MonadUnliftIO (ApplyStack tt IO)) => f tt -> ApplyStack tt IO r)
    -> View r
viewRunResource resource call =
    MkView $ do
        rc <- asks vcResourceContext
        lift $ runResource rc resource $ \ftt -> call ftt

viewRequest :: IOWitness t -> View (Maybe t)
viewRequest wit = MkView $ asks (\vc -> vcRequest vc wit)

viewWithContext :: (ViewContext -> ViewContext) -> View a -> View a
viewWithContext f (MkView ma) = MkView $ withReaderT f ma

viewGetResourceContext :: View ResourceContext
viewGetResourceContext = MkView $ asks vcResourceContext

runView :: forall w. ResourceContext -> (IO () -> IO ()) -> View w -> (forall t. IOWitness t -> Maybe t) -> IO w
runView vcResourceContext vcWithUILock (MkView (ReaderT view)) vcRequest = view MkViewContext {..}
