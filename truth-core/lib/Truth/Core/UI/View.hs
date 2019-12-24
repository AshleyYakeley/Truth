module Truth.Core.UI.View
    ( View(..)
    , liftIOView
    , viewObject
    , viewObjectRead
    , viewObjectMaybeEdit
    , viewObjectPushEdit
    , viewSetSelection
    , viewRequest
    , viewMapSetSelectionEdit
    , viewNoAspect
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Read
import Truth.Core.Resource
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.ViewContext

newtype View sel a =
    MkView (ReaderT (ViewContext sel) IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadTunnelIO, MonadFix, MonadUnliftIO, MonadAskUnliftIO)

liftIOView :: forall sel a. ((forall r. View sel r -> IO r) -> IO a) -> View sel a
liftIOView = liftIOWithUnlift

viewObject :: Subscriber update -> View sel (Object (UpdateEdit update))
viewObject sub = return $ subscriberObject sub

viewObjectRead ::
       Subscriber update
    -> (WIOFunction (View sel) -> forall m. MonadUnliftIO m => MutableRead m (UpdateReader update) -> m r)
    -> View sel r
viewObjectRead sub call = do
    unliftIO <- askUnliftIO
    MkResource rr MkAnObject {..} <- viewObject sub
    runResourceRunnerWith rr $ \run -> liftIO $ run $ call unliftIO $ objRead

viewObjectMaybeEdit ::
       Subscriber update
    -> (WIOFunction (View sel) -> forall m.
                                      MonadUnliftIO m =>
                                              (NonEmpty (UpdateEdit update) -> m (Maybe (EditSource -> m ()))) -> m r)
    -> View sel r
viewObjectMaybeEdit sub call = do
    unliftIO <- askUnliftIO
    MkResource rr MkAnObject {..} <- viewObject sub
    runResourceRunnerWith rr $ \run -> liftIO $ run $ call unliftIO $ objEdit

viewObjectPushEdit ::
       Subscriber update
    -> (WIOFunction (View sel) -> forall m.
                                      MonadUnliftIO m => (EditSource -> NonEmpty (UpdateEdit update) -> m Bool) -> m r)
    -> View sel r
viewObjectPushEdit sub call =
    viewObjectMaybeEdit sub $ \unlift push -> call unlift $ \esrc edits -> pushEdit esrc $ push edits

viewSetSelection :: Aspect sel -> View sel ()
viewSetSelection aspect = do
    setSelect <- MkView $ asks vcSetSelection
    liftIO $ setSelect aspect

viewRequest :: IOWitness t -> View sel (Maybe t)
viewRequest wit = MkView $ asks (\vc -> vcRequest vc wit)

viewMapSetSelectionEdit ::
       forall sela selb a. ()
    => (sela -> selb)
    -> View sela a
    -> View selb a
viewMapSetSelectionEdit f (MkView view) = MkView $ withReaderT (vcMapSelection f) view

viewNoAspect :: View sela a -> View selb a
viewNoAspect (MkView view) = MkView $ withReaderT vcNoAspect view
