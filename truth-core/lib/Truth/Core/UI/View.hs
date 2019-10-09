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
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.ViewContext

newtype View sel update a =
    MkView (ReaderT (ViewContext sel update) IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadTunnelIO, MonadFix, MonadUnliftIO, MonadAskUnliftIO)

liftIOView :: forall sel update a. ((forall r. View sel update r -> IO r) -> IO a) -> View sel update a
liftIOView = liftIOWithUnlift

viewObject :: View sel update (Object (UpdateEdit update))
viewObject = MkView $ asks $ subscriberObject . vcSubscriber

viewObjectRead ::
       (WIOFunction (View sel update) -> forall m. MonadUnliftIO m => MutableRead m (UpdateReader update) -> m r)
    -> View sel update r
viewObjectRead call = do
    unliftIO <- askUnliftIO
    MkCloseUnliftIO objRun MkAnObject {..} <- viewObject
    liftIO $ runWMFunction objRun $ call unliftIO $ objRead

viewObjectMaybeEdit ::
       (WIOFunction (View sel update) -> forall m.
                                             MonadUnliftIO m =>
                                                     ([UpdateEdit update] -> m (Maybe (EditSource -> m ()))) -> m r)
    -> View sel update r
viewObjectMaybeEdit call = do
    unliftIO <- askUnliftIO
    MkCloseUnliftIO objRun MkAnObject {..} <- viewObject
    liftIO $ runWMFunction objRun $ call unliftIO $ objEdit

viewObjectPushEdit ::
       (WIOFunction (View sel update) -> forall m.
                                             MonadUnliftIO m => (EditSource -> [UpdateEdit update] -> m Bool) -> m r)
    -> View sel update r
viewObjectPushEdit call = viewObjectMaybeEdit $ \unlift push -> call unlift $ \esrc edits -> pushEdit esrc $ push edits

viewSetSelection :: Aspect sel -> View sel update ()
viewSetSelection aspect = do
    setSelect <- MkView $ asks vcSetSelection
    liftIO $ setSelect aspect

viewRequest :: IOWitness t -> View sel update (Maybe t)
viewRequest wit = MkView $ asks (\vc -> vcRequest vc wit)

viewMapSetSelectionEdit ::
       forall sela selb update a. ()
    => (sela -> selb)
    -> View sela update a
    -> View selb update a
viewMapSetSelectionEdit f (MkView view) = MkView $ withReaderT (vcMapSelection f) view

viewNoAspect :: View sela update a -> View selb update a
viewNoAspect (MkView view) = MkView $ withReaderT vcNoAspect view
