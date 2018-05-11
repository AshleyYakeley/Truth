module Truth.Core.UI.View
    ( View(..)
    , liftIOView
    , viewObject
    , viewObjectRead
    , viewObjectMaybeEdit
    , viewObjectPushEdit
    , viewSetSelectedAspect
    , viewOpenSelection
    , viewOpenWindow
    , viewRequest
    , viewMapEdit
    , viewMapSelectionEdit
    , viewNoAspect
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.ViewContext

newtype View seledit edit a =
    MkView (ReaderT (ViewContext seledit edit) IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadTunnelIO, MonadFix, MonadUnliftIO, MonadAskUnliftIO)

liftIOView :: forall seledit edit a. ((forall r. View seledit edit r -> IO r) -> IO a) -> View seledit edit a
liftIOView call = liftIOWithUnlift $ \(MkUnliftIO unlift) -> call unlift

viewObject :: View seledit edit (Object edit)
viewObject = MkView $ asks vcObject

viewObjectRead ::
       (UnliftIO (View seledit edit) -> forall m. MonadUnliftIO m => MutableRead m (EditReader edit) -> m r)
    -> View seledit edit r
viewObjectRead call = do
    unliftIO <- askUnliftIO
    MkObject {..} <- viewObject
    liftIO $ runUnliftIO objRun $ call unliftIO $ objRead

viewObjectMaybeEdit ::
       (UnliftIO (View seledit edit) -> forall m. MonadUnliftIO m => ([edit] -> m (Maybe (m ()))) -> m r)
    -> View seledit edit r
viewObjectMaybeEdit call = do
    unliftIO <- askUnliftIO
    MkObject {..} <- viewObject
    liftIO $ runUnliftIO objRun $ call unliftIO $ objEdit

viewObjectPushEdit ::
       (UnliftIO (View seledit edit) -> forall m. MonadUnliftIO m => ([edit] -> m ()) -> m r) -> View seledit edit r
viewObjectPushEdit call = viewObjectMaybeEdit $ \unlift push -> call unlift $ \edits -> pushEdit $ push edits

viewSetSelectedAspect :: Aspect seledit edit -> View seledit edit ()
viewSetSelectedAspect aspect = do
    setSelect <- MkView $ asks vcSetSelect
    liftIO $ setSelect aspect

viewOpenSelection :: View seledit edit ()
viewOpenSelection = do
    openSelection <- MkView $ asks vcOpenSelection
    liftIO openSelection

viewOpenWindow :: UIWindow edit -> View seledit edit ()
viewOpenWindow window = do
    openWindow <- MkView $ asks vcOpenWindow
    liftIO $ openWindow window

viewRequest :: IOWitness t -> View seledit edit (Maybe t)
viewRequest wit = MkView $ asks (\vc -> vcRequest vc wit)

viewMapEdit ::
       forall seledit edita editb a. ()
    => EditLens edita editb
    -> View seledit editb a
    -> View seledit edita a
viewMapEdit lens (MkView viewB) = MkView $ withReaderT (vcMapEdit lens) viewB

viewMapSelectionEdit ::
       forall seledita seleditb edit a. ()
    => EditLens seledita seleditb
    -> View seledita edit a
    -> View seleditb edit a
viewMapSelectionEdit lens (MkView view) = MkView $ withReaderT (vcMapSelectionEdit lens) view

viewNoAspect :: View seledita edit a -> View seleditb edit a
viewNoAspect (MkView view) = MkView $ withReaderT vcNoAspect view
