module Truth.Core.UI.View
    ( View(..)
    , liftIOView
    , viewObject
    , viewObjectRead
    , viewObjectMaybeEdit
    , viewObjectPushEdit
    , viewSetSelection
    , viewGetSelection
    , viewOpenSelection
    , viewOpenWindow
    , viewRequest
    , viewMapEdit
    , viewMapSetSelectionEdit
    , viewMapGetSelectionEdit
    , viewNoAspect
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.ViewContext
import Truth.Debug.Object

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
    liftIO $ runUnliftIO (traceThing "viewObjectRead:run" objRun) $ call unliftIO $ objRead

viewObjectMaybeEdit ::
       (UnliftIO (View seledit edit) -> forall m. MonadUnliftIO m => ([edit] -> m (Maybe (m ()))) -> m r)
    -> View seledit edit r
viewObjectMaybeEdit call = traceBracket "viewObjectPush" $ do
    unliftIO <- askUnliftIO
    MkObject {..} <- viewObject
    liftIO $ runUnliftIO objRun $ call unliftIO $ objEdit

viewObjectPushEdit ::
       (UnliftIO (View seledit edit) -> forall m. MonadUnliftIO m => ([edit] -> m ()) -> m r) -> View seledit edit r
viewObjectPushEdit call = viewObjectMaybeEdit $ \unlift push -> call unlift $ \edits -> pushEdit $ push edits

viewSetSelection :: Aspect seledit edit -> View seledit edit ()
viewSetSelection aspect = do
    setSelect <- MkView $ asks vcSetSelection
    liftIO $ setSelect aspect

viewGetSelection :: View seledit edit (Maybe (Object seledit))
viewGetSelection =
    MkView $ do
        sel <- asks vcGetSelection
        liftIO sel

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

viewMapSetSelectionEdit ::
       forall seledita seleditb edit a. ()
    => EditLens seledita seleditb
    -> View seledita edit a
    -> View seleditb edit a
viewMapSetSelectionEdit lens (MkView view) = MkView $ withReaderT (vcMapSetSelectionEdit lens) view

viewMapGetSelectionEdit ::
       forall seledita seleditb edit a. ()
    => EditLens seledita seleditb
    -> View seleditb edit a
    -> View seledita edit a
viewMapGetSelectionEdit lens (MkView view) = MkView $ withReaderT (vcMapGetSelectionEdit lens) view

viewNoAspect :: View seledita edit a -> View seleditb edit a
viewNoAspect (MkView view) = MkView $ withReaderT vcNoAspect view
