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
    , mapViewEdit
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.ViewContext

newtype View edit a =
    MkView (ReaderT (ViewContext edit) IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadTunnelIO, MonadFix, MonadUnliftIO, MonadAskUnliftIO)

liftIOView :: forall edit a. ((forall r. View edit r -> IO r) -> IO a) -> View edit a
liftIOView call = liftIOWithUnlift $ \(MkUnliftIO unlift) -> call unlift

viewObject :: View edit (Object edit)
viewObject = MkView $ asks vcObject

viewObjectRead ::
       (UnliftIO (View edit) -> forall m. MonadUnliftIO m => MutableRead m (EditReader edit) -> m r) -> View edit r
viewObjectRead call = do
    unliftIO <- askUnliftIO
    MkObject {..} <- viewObject
    liftIO $ runUnliftIO objRun $ call unliftIO $ objRead

viewObjectMaybeEdit ::
       (UnliftIO (View edit) -> forall m. MonadUnliftIO m => ([edit] -> m (Maybe (m ()))) -> m r) -> View edit r
viewObjectMaybeEdit call = do
    unliftIO <- askUnliftIO
    MkObject {..} <- viewObject
    liftIO $ runUnliftIO objRun $ call unliftIO $ objEdit

viewObjectPushEdit :: (UnliftIO (View edit) -> forall m. MonadUnliftIO m => ([edit] -> m ()) -> m r) -> View edit r
viewObjectPushEdit call = viewObjectMaybeEdit $ \unlift push -> call unlift $ \edits -> pushEdit $ push edits

viewSetSelectedAspect :: Aspect edit -> View edit ()
viewSetSelectedAspect aspect = do
    setSelect <- MkView $ asks vcSetSelect
    liftIO $ setSelect aspect

viewOpenSelection :: View edit ()
viewOpenSelection = do
    openSelection <- MkView $ asks vcOpenSelection
    liftIO openSelection

viewOpenWindow :: UIWindow edit -> View edit ()
viewOpenWindow window = do
    openWindow <- MkView $ asks vcOpenWindow
    liftIO $ openWindow window

viewRequest :: IOWitness t -> View edit (Maybe t)
viewRequest wit = MkView $ asks (\vc -> vcRequest vc wit)

mapViewEdit ::
       forall edita editb a. ()
    => EditLens edita editb
    -> View editb a
    -> View edita a
mapViewEdit lens (MkView viewB) = MkView $ withReaderT (mapViewContextEdit lens) viewB
