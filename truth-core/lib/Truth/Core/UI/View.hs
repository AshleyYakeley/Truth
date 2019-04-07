module Truth.Core.UI.View
    ( View(..)
    , liftIOView
    , viewObject
    , viewObjectRead
    , viewObjectMaybeEdit
    , viewObjectPushEdit
    , viewSetSelection
    , viewRequest
    , viewMapEdit
    , viewMapSetSelectionEdit
    , viewNoAspect
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.ViewContext

newtype View sel edit a =
    MkView (ReaderT (ViewContext sel edit) IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadTunnelIO, MonadFix, MonadUnliftIO, MonadAskUnliftIO)

liftIOView :: forall sel edit a. ((forall r. View sel edit r -> IO r) -> IO a) -> View sel edit a
liftIOView call = liftIOWithUnlift $ \(MkTransform unlift) -> call unlift

viewObject :: View sel edit (Object edit)
viewObject = MkView $ asks vcObject

viewObjectRead ::
       (UnliftIO (View sel edit) -> forall m. MonadUnliftIO m => MutableRead m (EditReader edit) -> m r)
    -> View sel edit r
viewObjectRead call = do
    unliftIO <- askUnliftIO
    MkObject {..} <- viewObject
    liftIO $ runTransform objRun $ call unliftIO $ objRead

viewObjectMaybeEdit ::
       (UnliftIO (View sel edit) -> forall m. MonadUnliftIO m => ([edit] -> m (Maybe (EditSource -> m ()))) -> m r)
    -> View sel edit r
viewObjectMaybeEdit call = do
    unliftIO <- askUnliftIO
    MkObject {..} <- viewObject
    liftIO $ runTransform objRun $ call unliftIO $ objEdit

viewObjectPushEdit ::
       (UnliftIO (View sel edit) -> forall m. MonadUnliftIO m => (EditSource -> [edit] -> m Bool) -> m r)
    -> View sel edit r
viewObjectPushEdit call = viewObjectMaybeEdit $ \unlift push -> call unlift $ \esrc edits -> pushEdit esrc $ push edits

viewSetSelection :: Aspect sel -> View sel edit ()
viewSetSelection aspect = do
    setSelect <- MkView $ asks vcSetSelection
    liftIO $ setSelect aspect

viewRequest :: IOWitness t -> View sel edit (Maybe t)
viewRequest wit = MkView $ asks (\vc -> vcRequest vc wit)

viewMapEdit ::
       forall sel edita editb a. ()
    => EditLens edita editb
    -> View sel editb a
    -> View sel edita a
viewMapEdit lens (MkView viewB) = MkView $ withReaderT (vcMapEdit lens) viewB

viewMapSetSelectionEdit ::
       forall sela selb edit a. ()
    => (sela -> selb)
    -> View sela edit a
    -> View selb edit a
viewMapSetSelectionEdit f (MkView view) = MkView $ withReaderT (vcMapSelection f) view

viewNoAspect :: View sela edit a -> View selb edit a
viewNoAspect (MkView view) = MkView $ withReaderT vcNoAspect view
