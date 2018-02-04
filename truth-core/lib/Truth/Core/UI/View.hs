module Truth.Core.UI.View
    ( View
    , liftIOView
    , viewObject
    , viewObjectRead
    , viewObjectMaybeEdit
    , viewObjectPushEdit
    , viewSetSelectedAspect
    , viewOpenSelection
    , mapViewEdit
    , ViewResult
    , vrWidget
    , vrUpdate
    , vrFirstAspect
    , mapViewResultEdit
    , CreateView
    , createViewReceiveUpdates
    , createViewReceiveUpdate
    , mapUpdates
    , createViewAddAspect
    , mapCreateViewEdit
    , mapCreateViewAspect
    , ViewSubscription(..)
    , subscribeView
    , tupleCreateView
    ) where

import Data.IORef
import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Object.Subscriber
import Truth.Core.Read
import Truth.Core.Types
import Truth.Core.UI.Lens
import Truth.Core.UI.Specifier
import Truth.Debug

data ViewResult edit w = MkViewResult
    { vrWidget :: w
    , vrUpdate :: forall m. MonadUnliftIO m =>
                                MutableRead m (EditReader edit) -> [edit] -> m ()
    , vrFirstAspect :: Aspect edit
    }

instance Functor (ViewResult edit) where
    fmap f (MkViewResult w update fss) = MkViewResult (f w) update fss

instance Applicative (ViewResult edit) where
    pure vrWidget = let
        vrUpdate _ _ = return ()
        vrFirstAspect = return Nothing
        in MkViewResult {..}
    (MkViewResult w1 update1 fss1) <*> (MkViewResult w2 update2 fss2) = let
        vrWidget = w1 w2
        vrUpdate ::
               forall m. MonadUnliftIO m
            => MutableRead m (EditReader edit)
            -> [edit]
            -> m ()
        vrUpdate mr edits = do
            update1 mr edits
            update2 mr edits
        vrFirstAspect = do
            ma1 <- fss1
            case ma1 of
                Just a -> return $ Just a
                Nothing -> fss2
        in MkViewResult {..}

instance Monad (ViewResult edit) where
    return = pure
    vr >>= f = vr *> (f $ vrWidget vr)

instance Foldable (ViewResult edit) where
    foldMap am vr = am $ vrWidget vr

instance Traversable (ViewResult edit) where
    sequenceA vrfa = fmap (\w -> MkViewResult w (vrUpdate vrfa) (vrFirstAspect vrfa)) $ vrWidget vrfa

mapViewResultEdit :: forall edita editb w. EditLens edita editb -> ViewResult editb w -> ViewResult edita w
mapViewResultEdit lens@(MkCloseUnlift unlift flens) (MkViewResult w updateB a) = let
    MkAnEditLens {..} = flens
    MkAnEditFunction {..} = elFunction
    updateA ::
           forall m. MonadUnliftIO m
        => MutableRead m (EditReader edita)
        -> [edita]
        -> m ()
    updateA mrA editsA =
        runUnlift (traceUnlift "mapViewResultEdit.updateA" unlift) $
        withTransConstraintTM @MonadUnliftIO $ do
            editsB <- efUpdates elFunction editsA mrA
            updateB (efGet mrA) editsB
    a' = mapAspect lens a
    in MkViewResult w updateA a'

data ViewContext edit = MkViewContext
    { vcObject :: Object edit
    , vcSetSelect :: Aspect edit -> IO ()
    , vcOpenSelection :: IO ()
    }

mapViewContextEdit ::
       forall edita editb. ()
    => EditLens edita editb
    -> ViewContext edita
    -> ViewContext editb
mapViewContextEdit lens (MkViewContext objectA setSelectA os) = let
    objectB :: Object editb
    objectB = mapObject lens objectA
    setSelectB selB = setSelectA $ mapAspect lens selB
    in MkViewContext objectB setSelectB os

newtype View edit a =
    MkView (ViewContext edit -> IO a)

instance Functor (View edit) where
    fmap f (MkView ma) =
        MkView $ \context -> do
            a <- ma context
            return $ f a

instance Applicative (View edit) where
    pure a = MkView $ \_context -> pure a
    (MkView mab) <*> (MkView ma) =
        MkView $ \context -> do
            ab <- mab context
            a <- ma context
            return $ ab a

instance Monad (View edit) where
    return = pure
    (MkView ma) >>= f =
        MkView $ \context -> do
            a <- ma context
            let MkView mb = f a
            mb context

instance MonadIO (View edit) where
    liftIO ioa = MkView $ \_context -> ioa

liftIOView :: forall edit a. ((forall r. View edit r -> IO r) -> IO a) -> View edit a
liftIOView call = MkView $ \context -> call $ \(MkView view) -> view context

viewObject :: View edit (Object edit)
viewObject = MkView $ \MkViewContext {..} -> return vcObject

viewObjectRead ::
       (forall m. MonadUnliftIO m =>
                      MutableRead m (EditReader edit) -> m r)
    -> View edit r
viewObjectRead call = do
    MkObject {..} <- viewObject
    liftIO $ runUnliftIO objRun $ call $ objRead

viewObjectMaybeEdit ::
       (forall m. MonadUnliftIO m =>
                      ([edit] -> m (Maybe (m ()))) -> m r)
    -> View edit r
viewObjectMaybeEdit call = do
    MkObject {..} <- viewObject
    liftIO $ runUnliftIO objRun $ call $ objEdit

viewObjectPushEdit ::
       (forall m. MonadUnliftIO m =>
                      ([edit] -> m ()) -> m r)
    -> View edit r
viewObjectPushEdit call = viewObjectMaybeEdit $ \push -> call $ \edits -> pushEdit $ push edits

viewSetSelectedAspect :: Aspect edit -> View edit ()
viewSetSelectedAspect aspect = MkView $ \MkViewContext {..} -> vcSetSelect aspect

viewOpenSelection :: View edit ()
viewOpenSelection = MkView $ \MkViewContext {..} -> vcOpenSelection

mapViewEdit ::
       forall edita editb a. ()
    => EditLens edita editb
    -> View editb a
    -> View edita a
mapViewEdit lens (MkView viewB) = MkView $ \contextA -> viewB $ mapViewContextEdit lens contextA

type CreateView edit = Compose (View edit) (ViewResult edit)

createViewReceiveUpdates ::
       (forall m. MonadUnliftIO m =>
                      MutableRead m (EditReader edit) -> [edit] -> m ())
    -> CreateView edit ()
createViewReceiveUpdates recv = liftInner $ (pure ()) {vrUpdate = recv}

createViewReceiveUpdate ::
       (forall m. MonadUnliftIO m =>
                      MutableRead m (EditReader edit) -> edit -> m ())
    -> CreateView edit ()
createViewReceiveUpdate recv = createViewReceiveUpdates $ \mr edits -> for_ edits (recv mr)

mapUpdates ::
       forall r m edita editb. MonadUnliftIO m
    => EditFunction edita editb
    -> MutableRead m (EditReader edita)
    -> [edita]
    -> (forall t. MonadTransUnlift t =>
                      MutableRead (t m) (EditReader editb) -> [editb] -> t m r)
    -> m r
mapUpdates (MkCloseUnlift (unlift :: Unlift t) ef@MkAnEditFunction {..}) mrA editsA call =
    runUnlift (traceUnlift "mapUpdates" unlift) $
    withTransConstraintTM @MonadIO $ do
        editsB <- efUpdates ef editsA mrA
        let
            mrB :: MutableRead (t m) (EditReader editb)
            mrB = efGet mrA
        call mrB editsB

createViewAddAspect :: Aspect edit -> CreateView edit ()
createViewAddAspect aspect = liftInner $ (pure ()) {vrFirstAspect = aspect}

mapCreateViewEdit ::
       forall edita editb a. ()
    => EditLens edita editb
    -> CreateView editb a
    -> CreateView edita a
mapCreateViewEdit lens (Compose wv) = Compose $ mapViewEdit lens $ fmap (mapViewResultEdit lens) wv

mapCreateViewAspect :: (Aspect edit -> Aspect edit) -> CreateView edit w -> CreateView edit w
mapCreateViewAspect f (Compose vw) =
    Compose $ do
        MkViewResult w v ag <- vw
        return $ MkViewResult w v $ f ag

data ViewSubscription edit action w = MkViewSubscription
    { srWidget :: w
    , srGetSelection :: Aspect edit
    , srCloser :: IO ()
    , srAction :: action
    }

instance Functor (ViewSubscription edit action) where
    fmap f (MkViewSubscription w gs cl aa) = MkViewSubscription (f w) gs cl aa

subscribeView ::
       forall edit w action. CreateView edit w -> Subscriber edit action -> IO () -> IO (ViewSubscription edit action w)
subscribeView (Compose (MkView (view :: ViewContext edit -> IO (ViewResult edit w)))) sub vcOpenSelection = do
    let
        initialise :: Object edit -> IO (ViewResult edit w, IORef (Aspect edit))
        initialise vcObject = do
            rec
                let vcSetSelect ss = writeIORef selref ss
                vr <- view MkViewContext {..}
                selref <- newIORef $ vrFirstAspect vr
            return (vr, selref)
        receive (vr, _) = vrUpdate vr
    ((MkViewResult {..}, selref), srCloser, srAction) <- subscribe sub initialise receive
    let
        srGetSelection = do
            ss <- readIORef selref
            ss
        srWidget = vrWidget
    return MkViewSubscription {..}

tupleCreateView ::
       (Applicative m, FiniteTupleSelector sel, TupleWitness Edit sel)
    => (forall edit. sel edit -> m (CreateView edit w))
    -> m (CreateView (TupleEdit sel) [w])
tupleCreateView pickview =
    getCompose $
    for tupleAllSelectors $ \(MkAnyWitness sel) ->
        case tupleWitness (Proxy :: Proxy Edit) sel of
            Dict -> Compose $ fmap (mapCreateViewEdit (tupleEditLens sel)) (pickview sel)
