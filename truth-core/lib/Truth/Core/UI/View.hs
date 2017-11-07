module Truth.Core.UI.View
    ( View
    , liftIOView
    , viewObject
    , viewMutableEdit
    , viewMutableRead
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
import Truth.Core.Object.MutableEdit
import Truth.Core.Object.Object
import Truth.Core.Object.Subscriber
import Truth.Core.Read
import Truth.Core.Types
import Truth.Core.UI.Lens
import Truth.Core.UI.Specifier

data ViewResult edit w = MkViewResult
    { vrWidget :: w
    , vrUpdate :: forall m. IsStateIO m =>
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
               forall m. IsStateIO m
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

mapViewResultEdit ::
       forall edita editb w. Edit editb
    => GeneralLens edita editb
    -> ViewResult editb w
    -> ViewResult edita w
mapViewResultEdit lens@(MkCloseState flens) (MkViewResult w updateB a) = let
    MkEditLens {..} = flens
    MkEditFunction {..} = editLensFunction
    updateA ::
           forall m. IsStateIO m
        => MutableRead m (EditReader edita)
        -> [edita]
        -> m ()
    updateA mrA editsA =
        editAccess $
        StateT $ \oldls -> do
            (newls, editsB) <- unReadable (editUpdates editLensFunction editsA oldls) mrA
            updateB (mapMutableRead (editGet oldls) mrA) editsB
            return ((), newls)
    a' = mapAspect lens a
    in MkViewResult w updateA a'

data ViewContext edit = MkViewContext
    { vcObject :: Object edit
    , vcSetSelect :: Aspect edit -> IO ()
    , vcOpenSelection :: IO ()
    }

mapViewContextEdit ::
       forall edita editb. (Edit edita, Edit editb)
    => GeneralLens edita editb
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

viewMutableEdit ::
       (forall m. IsStateIO m =>
                      MutableEdit m edit -> m r)
    -> View edit r
viewMutableEdit call = do
    object <- viewObject
    liftIO $ runObject object call

viewMutableRead ::
       (forall m. IsStateIO m =>
                      MutableRead m (EditReader edit) -> m r)
    -> View edit r
viewMutableRead call = viewMutableEdit $ \muted -> call $ mutableRead muted

viewSetSelectedAspect :: Aspect edit -> View edit ()
viewSetSelectedAspect aspect = MkView $ \MkViewContext {..} -> vcSetSelect aspect

viewOpenSelection :: View edit ()
viewOpenSelection = MkView $ \MkViewContext {..} -> vcOpenSelection

mapViewEdit ::
       forall edita editb a. (Edit edita, Edit editb)
    => GeneralLens edita editb
    -> View editb a
    -> View edita a
mapViewEdit lens (MkView viewB) = MkView $ \contextA -> viewB $ mapViewContextEdit lens contextA

type CreateView edit = Compose (View edit) (ViewResult edit)

createViewReceiveUpdates ::
       (forall m. IsStateIO m =>
                      MutableRead m (EditReader edit) -> [edit] -> m ())
    -> CreateView edit ()
createViewReceiveUpdates recv = liftInner $ (pure ()) {vrUpdate = recv}

createViewReceiveUpdate ::
       (forall m. IsStateIO m =>
                      MutableRead m (EditReader edit) -> edit -> m ())
    -> CreateView edit ()
createViewReceiveUpdate recv = createViewReceiveUpdates $ \mr edits -> for_ edits (recv mr)

mapUpdates ::
       forall r m edita editb. IsStateIO m
    => GeneralFunction edita editb
    -> MutableRead m (EditReader edita)
    -> [edita]
    -> (MutableRead m (EditReader editb) -> [editb] -> m r)
    -> m r
mapUpdates (MkCloseState ef@MkEditFunction {..}) mrA editsA call =
    editAccess $
    StateT $ \oldstate -> do
        (newstate, editsB) <- unReadable (editUpdates ef editsA oldstate) mrA
        let
            mrB :: MutableRead m (EditReader editb)
            mrB = mapMutableRead (editGet newstate) mrA
        r <- call mrB editsB
        return (r, newstate)

createViewAddAspect :: Aspect edit -> CreateView edit ()
createViewAddAspect aspect = liftInner $ (pure ()) {vrFirstAspect = aspect}

mapCreateViewEdit ::
       forall edita editb a. (Edit edita, Edit editb)
    => GeneralLens edita editb
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
            Dict -> Compose $ fmap (mapCreateViewEdit (tupleGeneralLens sel)) (pickview sel)
