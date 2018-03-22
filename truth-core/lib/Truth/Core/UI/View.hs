module Truth.Core.UI.View
    ( View
    , liftIOView
    , viewObject
    , viewObjectRead
    , viewObjectMaybeEdit
    , viewObjectPushEdit
    , viewSetSelectedAspect
    , viewOpenSelection
    , viewOpenWindow
    , mapViewEdit
    , ViewResult
    , vrWidget
    , vrUpdate
    , vrFirstAspect
    , mapViewResultEdit
    , CreateView
    , ViewState
    , viewCreateView
    , cvLiftView
    , cvReceiveUpdates
    , cvReceiveUpdate
    , cvBindEditFunction
    , cvAddAspect
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
import Truth.Core.UI.Specifier.Lens
import Truth.Core.UI.Specifier.Specifier

data ViewResult edit w = MkViewResult
    { vrWidget :: w
    , vrUpdate :: ReceiveUpdates edit
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
        vrUpdate :: ReceiveUpdates edit
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
        runUnlift unlift $
        withTransConstraintTM @MonadUnliftIO $ do
            editsB <- efUpdates elFunction editsA mrA
            updateB (efGet mrA) editsB
    a' = mapAspect lens a
    in MkViewResult w updateA a'

data ViewContext edit = MkViewContext
    { vcObject :: Object edit
    , vcSetSelect :: Aspect edit -> IO ()
    , vcOpenSelection :: IO ()
    , vcOpenWindow :: UIWindow edit -> IO ()
    }

mapViewContextEdit ::
       forall edita editb. ()
    => EditLens edita editb
    -> ViewContext edita
    -> ViewContext editb
mapViewContextEdit lens (MkViewContext objectA setSelectA os owA) = let
    objectB :: Object editb
    objectB = lensObject True lens objectA
    setSelectB selB = setSelectA $ mapAspect lens selB
    owB window = owA $ mapUIWindow lens window
    in MkViewContext objectB setSelectB os owB

newtype View edit a =
    MkView (ReaderT (ViewContext edit) IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadTunnelIO, MonadFix, MonadUnliftIO)

liftIOView :: forall edit a. ((forall r. View edit r -> IO r) -> IO a) -> View edit a
liftIOView call = liftIOWithUnlift $ \(MkUnliftIO unlift) -> call unlift

viewObject :: View edit (Object edit)
viewObject = MkView $ asks vcObject

viewObjectRead ::
       (UnliftIO (View edit) -> forall m. MonadUnliftIO m =>
                                              MutableRead m (EditReader edit) -> m r)
    -> View edit r
viewObjectRead call = do
    unliftIO <- liftIOView $ \unlift -> return $ MkUnliftIO unlift
    MkObject {..} <- viewObject
    liftIO $ runUnliftIO objRun $ call unliftIO $ objRead

viewObjectMaybeEdit ::
       (UnliftIO (View edit) -> forall m. MonadUnliftIO m =>
                                              ([edit] -> m (Maybe (m ()))) -> m r)
    -> View edit r
viewObjectMaybeEdit call = do
    unliftIO <- liftIOView $ \unlift -> return $ MkUnliftIO unlift
    MkObject {..} <- viewObject
    liftIO $ runUnliftIO objRun $ call unliftIO $ objEdit

viewObjectPushEdit ::
       (UnliftIO (View edit) -> forall m. MonadUnliftIO m =>
                                              ([edit] -> m ()) -> m r)
    -> View edit r
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

mapViewEdit ::
       forall edita editb a. ()
    => EditLens edita editb
    -> View editb a
    -> View edita a
mapViewEdit lens (MkView viewB) = MkView $ withReaderT (mapViewContextEdit lens) viewB

newtype CreateView edit a =
    MkCreateView (Compose (ReaderT (ViewContext edit) LifeCycle) (ViewResult edit) a)
    deriving (Functor, Applicative, Monad, MonadIO)

type ViewState edit a = LifeState (ViewResult edit a)

viewCreateView :: CreateView edit a -> View edit (ViewState edit a)
viewCreateView (MkCreateView (Compose (ReaderT ff))) = MkView $ ReaderT $ \vc -> runLifeCycle $ ff vc

cvLiftView :: View edit a -> CreateView edit a
cvLiftView (MkView va) = MkCreateView $ liftOuter $ remonad liftIO va

cvLiftViewResult :: ViewResult edit a -> CreateView edit a
cvLiftViewResult vr = MkCreateView $ liftInner vr

instance MonadLifeCycle (CreateView edit) where
    liftLifeCycle lc = MkCreateView $ liftOuter $ lift lc

cvReceiveUpdates :: (UnliftIO (View edit) -> ReceiveUpdates edit) -> CreateView edit ()
cvReceiveUpdates recv = do
    unliftIO <- cvLiftView $ liftIOView $ \unlift -> return $ MkUnliftIO unlift
    cvLiftViewResult $ (pure ()) {vrUpdate = recv unliftIO}

cvReceiveUpdate ::
       (UnliftIO (View edit) -> forall m. MonadUnliftIO m =>
                                              MutableRead m (EditReader edit) -> edit -> m ())
    -> CreateView edit ()
cvReceiveUpdate recv = cvReceiveUpdates $ \unlift mr edits -> for_ edits (recv unlift mr)

cvBindEditFunction :: EditFunction edit (WholeEdit t) -> (t -> View edit ()) -> CreateView edit ()
cvBindEditFunction ef setf = do
    initial <- cvLiftView $ viewObjectRead $ \_ mr -> editFunctionRead ef mr ReadWhole
    cvLiftView $ setf initial
    cvReceiveUpdates $ \(MkUnliftIO unlift) ->
        mapReceiveUpdates ef $ \_ wedits ->
            case lastWholeEdit wedits of
                Just newval -> liftIO $ unlift $ setf newval
                Nothing -> return ()

cvAddAspect :: Aspect edit -> CreateView edit ()
cvAddAspect aspect = cvLiftViewResult $ (pure ()) {vrFirstAspect = aspect}

mapCreateViewEdit ::
       forall edita editb a. ()
    => EditLens edita editb
    -> CreateView editb a
    -> CreateView edita a
mapCreateViewEdit lens (MkCreateView (Compose rclva)) =
    MkCreateView $ Compose $ withReaderT (mapViewContextEdit lens) $ fmap (mapViewResultEdit lens) rclva

mapCreateViewAspect :: (Aspect edit -> Aspect edit) -> CreateView edit w -> CreateView edit w
mapCreateViewAspect f (MkCreateView (Compose vw)) =
    MkCreateView $
    Compose $ do
        MkViewResult w v ag <- vw
        return $ MkViewResult w v $ f ag

data ViewSubscription edit action w = MkViewSubscription
    { srWidget :: w
    , srGetSelection :: Aspect edit
    , srAction :: action
    }

instance Functor (ViewSubscription edit action) where
    fmap f (MkViewSubscription w gs aa) = MkViewSubscription (f w) gs aa

subscribeView ::
       forall edit w action.
       CreateView edit w
    -> Subscriber edit action
    -> IO ()
    -> (UIWindow edit -> IO ())
    -> LifeCycle (ViewSubscription edit action w)
subscribeView (MkCreateView (Compose (ReaderT (view :: ViewContext edit -> LifeCycle (ViewResult edit w))))) sub vcOpenSelection vcOpenWindow = do
    let
        initialise :: Object edit -> LifeCycle (ViewResult edit w, IORef (Aspect edit))
        initialise vcObject = do
            rec
                let vcSetSelect ss = liftIO $ writeIORef selref ss
                vr <- view MkViewContext {..}
                selref <- liftIO $ newIORef $ vrFirstAspect vr
            return (vr, selref)
        receive (vr, _) = vrUpdate vr
    ((MkViewResult {..}, selref), srAction) <- subscribeLifeCycle sub initialise receive
    let
        srGetSelection = do
            ss <- readIORef selref
            ss
        srWidget = vrWidget
    return MkViewSubscription {..}

tupleCreateView ::
       (Applicative m, FiniteTupleSelector sel, TupleWitness ApplicableEdit sel)
    => (forall edit. sel edit -> m (CreateView edit w))
    -> m (CreateView (TupleEdit sel) [w])
tupleCreateView pickview =
    getCompose $
    for tupleAllSelectors $ \(MkAnyWitness sel) ->
        case tupleWitness @ApplicableEdit sel of
            Dict -> Compose $ fmap (mapCreateViewEdit (tupleEditLens sel)) (pickview sel)
