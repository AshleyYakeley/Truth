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
    , viewRequest
    , mapViewEdit
    , mapViewResultEdit
    , CreateView
    , ViewState
    , vsUpdate
    , vsFirstAspect
    , viewCreateView
    , cvLiftView
    , cvReceiveUpdates
    , cvReceiveUpdate
    , cvBindEditFunction
    , cvAddAspect
    , mapCreateViewEdit
    , mapCreateViewAspect
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
import Truth.Debug.Object

data ViewOutput edit = MkViewOutput
    { voUpdate :: ReceiveUpdates edit
    , voFirstAspect :: Aspect edit
    }

instance Semigroup (ViewOutput edit) where
    (MkViewOutput update1 fss1) <> (MkViewOutput update2 fss2) = let
        voUpdate :: ReceiveUpdates edit
        voUpdate mr edits = do
            update1 mr edits
            update2 mr edits
        voFirstAspect = do
            ma1 <- fss1
            case ma1 of
                Just a -> return $ Just a
                Nothing -> fss2
        in MkViewOutput {..}

instance Monoid (ViewOutput edit) where
    mempty = let
        voUpdate _ _ = return ()
        voFirstAspect = return Nothing
        in MkViewOutput {..}
    mappend = (<>)

mapViewOutput :: forall edita editb. EditLens edita editb -> ViewOutput editb -> ViewOutput edita
mapViewOutput lens@(MkCloseUnlift unlift flens) (MkViewOutput updateB a) = let
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
    in (MkViewOutput updateA a')

type ViewResult edit a = (ViewOutput edit, a)

mapViewResultEdit :: EditLens edita editb -> ViewResult editb a -> ViewResult edita a
mapViewResultEdit lens (vo, a) = (mapViewOutput lens vo, a)

data ViewContext edit = MkViewContext
    { vcObject :: Object edit
    , vcSetSelect :: Aspect edit -> IO ()
    , vcOpenSelection :: IO ()
    , vcOpenWindow :: UIWindow edit -> IO ()
    , vcRequest :: forall t. IOWitness t -> Maybe t
    }

mapViewContextEdit ::
       forall edita editb. ()
    => EditLens edita editb
    -> ViewContext edita
    -> ViewContext editb
mapViewContextEdit lens (MkViewContext objectA setSelectA os owA oGA) = let
    objectB :: Object editb
    objectB = lensObject True lens objectA
    setSelectB selB = setSelectA $ mapAspect lens selB
    owB window = owA $ mapUIWindow lens window
    in MkViewContext objectB setSelectB os owB oGA

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

viewRequest :: IOWitness t -> View edit (Maybe t)
viewRequest wit = MkView $ asks (\vc -> vcRequest vc wit)

mapViewEdit ::
       forall edita editb a. ()
    => EditLens edita editb
    -> View editb a
    -> View edita a
mapViewEdit lens (MkView viewB) = MkView $ withReaderT (mapViewContextEdit lens) viewB

newtype CreateView edit a =
    MkCreateView (ReaderT (ViewContext edit) (WriterT (ViewOutput edit) LifeCycle) a)
    deriving (Functor, Applicative, Monad, MonadIO)

type ViewState edit a = LifeState (ViewResult edit a)

vsUpdate :: ViewState edit a -> ReceiveUpdates edit
vsUpdate ((vo, _), _) = voUpdate vo

vsFirstAspect :: ViewState edit a -> Aspect edit
vsFirstAspect ((vo, _), _) = voFirstAspect vo

viewCreateView :: CreateView edit a -> View edit (ViewState edit a)
viewCreateView (MkCreateView (ReaderT wff)) = MkView $ ReaderT $ \vc -> runLifeCycle $ fmap swap $ runWriterT $ wff vc

cvLiftView :: View edit a -> CreateView edit a
cvLiftView (MkView (ReaderT va)) = MkCreateView $ ReaderT $ \vc -> liftIO $ va vc

cvLiftViewResult :: ViewResult edit a -> CreateView edit a
cvLiftViewResult (vo, a) = MkCreateView $ lift $ WriterT $ return (a, vo)

instance MonadLifeCycle (CreateView edit) where
    liftLifeCycle lc = MkCreateView $ lift $ lift lc

cvReceiveUpdates :: (UnliftIO (View edit) -> ReceiveUpdates edit) -> CreateView edit ()
cvReceiveUpdates recv = do
    unliftIO <- cvLiftView $ liftIOView $ \unlift -> return $ MkUnliftIO unlift
    cvLiftViewResult $ (mempty {voUpdate = recv unliftIO}, ())

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
cvAddAspect aspect = cvLiftViewResult $ (mempty {voFirstAspect = aspect}, ())

mapReaderContext :: (r2 -> r1) -> ReaderT r1 m a -> ReaderT r2 m a
mapReaderContext f (ReaderT rma) = ReaderT $ \r2 -> rma $ f r2

mapWriterOutput :: Functor m => (w1 -> w2) -> WriterT w1 m a -> WriterT w2 m a
mapWriterOutput f (WriterT maw) = WriterT $ fmap (\(a, w) -> (a, f w)) maw

mapCreateViewEdit ::
       forall edita editb a. ()
    => EditLens edita editb
    -> CreateView editb a
    -> CreateView edita a
mapCreateViewEdit lens (MkCreateView ma) =
    MkCreateView $ mapReaderContext (mapViewContextEdit lens) $ remonad (mapWriterOutput $ mapViewOutput lens) ma

mapCreateViewAspect :: (Aspect edit -> Aspect edit) -> CreateView edit w -> CreateView edit w
mapCreateViewAspect f (MkCreateView mw) =
    MkCreateView $ remonad (mapWriterOutput (\(MkViewOutput v ag) -> MkViewOutput v $ f ag)) mw

subscribeView' ::
       forall edit w action.
       CreateView edit w
    -> Subscriber edit action
    -> (UIWindow edit -> IO ())
    -> (forall t. IOWitness t -> Maybe t)
    -> LifeCycle (w, action)
subscribeView' (MkCreateView (ReaderT (view :: ViewContext edit -> WriterT (ViewOutput edit) LifeCycle w))) sub vcOpenWindow vcRequest = do
    let
        initialise :: Object edit -> LifeCycle (ViewResult edit w, IORef (Aspect edit))
        initialise vcObject = do
            rec
                let
                    vcSetSelect ss = liftIO $ writeIORef selref ss
                    vcOpenSelection :: IO ()
                    vcOpenSelection = traceBracket "Selection button" $ do
                        ss <- readIORef selref
                        msel <- ss
                        case msel of
                            Just neww -> vcOpenWindow neww
                            Nothing -> traceIOM $ "no selection"
                (w, vo) <- runWriterT $ view MkViewContext {..}
                selref <- liftIO $ newIORef $ voFirstAspect vo
            return ((vo, w), selref)
        receive :: (ViewResult edit w, IORef (Aspect edit)) -> ReceiveUpdates edit
        receive ((vo, _), _) = voUpdate vo
    (((_, w), _), action) <- subscribeLifeCycle sub initialise receive
    return (w, action)

subscribeView ::
       forall edit w action.
       (IO () -> CreateView edit (action -> LifeCycle w))
    -> Subscriber edit action
    -> (UIWindow edit -> IO ())
    -> (forall t. IOWitness t -> Maybe t)
    -> IO w
subscribeView createView sub openWindow getRequest = do
    rec
        (w, closer) <-
            runLifeCycle $ do
                (followUp, action) <- subscribeView' (createView closer) sub openWindow getRequest
                followUp action
    return w

tupleCreateView ::
       (Applicative m, FiniteTupleSelector sel, TupleWitness ApplicableEdit sel)
    => (forall edit. sel edit -> m (CreateView edit w))
    -> m (CreateView (TupleEdit sel) [w])
tupleCreateView pickview =
    fmap sequence $
    for tupleAllSelectors $ \(MkAnyWitness sel) ->
        case tupleWitness @ApplicableEdit sel of
            Dict -> fmap (mapCreateViewEdit (tupleEditLens sel)) (pickview sel)
