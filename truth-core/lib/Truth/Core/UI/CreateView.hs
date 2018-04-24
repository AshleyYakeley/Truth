module Truth.Core.UI.CreateView
    ( mapViewResultEdit
    , CreateView
    , ViewState
    , vsUpdate
    , vsFirstAspect
    , viewCreateView
    , cvLiftView
    , cvReceiveIOUpdates
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
import Truth.Core.UI.View
import Truth.Core.UI.ViewContext
import Truth.Debug.Object

data ViewOutput edit = MkViewOutput
    { voUpdate :: Object edit -> [edit] -> IO ()
    , voFirstAspect :: Aspect edit
    }

instance Semigroup (ViewOutput edit) where
    (MkViewOutput update1 fss1) <> (MkViewOutput update2 fss2) = let
        voUpdate :: Object edit -> [edit] -> IO ()
        voUpdate obj edits = do
            update1 obj edits
            update2 obj edits
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
    updateA :: Object edita -> [edita] -> IO ()
    updateA objA@(MkObject ou omr _) editsA = do
        editsB <-
            runUnliftIO (composeUnliftIO unlift ou) $
            withTransConstraintTM @MonadUnliftIO $ efUpdates elFunction editsA omr
        updateB (mapObject lens objA) editsB
    a' = mapAspect lens a
    in (MkViewOutput updateA a')

type ViewResult edit a = (ViewOutput edit, a)

mapViewResultEdit :: EditLens edita editb -> ViewResult editb a -> ViewResult edita a
mapViewResultEdit lens (vo, a) = (mapViewOutput lens vo, a)

newtype CreateView edit a =
    MkCreateView (ReaderT (ViewContext edit) (WriterT (ViewOutput edit) LifeCycle) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadTunnelIO, MonadFix, MonadUnliftIO)

type ViewState edit a = LifeState (ViewResult edit a)

vsUpdate :: ViewState edit a -> Object edit -> [edit] -> IO ()
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

cvReceiveIOUpdates :: (Object edit -> [edit] -> IO ()) -> CreateView edit ()
cvReceiveIOUpdates recv = do cvLiftViewResult $ (mempty {voUpdate = recv}, ())

cvReceiveUpdates :: (UnliftIO (View edit) -> ReceiveUpdates edit) -> CreateView edit ()
cvReceiveUpdates recv = do
    unliftIO <- cvLiftView $ liftIOView $ \unlift -> return $ MkUnliftIO unlift
    cvReceiveIOUpdates $ \(MkObject unliftObj mr _) edits -> runUnliftIO unliftObj $ recv unliftIO mr edits

cvReceiveUpdate ::
       (UnliftIO (View edit) -> forall m. MonadUnliftIO m => MutableRead m (EditReader edit) -> edit -> m ())
    -> CreateView edit ()
cvReceiveUpdate recv = cvReceiveUpdates $ \unlift mr edits -> for_ edits (recv unlift mr)

cvBindEditFunction :: EditFunction edit (WholeEdit t) -> (t -> View edit ()) -> CreateView edit ()
cvBindEditFunction ef setf = do
    initial <- traceBracket "cvBindEditFunction:initial" $ cvLiftView $ viewObjectRead $ \_ mr -> traceBracket "cvBindEditFunction:editFunctionRead" $ editFunctionRead ef mr ReadWhole
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
                            Nothing -> return ()
                (w, vo) <- runWriterT $ view MkViewContext {..}
                selref <- liftIO $ newIORef $ voFirstAspect vo
            return ((vo, w), selref)
        receive :: (ViewResult edit w, IORef (Aspect edit)) -> Object edit -> [edit] -> IO ()
        receive ((vo, _), _) = voUpdate vo
    (((_, w), _), _, action) <- subscribeLifeCycle sub initialise receive
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
