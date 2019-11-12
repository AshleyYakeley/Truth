module Truth.Core.UI.CreateView
    ( CreateView
    , ViewState
    , vsFirstAspect
    , viewCreateView
    , cvLiftView
    , cvReceiveIOUpdates
    , cvReceiveUpdates
    , cvReceiveUpdate
    , cvBindUpdateFunction
    , cvAddAspect
    , cvMapEdit
    , cvMapSelection
    , cvNoAspect
    , cvAccessAspect
    , cvWithAspect
    , AnyCreateView(..)
    , subscribeView
    , tupleCreateView
    ) where

import Data.IORef
import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Read
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.View
import Truth.Core.UI.ViewContext

data ViewOutput sel = MkViewOutput
    { voFirstAspect :: Aspect sel
    }

instance Semigroup (ViewOutput sel) where
    (MkViewOutput fss1) <> (MkViewOutput fss2) = let
        voFirstAspect = do
            ma1 <- fss1
            case ma1 of
                Just a -> return $ Just a
                Nothing -> fss2
        in MkViewOutput {..}

instance Monoid (ViewOutput sel) where
    mempty = let
        voFirstAspect = return Nothing
        in MkViewOutput {..}
    mappend = (<>)

voMapSelection :: forall sela selb. (sela -> selb) -> ViewOutput sela -> ViewOutput selb
voMapSelection f (MkViewOutput asp) = MkViewOutput $ mapSelectionAspect f asp

voNoAspect :: ViewOutput sela -> ViewOutput selb
voNoAspect (MkViewOutput _) = MkViewOutput noAspect

newtype CreateView sel update a =
    MkCreateView (ReaderT (ViewContext sel update) (WriterT (ViewOutput sel) LifeCycleIO) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadTunnelIO, MonadFix, MonadUnliftIO)

type ViewState sel = LifeState IO (ViewOutput sel)

vsFirstAspect :: ViewState sel -> Aspect sel
vsFirstAspect (vo, _) = voFirstAspect vo

viewCreateView :: CreateView sel update () -> View sel update (ViewState sel)
viewCreateView (MkCreateView (ReaderT wff)) = MkView $ ReaderT $ \vc -> getLifeState $ fmap snd $ runWriterT $ wff vc

cvLiftView :: View sel update a -> CreateView sel update a
cvLiftView (MkView (ReaderT va)) = MkCreateView $ ReaderT $ \vc -> liftIO $ va vc

cvViewOutput :: ViewOutput sel -> CreateView sel update ()
cvViewOutput vo = MkCreateView $ lift $ tell vo

instance MonadLifeCycleIO (CreateView sel update) where
    liftLifeCycleIO lc = MkCreateView $ lift $ lift lc

cvReceiveIOUpdates :: (Object (UpdateEdit update) -> [update] -> EditSource -> IO ()) -> CreateView sel update ()
cvReceiveIOUpdates recv = do
    -- monitor makes sure updates are ignored after the view has been closed
    monitor <- liftLifeCycleIO lifeCycleMonitor
    withUILock <- MkCreateView $ asks vcWithUILock
    MkRunnable1 trun asub <- MkCreateView $ asks vcSubscriber
    liftLifeCycleIO $
        runMonoTransStackRunner @IO trun $ \run ->
            remonad run $
            subscribe asub $ \edits MkEditContext {..} ->
                withUILock editContextTiming $ do
                    alive <- monitor
                    if alive
                        then recv (MkRunnable1 trun $ subAnObject asub) edits editContextSource
                        else return ()

cvReceiveUpdates ::
       Maybe EditSource -> (WIOFunction (View sel update) -> ReceiveUpdates update) -> CreateView sel update ()
cvReceiveUpdates mesrc recv = do
    unliftIO <- cvLiftView $ liftIOView $ \unlift -> return $ MkWMFunction unlift
    cvReceiveIOUpdates $ \(MkRunnable1 trun (MkAnObject mr _)) edits esrc ->
        if mesrc == Just esrc
            then return ()
            else runMonoTransStackRunner @IO trun $ \run -> run $ recv unliftIO mr edits

cvReceiveUpdate ::
       Maybe EditSource
    -> (WIOFunction (View sel update) -> forall m.
                                             MonadUnliftIO m => MutableRead m (UpdateReader update) -> update -> m ())
    -> CreateView sel update ()
cvReceiveUpdate mesrc recv = cvReceiveUpdates mesrc $ \unlift mr edits -> for_ edits (recv unlift mr)

cvBindUpdateFunction ::
       Maybe EditSource
    -> UpdateFunction update (WholeUpdate t)
    -> (t -> View sel update ())
    -> CreateView sel update ()
cvBindUpdateFunction mesrc ef setf = do
    initial <- cvLiftView $ viewObjectRead $ \_ mr -> updateFunctionRead ef mr ReadWhole
    cvLiftView $ setf initial
    cvReceiveUpdates mesrc $ \(MkWMFunction unlift) ->
        mapReceiveUpdates ef $ \_ wupdates ->
            case lastWholeUpdate wupdates of
                Just newval -> liftIO $ unlift $ setf newval
                Nothing -> return ()

cvAddAspect :: Aspect sel -> CreateView sel update ()
cvAddAspect aspect = cvViewOutput $ mempty {voFirstAspect = aspect}

mapReaderContext :: (r2 -> r1) -> ReaderT r1 m a -> ReaderT r2 m a
mapReaderContext f (ReaderT rma) = ReaderT $ \r2 -> rma $ f r2

mapWriterOutput :: Functor m => (w1 -> w2) -> WriterT w1 m a -> WriterT w2 m a
mapWriterOutput f (WriterT maw) = WriterT $ fmap (\(a, w) -> (a, f w)) maw

cvMapEdit ::
       forall sel edita editb a. ()
    => EditLens edita editb
    -> CreateView sel editb a
    -> CreateView sel edita a
cvMapEdit lens (MkCreateView (ReaderT ma)) =
    MkCreateView $
    ReaderT $ \vca -> do
        vcb <- lift $ vcMapEdit lens vca
        ma vcb

cvAccessAspect ::
       ((Aspect sel -> IO ()) -> (Aspect sel -> IO ()))
    -> CreateView sel update a
    -> CreateView sel update (Aspect sel, a)
cvAccessAspect f (MkCreateView (ReaderT ma)) = do
    (a, vo) <- MkCreateView $ mapReaderContext (vcMapSetSelection f) $ ReaderT $ fmap listen ma
    return (voFirstAspect vo, a)

cvMapSelection ::
       forall sela selb update a. ()
    => (sela -> selb)
    -> CreateView sela update a
    -> CreateView selb update a
cvMapSelection f (MkCreateView ma) =
    MkCreateView $ mapReaderContext (vcMapSelection f) $ remonad (mapWriterOutput $ voMapSelection f) ma

cvNoAspect :: CreateView sela update a -> CreateView selb update a
cvNoAspect (MkCreateView ma) = MkCreateView $ mapReaderContext vcNoAspect $ remonad (mapWriterOutput voNoAspect) ma

cvWithAspect :: (Aspect sel -> CreateView sel update a) -> CreateView sel update a
cvWithAspect f = do
    selref <- liftIO $ newIORef $ return Nothing
    let
        getsel :: Aspect _
        getsel = do
            asp <- readIORef selref
            asp
        updatesetsel :: (Aspect _ -> IO ()) -> (Aspect _ -> IO ())
        updatesetsel setsel asp = do
            writeIORef selref asp
            setsel asp
    (firstAspect, w) <- cvAccessAspect updatesetsel $ f getsel
    liftIO $ writeIORef selref firstAspect
    return w

data AnyCreateView update w =
    forall sel. MkAnyCreateView (CreateView sel update w)

subscribeView ::
       forall update w.
       (UpdateTiming -> IO () -> IO ())
    -> AnyCreateView update w
    -> Subscriber update
    -> (forall t. IOWitness t -> Maybe t)
    -> LifeCycleIO w
subscribeView vcWithUILock (MkAnyCreateView (MkCreateView (ReaderT view))) vcSubscriber vcRequest = do
    let
        vcSetSelection :: Aspect sel -> IO ()
        vcSetSelection _ = return ()
    (w, _) <- runWriterT $ view MkViewContext {..}
    return w

tupleCreateView ::
       (Applicative m, FiniteTupleSelector s)
    => (forall update. s update -> m (CreateView sel update w))
    -> m (CreateView sel (TupleUpdate s) [w])
tupleCreateView pickview =
    fmap sequence $ for tupleAllSelectors $ \(MkAnyW sel) -> fmap (cvMapEdit (tupleEditLens sel)) (pickview sel)
