module Truth.Core.UI.CreateView
    ( CreateView
    , ViewState
    , vsFirstAspect
    , viewCreateView
    , cvLiftView
    , cvReceiveSubscriberUpdates
    , cvReceiveIOUpdates
    , cvReceiveUpdates
    , cvReceiveUpdate
    , cvBindUpdateFunction
    , cvAddAspect
    , cvMapSelection
    , cvNoAspect
    , cvAccessAspect
    , cvWithAspect
    , AnyCreateView(..)
    , runCreateView
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

newtype CreateView sel a = MkCreateView
    { unCreateView :: ReaderT (ViewContext sel) (WriterT (ViewOutput sel) LifeCycleIO) a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadTunnelIO, MonadFix, MonadUnliftIO)

type ViewState sel = LifeState IO (ViewOutput sel)

vsFirstAspect :: ViewState sel -> Aspect sel
vsFirstAspect (vo, _) = voFirstAspect vo

viewCreateView :: CreateView sel () -> View sel (ViewState sel)
viewCreateView (MkCreateView (ReaderT wff)) = MkView $ ReaderT $ \vc -> getLifeState $ fmap snd $ runWriterT $ wff vc

cvLiftView :: View sel a -> CreateView sel a
cvLiftView (MkView (ReaderT va)) = MkCreateView $ ReaderT $ \vc -> liftIO $ va vc

cvViewOutput :: ViewOutput sel -> CreateView sel ()
cvViewOutput vo = MkCreateView $ lift $ tell vo

instance MonadLifeCycleIO (CreateView sel) where
    liftLifeCycleIO lc = MkCreateView $ lift $ lift lc

instance MonadUnliftLifeCycleIO (CreateView sel) where
    liftLifeCycleIOWithUnlift call =
        MkCreateView $ liftWithUnlift $ \unliftR -> liftWithUnlift $ \unliftW -> call $ unliftW . unliftR . unCreateView

cvReceiveSubscriberUpdates ::
       Subscriber update
    -> (forall m. MonadIO m => MFunction (CreateView sel') m -> MutableRead m (UpdateReader update) -> m a)
    -> (a -> NonEmpty update -> EditSource -> IO ())
    -> CreateView sel' a
cvReceiveSubscriberUpdates sub fstCall recv = do
    -- monitor makes sure updates are ignored after the view has been closed
    monitor <- liftLifeCycleIO lifeCycleMonitor
    withUILock <- MkCreateView $ asks vcWithUILock
    runResource sub $ \run (asub :: _ tt) ->
        liftLifeCycleIOWithUnlift $ \unlift ->
            remonad run $ do
                a <-
                    fstCall @(LifeCycleT (ApplyStack tt IO)) (remonad (stackLift @tt) . unlift) $
                    remonadMutableRead lift $ subRead asub
                subscribe asub $ \edits MkEditContext {..} ->
                    withUILock $ do
                        alive <- monitor
                        if alive
                            then recv a edits editContextSource
                            else return ()
                return a

cvReceiveIOUpdates :: Subscriber update -> (NonEmpty update -> EditSource -> IO ()) -> CreateView sel ()
cvReceiveIOUpdates sub recv = cvReceiveSubscriberUpdates sub (\_ _ -> return ()) $ \_ -> recv

cvReceiveUpdates ::
       Subscriber update -> Maybe EditSource -> (WIOFunction (View sel) -> ReceiveUpdates update) -> CreateView sel ()
cvReceiveUpdates sub mesrc recv = do
    unliftIO <- cvLiftView $ liftIOView $ \unlift -> return $ MkWMFunction unlift
    cvReceiveIOUpdates sub $ \updates esrc ->
        if mesrc == Just esrc
            then return ()
            else runResource sub $ \run asub -> run $ recv unliftIO (subRead asub) updates

cvReceiveUpdate ::
       Subscriber update
    -> Maybe EditSource
    -> (WIOFunction (View sel) -> forall m. MonadUnliftIO m => MutableRead m (UpdateReader update) -> update -> m ())
    -> CreateView sel ()
cvReceiveUpdate sub mesrc recv = cvReceiveUpdates sub mesrc $ \unlift mr edits -> for_ edits (recv unlift mr)

cvBindUpdateFunction ::
       Maybe EditSource -> ReadOnlySubscriber (WholeUpdate t) -> (t -> View sel ()) -> CreateView sel ()
cvBindUpdateFunction mesrc sub setf =
    runResource sub $ \run asub -> do
        initial <- liftIO $ run $ subRead asub ReadWhole
        cvLiftView $ setf initial
        cvReceiveUpdates sub mesrc $ \(MkWMFunction unlift) _ wupdates ->
            case last wupdates of
                MkReadOnlyUpdate (MkWholeUpdate newval) -> liftIO $ unlift $ setf newval

cvAddAspect :: Aspect sel -> CreateView sel ()
cvAddAspect aspect = cvViewOutput $ mempty {voFirstAspect = aspect}

mapReaderContext :: (r2 -> r1) -> ReaderT r1 m a -> ReaderT r2 m a
mapReaderContext f (ReaderT rma) = ReaderT $ \r2 -> rma $ f r2

mapWriterOutput :: Functor m => (w1 -> w2) -> WriterT w1 m a -> WriterT w2 m a
mapWriterOutput f (WriterT maw) = WriterT $ fmap (\(a, w) -> (a, f w)) maw

cvAccessAspect :: ((Aspect sel -> IO ()) -> (Aspect sel -> IO ())) -> CreateView sel a -> CreateView sel (Aspect sel, a)
cvAccessAspect f (MkCreateView (ReaderT ma)) = do
    (a, vo) <- MkCreateView $ mapReaderContext (vcMapSetSelection f) $ ReaderT $ fmap listen ma
    return (voFirstAspect vo, a)

cvMapSelection ::
       forall sela selb a. ()
    => (sela -> selb)
    -> CreateView sela a
    -> CreateView selb a
cvMapSelection f (MkCreateView ma) =
    MkCreateView $ mapReaderContext (vcMapSelection f) $ remonad (mapWriterOutput $ voMapSelection f) ma

cvNoAspect :: CreateView sela a -> CreateView selb a
cvNoAspect (MkCreateView ma) = MkCreateView $ mapReaderContext vcNoAspect $ remonad (mapWriterOutput voNoAspect) ma

cvWithAspect :: (Aspect sel -> CreateView sel a) -> CreateView sel a
cvWithAspect f = do
    selref <- liftIO $ newIORef $ return Nothing
    let
        getsel :: Aspect _
        getsel = do
            asp <- liftIO $ readIORef selref
            asp
        updatesetsel :: (Aspect _ -> IO ()) -> (Aspect _ -> IO ())
        updatesetsel setsel asp = do
            writeIORef selref asp
            setsel asp
    (firstAspect, w) <- cvAccessAspect updatesetsel $ f getsel
    liftIO $ writeIORef selref firstAspect
    return w

data AnyCreateView w =
    forall sel. MkAnyCreateView (CreateView sel w)

runCreateView :: forall w. (IO () -> IO ()) -> AnyCreateView w -> (forall t. IOWitness t -> Maybe t) -> LifeCycleIO w
runCreateView vcWithUILock (MkAnyCreateView (MkCreateView (ReaderT view))) vcRequest = do
    let
        vcSetSelection :: Aspect sel -> IO ()
        vcSetSelection _ = return ()
    (w, _) <- runWriterT $ view MkViewContext {..}
    return w
