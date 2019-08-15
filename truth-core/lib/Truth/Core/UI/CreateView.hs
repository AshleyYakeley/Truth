module Truth.Core.UI.CreateView
    ( CreateView
    , ViewState
    , vsFirstAspect
    , viewCreateView
    , cvLiftView
    , cvReceiveIOUpdates
    , cvReceiveUpdates
    , cvReceiveUpdate
    , cvBindEditFunction
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
import Truth.Debug.Object

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

newtype CreateView sel edit a =
    MkCreateView (ReaderT (ViewContext sel edit) (WriterT (ViewOutput sel) LifeCycleIO) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadTunnelIO, MonadFix, MonadUnliftIO)

type ViewState sel = LifeState IO (ViewOutput sel)

vsFirstAspect :: ViewState sel -> Aspect sel
vsFirstAspect (vo, _) = voFirstAspect vo

viewCreateView :: CreateView sel edit () -> View sel edit (ViewState sel)
viewCreateView (MkCreateView (ReaderT wff)) = MkView $ ReaderT $ \vc -> getLifeState $ fmap snd $ runWriterT $ wff vc

cvLiftView :: View sel edit a -> CreateView sel edit a
cvLiftView (MkView (ReaderT va)) = MkCreateView $ ReaderT $ \vc -> liftIO $ va vc

cvViewOutput :: ViewOutput sel -> CreateView sel edit ()
cvViewOutput vo = MkCreateView $ lift $ tell vo

instance MonadLifeCycleIO (CreateView sel edit) where
    liftLifeCycleIO lc = MkCreateView $ lift $ lift lc

cvReceiveIOUpdates :: (Object edit -> [edit] -> EditSource -> IO ()) -> CreateView sel edit ()
cvReceiveIOUpdates recv = do
    -- monitor makes sure updates are ignored after the view has been closed
    monitor <- liftLifeCycleIO lifeCycleMonitor
    withUILock <- MkCreateView $ asks $ vcWithUILock
    MkCloseUnliftIO run asub <- MkCreateView $ asks $ vcSubscriber
    liftLifeCycleIO $
        remonad (runTransform run) $
        subscribe asub $ \edits MkEditContext {..} ->
            traceBarrier "cvReceiveIOUpdates:update" (withUILock editContextTiming) $ do
                alive <- monitor
                if alive
                    then recv (MkCloseUnliftIO run $ subAnObject asub) edits editContextSource
                    else return ()

cvReceiveUpdates :: Maybe EditSource -> (UnliftIO (View sel edit) -> ReceiveUpdates edit) -> CreateView sel edit ()
cvReceiveUpdates mesrc recv = do
    unliftIO <- cvLiftView $ liftIOView $ \unlift -> return $ MkTransform unlift
    cvReceiveIOUpdates $ \(MkCloseUnliftIO unliftObj (MkAnObject mr _)) edits esrc ->
        if mesrc == Just esrc
            then return ()
            else runTransform unliftObj $ recv unliftIO mr edits

cvReceiveUpdate ::
       Maybe EditSource
    -> (UnliftIO (View sel edit) -> forall m. MonadUnliftIO m => MutableRead m (EditReader edit) -> edit -> m ())
    -> CreateView sel edit ()
cvReceiveUpdate mesrc recv = cvReceiveUpdates mesrc $ \unlift mr edits -> for_ edits (recv unlift mr)

cvBindEditFunction ::
       Maybe EditSource -> EditFunction edit (WholeEdit t) -> (t -> View sel edit ()) -> CreateView sel edit ()
cvBindEditFunction mesrc ef setf = do
    initial <- traceBracket "cvBindEditFunction:initial" $ cvLiftView $ viewObjectRead $ \_ mr -> traceBracket "cvBindEditFunction:editFunctionRead" $ editFunctionRead ef mr ReadWhole
    cvLiftView $ setf initial
    cvReceiveUpdates mesrc $ \(MkTransform unlift) ->
        mapReceiveUpdates ef $ \_ wedits ->
            case lastWholeEdit wedits of
                Just newval -> traceBracket "cvBindEditFunction:receive:val" $ liftIO $ unlift $ setf newval
                Nothing -> traceBracket "cvBindEditFunction:receive:nothing" $ return ()

cvAddAspect :: Aspect sel -> CreateView sel edit ()
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
cvMapEdit lens (MkCreateView ma) = MkCreateView $ mapReaderContext (vcMapEdit lens) ma

cvAccessAspect ::
       ((Aspect sel -> IO ()) -> (Aspect sel -> IO ())) -> CreateView sel edit a -> CreateView sel edit (Aspect sel, a)
cvAccessAspect f (MkCreateView (ReaderT ma)) = do
    (a, vo) <- MkCreateView $ mapReaderContext (vcMapSetSelection f) $ ReaderT $ fmap listen ma
    return (voFirstAspect vo, a)

cvMapSelection ::
       forall sela selb edit a. ()
    => (sela -> selb)
    -> CreateView sela edit a
    -> CreateView selb edit a
cvMapSelection f (MkCreateView ma) =
    MkCreateView $ mapReaderContext (vcMapSelection f) $ remonad (mapWriterOutput $ voMapSelection f) ma

cvNoAspect :: CreateView sela edit a -> CreateView selb edit a
cvNoAspect (MkCreateView ma) = MkCreateView $ mapReaderContext vcNoAspect $ remonad (mapWriterOutput voNoAspect) ma

cvWithAspect :: (Aspect sel -> CreateView sel edit a) -> CreateView sel edit a
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

data AnyCreateView edit w =
    forall sel. MkAnyCreateView (CreateView sel edit w)

subscribeView ::
       forall edit w.
       (UpdateTiming -> IO () -> IO ())
    -> AnyCreateView edit w
    -> Subscriber edit
    -> (forall t. IOWitness t -> Maybe t)
    -> LifeCycleIO w
subscribeView vcWithUILock (MkAnyCreateView (MkCreateView (ReaderT view))) vcSubscriber vcRequest = do
    let
        vcSetSelection :: Aspect sel -> IO ()
        vcSetSelection _ = return ()
    (w, _) <- runWriterT $ view MkViewContext {..}
    return w

tupleCreateView ::
       (Applicative m, FiniteTupleSelector s, TupleWitness ApplicableEdit s)
    => (forall edit. s edit -> m (CreateView sel edit w))
    -> m (CreateView sel (TupleEdit s) [w])
tupleCreateView pickview =
    fmap sequence $
    for tupleAllSelectors $ \(MkAnyW sel) ->
        case tupleWitness @ApplicableEdit sel of
            Dict -> fmap (cvMapEdit (tupleEditLens sel)) (pickview sel)
