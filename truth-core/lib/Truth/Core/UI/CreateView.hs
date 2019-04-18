module Truth.Core.UI.CreateView
    ( vrMapEdit
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
import Truth.Core.UI.Specifier.Selection
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.View
import Truth.Core.UI.ViewContext

data ViewOutput sel edit = MkViewOutput
    { voUpdate :: Object edit -> [edit] -> EditContext -> IO ()
    , voFirstAspect :: Aspect sel
    }

instance Semigroup (ViewOutput sel edit) where
    (MkViewOutput update1 fss1) <> (MkViewOutput update2 fss2) = let
        voUpdate :: Object edit -> [edit] -> EditContext -> IO ()
        voUpdate obj edits ectxt = do
            update1 obj edits ectxt
            update2 obj edits ectxt
        voFirstAspect = do
            ma1 <- fss1
            case ma1 of
                Just a -> return $ Just a
                Nothing -> fss2
        in MkViewOutput {..}

instance Monoid (ViewOutput sel edit) where
    mempty = let
        voUpdate _ _ _ = return ()
        voFirstAspect = return Nothing
        in MkViewOutput {..}
    mappend = (<>)

voMapEdit :: forall sel edita editb. EditLens edita editb -> ViewOutput sel editb -> ViewOutput sel edita
voMapEdit lens@(MkCloseUnlift unlift flens) (MkViewOutput updateB a) = let
    MkAnEditLens {..} = flens
    MkAnEditFunction {..} = elFunction
    updateA :: Object edita -> [edita] -> EditContext -> IO ()
    updateA objA@(MkObject ou omr _) editsA ectxt = do
        editsB <-
            runTransform (composeUnliftTransform unlift ou) $
            withTransConstraintTM @MonadUnliftIO $ efUpdates elFunction editsA omr
        updateB (mapObject lens objA) editsB ectxt
    in (MkViewOutput updateA a)

voMapSelection :: forall sela selb edit. (sela -> selb) -> ViewOutput sela edit -> ViewOutput selb edit
voMapSelection f (MkViewOutput upd asp) = MkViewOutput upd $ mapSelectionAspect f asp

voNoAspect :: ViewOutput sela edit -> ViewOutput selb edit
voNoAspect (MkViewOutput upd _) = MkViewOutput upd noAspect

type ViewResult sel edit a = (ViewOutput sel edit, a)

vrMapEdit :: EditLens edita editb -> ViewResult sel editb a -> ViewResult sel edita a
vrMapEdit lens (vo, a) = (voMapEdit lens vo, a)

newtype CreateView sel edit a =
    MkCreateView (ReaderT (ViewContext sel edit) (WriterT (ViewOutput sel edit) LifeCycle) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadTunnelIO, MonadFix, MonadUnliftIO)

type ViewState sel edit a = LifeState (ViewResult sel edit a)

vsUpdate :: ViewState sel edit a -> Object edit -> [edit] -> EditContext -> IO ()
vsUpdate ((vo, _), _) = voUpdate vo

vsFirstAspect :: ViewState sel edit a -> Aspect sel
vsFirstAspect ((vo, _), _) = voFirstAspect vo

viewCreateView :: CreateView sel edit a -> View sel edit (ViewState sel edit a)
viewCreateView (MkCreateView (ReaderT wff)) = MkView $ ReaderT $ \vc -> getLifeState $ fmap swap $ runWriterT $ wff vc

cvLiftView :: View sel edit a -> CreateView sel edit a
cvLiftView (MkView (ReaderT va)) = MkCreateView $ ReaderT $ \vc -> liftIO $ va vc

cvLiftViewResult :: ViewResult sel edit a -> CreateView sel edit a
cvLiftViewResult (vo, a) = MkCreateView $ lift $ WriterT $ return (a, vo)

instance MonadLifeCycle (CreateView sel edit) where
    liftLifeCycle lc = MkCreateView $ lift $ lift lc

cvReceiveIOUpdates :: (Object edit -> [edit] -> EditSource -> IO ()) -> CreateView sel edit ()
cvReceiveIOUpdates recv = do
    tb <- MkCreateView $ asks vcThreadBarrier
    cvLiftViewResult $
        ( mempty {voUpdate = \obj edits MkEditContext {..} -> tb editContextAsync $ recv obj edits editContextSource}
        , ())

cvReceiveUpdates :: Maybe EditSource -> (UnliftIO (View sel edit) -> ReceiveUpdates edit) -> CreateView sel edit ()
cvReceiveUpdates mesrc recv = do
    unliftIO <- cvLiftView $ liftIOView $ \unlift -> return $ MkTransform unlift
    cvReceiveIOUpdates $ \(MkObject unliftObj mr _) edits esrc ->
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
    initial <- cvLiftView $ viewObjectRead $ \_ mr -> editFunctionRead ef mr ReadWhole
    cvLiftView $ setf initial
    cvReceiveUpdates mesrc $ \(MkTransform unlift) ->
        mapReceiveUpdates ef $ \_ wedits ->
            case lastWholeEdit wedits of
                Just newval -> liftIO $ unlift $ setf newval
                Nothing -> return ()

cvAddAspect :: Aspect sel -> CreateView sel edit ()
cvAddAspect aspect = cvLiftViewResult $ (mempty {voFirstAspect = aspect}, ())

mapReaderContext :: (r2 -> r1) -> ReaderT r1 m a -> ReaderT r2 m a
mapReaderContext f (ReaderT rma) = ReaderT $ \r2 -> rma $ f r2

mapWriterOutput :: Functor m => (w1 -> w2) -> WriterT w1 m a -> WriterT w2 m a
mapWriterOutput f (WriterT maw) = WriterT $ fmap (\(a, w) -> (a, f w)) maw

cvMapEdit ::
       forall sel edita editb a. ()
    => EditLens edita editb
    -> CreateView sel editb a
    -> CreateView sel edita a
cvMapEdit lens (MkCreateView ma) =
    MkCreateView $ mapReaderContext (vcMapEdit lens) $ remonad (mapWriterOutput $ voMapEdit lens) ma

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
       (Bool -> IO () -> IO ())
    -> AnyCreateView edit w
    -> Subscriber edit
    -> (forall t. IOWitness t -> Maybe t)
    -> LifeCycle w
subscribeView vcThreadBarrier (MkAnyCreateView (MkCreateView (ReaderT view))) (MkSubscriber vcObject sub) vcRequest = do
    let
        vcSetSelection :: Aspect sel -> IO ()
        vcSetSelection _ = return ()
    (w, vo) <- runWriterT $ view MkViewContext {..}
    sub $ voUpdate vo vcObject
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
