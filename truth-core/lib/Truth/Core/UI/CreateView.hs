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
    , cvMapSetSelectionEdit
    , cvNoAspect
    , cvMapAspect
    , subscribeView
    , subscribeView'
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
import Truth.Core.UI.Specifier.SelectionLens
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.View
import Truth.Core.UI.ViewContext

data ViewOutput seledit edit = MkViewOutput
    { voUpdate :: Object edit -> [edit] -> IO ()
    , voFirstAspect :: Aspect seledit edit
    }

instance Semigroup (ViewOutput seledit edit) where
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

instance Monoid (ViewOutput seledit edit) where
    mempty = let
        voUpdate _ _ = return ()
        voFirstAspect = return Nothing
        in MkViewOutput {..}
    mappend = (<>)

voMapEdit :: forall seledit edita editb. EditLens edita editb -> ViewOutput seledit editb -> ViewOutput seledit edita
voMapEdit lens@(MkCloseUnlift unlift flens) (MkViewOutput updateB a) = let
    MkAnEditLens {..} = flens
    MkAnEditFunction {..} = elFunction
    updateA :: Object edita -> [edita] -> IO ()
    updateA objA@(MkObject ou omr _) editsA = do
        editsB <-
            runTransform (composeUnliftTransform unlift ou) $
            withTransConstraintTM @MonadUnliftIO $ efUpdates elFunction editsA omr
        updateB (mapObject lens objA) editsB
    a' = aspectMapEdit lens a
    in (MkViewOutput updateA a')

voMapSetSelectionEdit ::
       forall seledita seleditb edit. EditLens seledita seleditb -> ViewOutput seledita edit -> ViewOutput seleditb edit
voMapSetSelectionEdit lens (MkViewOutput upd asp) = MkViewOutput upd $ aspectMapSelectionEdit lens asp

voNoAspect :: ViewOutput seledita edit -> ViewOutput seleditb edit
voNoAspect (MkViewOutput upd _) = MkViewOutput upd noAspect

type ViewResult seledit edit a = (ViewOutput seledit edit, a)

vrMapEdit :: EditLens edita editb -> ViewResult seledit editb a -> ViewResult seledit edita a
vrMapEdit lens (vo, a) = (voMapEdit lens vo, a)

newtype CreateView seledit edit a =
    MkCreateView (ReaderT (ViewContext seledit edit) (WriterT (ViewOutput seledit edit) LifeCycle) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadTunnelIO, MonadFix, MonadUnliftIO)

type ViewState seledit edit a = LifeState (ViewResult seledit edit a)

vsUpdate :: ViewState seledit edit a -> Object edit -> [edit] -> IO ()
vsUpdate ((vo, _), _) = voUpdate vo

vsFirstAspect :: ViewState seledit edit a -> Aspect seledit edit
vsFirstAspect ((vo, _), _) = voFirstAspect vo

viewCreateView :: CreateView seledit edit a -> View seledit edit (ViewState seledit edit a)
viewCreateView (MkCreateView (ReaderT wff)) = MkView $ ReaderT $ \vc -> runLifeCycle $ fmap swap $ runWriterT $ wff vc

cvLiftView :: View seledit edit a -> CreateView seledit edit a
cvLiftView (MkView (ReaderT va)) = MkCreateView $ ReaderT $ \vc -> liftIO $ va vc

cvLiftViewResult :: ViewResult seledit edit a -> CreateView seledit edit a
cvLiftViewResult (vo, a) = MkCreateView $ lift $ WriterT $ return (a, vo)

instance MonadLifeCycle (CreateView seledit edit) where
    liftLifeCycle lc = MkCreateView $ lift $ lift lc

cvReceiveIOUpdates :: (Object edit -> [edit] -> IO ()) -> CreateView seledit edit ()
cvReceiveIOUpdates recv = do cvLiftViewResult $ (mempty {voUpdate = recv}, ())

cvReceiveUpdates :: (UnliftIO (View seledit edit) -> ReceiveUpdates edit) -> CreateView seledit edit ()
cvReceiveUpdates recv = do
    unliftIO <- cvLiftView $ liftIOView $ \unlift -> return $ MkTransform unlift
    cvReceiveIOUpdates $ \(MkObject unliftObj mr _) edits -> runTransform unliftObj $ recv unliftIO mr edits

cvReceiveUpdate ::
       (UnliftIO (View seledit edit) -> forall m. MonadUnliftIO m => MutableRead m (EditReader edit) -> edit -> m ())
    -> CreateView seledit edit ()
cvReceiveUpdate recv = cvReceiveUpdates $ \unlift mr edits -> for_ edits (recv unlift mr)

cvBindEditFunction :: EditFunction edit (WholeEdit t) -> (t -> View seledit edit ()) -> CreateView seledit edit ()
cvBindEditFunction ef setf = do
    initial <- cvLiftView $ viewObjectRead $ \_ mr -> editFunctionRead ef mr ReadWhole
    cvLiftView $ setf initial
    cvReceiveUpdates $ \(MkTransform unlift) ->
        mapReceiveUpdates ef $ \_ wedits ->
            case lastWholeEdit wedits of
                Just newval -> liftIO $ unlift $ setf newval
                Nothing -> return ()

cvAddAspect :: Aspect seledit edit -> CreateView seledit edit ()
cvAddAspect aspect = cvLiftViewResult $ (mempty {voFirstAspect = aspect}, ())

mapReaderContext :: (r2 -> r1) -> ReaderT r1 m a -> ReaderT r2 m a
mapReaderContext f (ReaderT rma) = ReaderT $ \r2 -> rma $ f r2

mapWriterOutput :: Functor m => (w1 -> w2) -> WriterT w1 m a -> WriterT w2 m a
mapWriterOutput f (WriterT maw) = WriterT $ fmap (\(a, w) -> (a, f w)) maw

cvMapEdit ::
       forall seledit edita editb a. ()
    => EditLens edita editb
    -> CreateView seledit editb a
    -> CreateView seledit edita a
cvMapEdit lens (MkCreateView ma) =
    MkCreateView $ mapReaderContext (vcMapEdit lens) $ remonad (mapWriterOutput $ voMapEdit lens) ma

cvMapSetSelectionEdit ::
       forall seledita seleditb edit a. ()
    => EditLens seledita seleditb
    -> CreateView seledita edit a
    -> CreateView seleditb edit a
cvMapSetSelectionEdit lens (MkCreateView ma) =
    MkCreateView $
    mapReaderContext (vcMapSetSelectionEdit lens) $ remonad (mapWriterOutput $ voMapSetSelectionEdit lens) ma

cvNoAspect :: CreateView seledita edit a -> CreateView seleditb edit a
cvNoAspect (MkCreateView ma) = MkCreateView $ mapReaderContext vcNoAspect $ remonad (mapWriterOutput voNoAspect) ma

cvMapAspect :: (Aspect seledit edit -> Aspect seledit edit) -> CreateView seledit edit w -> CreateView seledit edit w
cvMapAspect f (MkCreateView mw) =
    MkCreateView $ remonad (mapWriterOutput (\(MkViewOutput v ag) -> MkViewOutput v $ f ag)) mw

subscribeView' ::
       forall seledit edit w action.
       CreateView seledit edit w
    -> Subscriber edit action
    -> (UIWindow edit -> IO ())
    -> (forall t. IOWitness t -> Maybe t)
    -> LifeCycle (w, action)
subscribeView' (MkCreateView (ReaderT (view :: ViewContext seledit edit -> WriterT (ViewOutput seledit edit) LifeCycle w))) sub vcOpenWindow vcRequest = do
    let
        initialise :: Object edit -> LifeCycle (ViewResult seledit edit w)
        initialise vcObject = do
            rec
                let
                    vcSetSelection :: Aspect seledit edit -> IO ()
                    vcSetSelection ss = liftIO $ writeIORef selref ss
                    vcGetSelection :: IO (Maybe (Object seledit))
                    vcGetSelection = do
                        asp <- readIORef selref
                        muasp <- asp
                        for muasp $ \uasp -> return $ mapObject (uiaLens uasp) vcObject
                    vcOpenSelection :: IO ()
                    vcOpenSelection = do
                        asp <- readIORef selref
                        muasp <- asp
                        case muasp of
                            Just (neww :: UIAspect seledit edit) -> vcOpenWindow $ uiaWindow neww
                            Nothing -> return ()
                (w, vo) <- runWriterT $ view MkViewContext {..}
                selref :: IORef (Aspect seledit edit) <- liftIO $ newIORef $ voFirstAspect vo
            return (vo, w)
        receive :: (ViewResult seledit edit w) -> Object edit -> [edit] -> IO ()
        receive (vo, _) = voUpdate vo
    ((_, w), _, action) <- subscribeLifeCycle sub initialise receive
    return (w, action)

subscribeView ::
       forall seledit edit w action.
       (IO () -> CreateView seledit edit (action -> LifeCycle w))
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
    => (forall edit. sel edit -> m (CreateView seledit edit w))
    -> m (CreateView seledit (TupleEdit sel) [w])
tupleCreateView pickview =
    fmap sequence $
    for tupleAllSelectors $ \(MkAnyW sel) ->
        case tupleWitness @ApplicableEdit sel of
            Dict -> fmap (cvMapEdit (tupleEditLens sel)) (pickview sel)
