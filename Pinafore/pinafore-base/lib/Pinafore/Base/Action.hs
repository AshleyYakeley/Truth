module Pinafore.Base.Action
    ( Action
    , unliftAction
    , unliftActionOrFail
    , runAction
    , actionLiftView
    , actionLiftViewKnow
    , actionLiftViewKnowWithUnlift
    , actionTunnelView
    , actionHoistView
    , actionLiftLifecycle
    , actionGetCreateViewUnlift
    , actionResourceContext
    , actionModelGet
    , actionModelPush
    , actionKnow
    , knowAction
    , actionOnClose
    , actionEarlyCloser
    , actionFloatMap
    , actionFloatMapReadOnly
    )
where

import Changes.Core
import Shapes

import Pinafore.Base.Know

newtype Action a = MkAction
    { unAction :: ComposeInner Know View a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , Alternative
        , MonadPlus
        , MonadFix
        , MonadFail
        , MonadIO
        , MonadHoistIO
        , MonadTunnelIO
        , MonadException
        , RepresentationalRole
        )

unliftAction :: forall a. Action a -> View (Know a)
unliftAction = unComposeInner . unAction

unliftActionOrFail :: Action --> View
unliftActionOrFail action = do
    ka <- unliftAction action
    case ka of
        Known a -> return a
        Unknown -> fail "action stopped"

runAction :: Action () -> View ()
runAction action = fmap (\_ -> ()) $ unliftAction action

actionLiftView :: View --> Action
actionLiftView va = MkAction $ lift va

actionLiftViewKnow :: View (Know a) -> Action a
actionLiftViewKnow va = MkAction $ MkComposeInner va

actionLiftViewKnowWithUnlift :: ((forall r. Action r -> View (Know r)) -> View (Know a)) -> Action a
actionLiftViewKnowWithUnlift call = actionLiftViewKnow $ call unliftAction

actionTunnelView :: forall r. (forall f. Functor f => (forall a. Action a -> View (f a)) -> View (f r)) -> Action r
actionTunnelView call = actionLiftViewKnow $ call unliftAction

actionHoistView :: (View --> View) -> Action --> Action
actionHoistView vv (MkAction ma) = MkAction $ hoist vv ma

actionGetCreateViewUnlift :: Action (WRaised Action (ComposeInner Know View))
actionGetCreateViewUnlift = return $ MkWRaised $ \(MkAction ra) -> ra

actionResourceContext :: Action ResourceContext
actionResourceContext = actionLiftView viewGetResourceContext

actionModelGet :: WModel update -> ReadM (UpdateReader update) t -> Action t
actionModelGet model rm = do
    rc <- actionResourceContext
    liftIO $ wModelGet rc model rm

actionModelPush :: WModel update -> NonEmpty (UpdateEdit update) -> Action ()
actionModelPush model edits = do
    rc <- actionResourceContext
    ok <- liftIO $ wModelPush rc model edits
    if ok
        then return ()
        else empty

actionLiftLifecycle :: Lifecycle --> Action
actionLiftLifecycle la = actionLiftView $ viewLiftLifecycle la

actionKnow :: forall a. Know a -> Action a
actionKnow (Known a) = pure a
actionKnow Unknown = empty

knowAction :: forall a. Action a -> Action (Know a)
knowAction (MkAction ka) = MkAction $ MkComposeInner $ fmap Known $ unComposeInner ka

actionOnClose :: Action () -> Action ()
actionOnClose closer = do
    MkWRaised unlift <- actionGetCreateViewUnlift
    actionLiftView
        $ viewOnClose
        $ do
            _ <- unComposeInner $ unlift closer
            return ()

actionEarlyCloser :: Action a -> Action (a, IO ())
actionEarlyCloser ra = do
    MkWRaised unlift <- actionGetCreateViewUnlift
    MkAction
        $ MkComposeInner
        $ do
            (ka, closer) <- viewGetCloser $ unComposeInner $ unlift ra
            return $ fmap (\a -> (a, closer)) ka

actionFloatMap ::
    forall f updateA updateB.
    FloatingEditApplicative f =>
    FloatingChangeLens updateA updateB ->
    f updateA ->
    Action (f updateB)
actionFloatMap flens fa = do
    rc <- actionResourceContext
    actionLiftLifecycle $ eaFloatMap rc flens fa

actionFloatMapReadOnly ::
    forall f updateA updateB.
    FloatingEditApplicative f =>
    FloatingChangeLens updateA (ReadOnlyUpdate updateB) ->
    f (ReadOnlyUpdate updateA) ->
    Action (f (ReadOnlyUpdate updateB))
actionFloatMapReadOnly flens = actionFloatMap $ liftReadOnlyFloatingChangeLens flens
