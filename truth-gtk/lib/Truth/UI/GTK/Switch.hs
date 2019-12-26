module Truth.UI.GTK.Switch
    ( switchView
    , switchGetView
    ) where

import GI.Gtk hiding (get)
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

switchView :: forall sel. ReadOnlySubscriber (WholeUpdate (GCreateView sel)) -> GCreateView sel
switchView sub = do
    box <- liftIO $ boxNew OrientationVertical 0
    let
        getViewState :: GCreateView sel -> View sel (ViewState sel)
        getViewState gview =
            viewCreateView $ do
                widget <- gview
                lcContainPackStart True box widget
                #show widget
        getFirstVS ::
               forall m. MonadIO m
            => MFunction (CreateView sel) m
            -> MutableRead m (WholeReader (GCreateView sel))
            -> m (ViewState sel)
        getFirstVS unlift mr = do
            firstspec <- mr ReadWhole
            unlift $ cvLiftView $ getViewState firstspec
    unliftView <- cvLiftView askUnliftIO
    cvDynamic @(ViewState sel) sub getFirstVS $ \updates ->
        for_ (lastWholeUpdate $ fmap unReadOnlyUpdate updates) $ \spec ->
            replaceDynamicView $ runWMFunction unliftView $ getViewState spec
    toWidget box

switchGetView :: GetGView
switchGetView =
    MkGetView $ \getview uispec -> do
        spec <- isUISpec uispec
        return $
            case spec of
                MkSwitchUISpec sub -> switchView $ mapReadOnlySubscriber (funcUpdateFunction getview) sub
