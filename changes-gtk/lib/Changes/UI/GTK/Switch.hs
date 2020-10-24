module Changes.UI.GTK.Switch
    ( createDynamic
    ) where

import Changes.Core
import Changes.UI.GTK.Useful
import GI.Gtk hiding (get)
import Shapes

createDynamic :: Model (ROWUpdate (CreateView Widget)) -> CreateView Widget
createDynamic sub = do
    box <- liftIO $ boxNew OrientationVertical 0
    let
        getViewState :: CreateView Widget -> View ViewState
        getViewState gview = do
            ((), vs) <-
                getInnerLifeState $ do
                    widget <- gview
                    cvPackStart True box widget
                    #show widget
            return vs
        initVS :: Model (ROWUpdate (CreateView Widget)) -> CreateView (ViewState, ())
        initVS rm = do
            firstspec <- viewRunResource rm $ \am -> aModelRead am ReadWhole
            vs <- liftToLifeCycle $ getViewState firstspec
            return (vs, ())
        recvVS :: () -> [ROWUpdate (CreateView Widget)] -> StateT ViewState View ()
        recvVS () updates = for_ (lastReadOnlyWholeUpdate updates) $ \spec -> replaceDynamicView $ getViewState spec
    cvDynamic @(ViewState) sub initVS mempty recvVS
    toWidget box
