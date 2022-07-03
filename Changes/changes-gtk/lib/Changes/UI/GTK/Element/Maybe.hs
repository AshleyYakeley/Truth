module Changes.UI.GTK.Element.Maybe
    ( createOneWhole
    , createOneWholeSel
    ) where

import Changes.Core
import Changes.GI
import GI.Gtk hiding (get)
import Shapes

oneWholeView ::
       forall f update. (MonadInner f, IsUpdate update, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update) -> GView 'Locked Widget)
    -> SelectNotify (f ())
    -> GView 'Locked Widget
oneWholeView model baseView sn = do
    box <- gvNew Box [#orientation := OrientationVertical]
    let
        addWidget :: GView 'Locked Widget -> GView 'Locked ()
        addWidget cvw = do
            widget <- cvw
            gvPackStart True box widget
            widgetShow widget
    gvInnerWholeView model (\fmodel -> gvRunLocked $ addWidget $ baseView fmodel) sn
    toWidget box

createOneWhole ::
       forall f update. (IsUpdate update, MonadInner f, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update) -> GView 'Locked Widget)
    -> GView 'Locked Widget
createOneWhole sub itemspec = oneWholeView sub itemspec mempty

createOneWholeSel ::
       forall sel f update. (IsUpdate update, MonadInner f, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update, SelectNotify sel) -> GView 'Locked Widget)
    -> SelectNotify (f sel)
    -> GView 'Locked Widget
createOneWholeSel subf specsel snfsel = let
    spec :: f (Model update) -> GView 'Locked Widget
    spec fsub = specsel $ fmap (\sub -> (sub, contramap pure snfsel)) fsub
    getf :: f () -> Maybe (f sel)
    getf fu =
        case retrieveInner fu of
            SuccessResult _ -> Nothing
            FailureResult e -> Just $ throwExc e
    snfu :: SelectNotify (f ())
    snfu = mapMaybeSelectNotify getf snfsel
    in oneWholeView subf spec snfu
