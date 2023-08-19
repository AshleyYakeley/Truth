module Changes.World.GNOME.GTK.Element.Maybe
    ( createOneWhole
    , createOneWholeSel
    ) where

import Changes.Core
import Changes.World.GNOME.GI
import GI.Gtk hiding (get)
import Shapes

oneWholeView ::
       forall f update. (MonadInner f, IsUpdate update, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update) -> GView 'Unlocked Widget)
    -> SelectNotify (f ())
    -> GView 'Unlocked Widget
oneWholeView model baseView sn = do
    (box, widget) <- gvRunLocked $ gvNewWidget Box [#orientation := OrientationVertical]
    let
        addWidget :: GView 'Unlocked Widget -> GView 'Unlocked ()
        addWidget cvw = do
            item <- cvw
            gvRunLocked $ do
                gvPackStart True box item
                widgetShow item
    gvInnerWholeView model (\fmodel -> addWidget $ baseView fmodel) sn
    return widget

createOneWhole ::
       forall f update. (IsUpdate update, MonadInner f, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update) -> GView 'Unlocked Widget)
    -> GView 'Unlocked Widget
createOneWhole sub itemspec = oneWholeView sub itemspec mempty

createOneWholeSel ::
       forall sel f update. (IsUpdate update, MonadInner f, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update, SelectNotify sel) -> GView 'Unlocked Widget)
    -> SelectNotify (f sel)
    -> GView 'Unlocked Widget
createOneWholeSel subf specsel snfsel = let
    spec :: f (Model update) -> GView 'Unlocked Widget
    spec fsub = specsel $ fmap (\sub -> (sub, contramap pure snfsel)) fsub
    getf :: f () -> Maybe (f sel)
    getf fu =
        case retrieveInner fu of
            SuccessResult _ -> Nothing
            FailureResult e -> Just $ throwExc e
    snfu :: SelectNotify (f ())
    snfu = mapMaybeSelectNotify getf snfsel
    in oneWholeView subf spec snfu
