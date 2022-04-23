module Changes.UI.GTK.Maybe
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
    -> (f (Model update) -> View Widget)
    -> SelectNotify (f ())
    -> View Widget
oneWholeView model baseView sn = do
    box <- cvNew Box [#orientation := OrientationVertical]
    let
        addWidget :: View Widget -> View ()
        addWidget cvw = do
            widget <- cvw
            cvPackStart True box widget
            widgetShow widget
    viewInnerWholeView model (\fmodel -> addWidget $ baseView fmodel) sn
    toWidget box

createOneWhole ::
       forall f update. (IsUpdate update, MonadInner f, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update) -> View Widget)
    -> View Widget
createOneWhole sub itemspec = oneWholeView sub itemspec mempty

createOneWholeSel ::
       forall sel f update. (IsUpdate update, MonadInner f, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update, SelectNotify sel) -> View Widget)
    -> SelectNotify (f sel)
    -> View Widget
createOneWholeSel subf specsel snfsel = let
    spec :: f (Model update) -> View Widget
    spec fsub = specsel $ fmap (\sub -> (sub, contramap pure snfsel)) fsub
    getf :: f () -> Maybe (f sel)
    getf fu =
        case retrieveInner fu of
            SuccessResult _ -> Nothing
            FailureResult e -> Just $ throwExc e
    snfu :: SelectNotify (f ())
    snfu = mapMaybeSelectNotify getf snfsel
    in oneWholeView subf spec snfu
