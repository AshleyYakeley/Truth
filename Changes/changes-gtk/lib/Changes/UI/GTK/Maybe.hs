module Changes.UI.GTK.Maybe
    ( createOneWhole
    , createOneWholeSel
    ) where

import Changes.Core
import Changes.UI.GTK.Useful
import GI.Gtk hiding (get)
import Shapes

oneWholeView ::
       forall f update. (MonadOne f, IsUpdate update, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update) -> CreateView Widget)
    -> SelectNotify (f ())
    -> CreateView Widget
oneWholeView model baseView sn = do
    box <- cvNew Box [#orientation := OrientationVertical]
    let
        addWidget :: CreateView Widget -> CreateView ()
        addWidget cvw = do
            widget <- cvw
            cvPackStart True box widget
            widgetShow widget
    cvOneWholeView model (\fmodel -> addWidget $ baseView fmodel) sn
    toWidget box

createOneWhole ::
       forall f update. (IsUpdate update, MonadOne f, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update) -> CreateView Widget)
    -> CreateView Widget
createOneWhole sub itemspec = oneWholeView sub itemspec mempty

createOneWholeSel ::
       forall sel f update. (IsUpdate update, MonadOne f, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update, SelectNotify sel) -> CreateView Widget)
    -> SelectNotify (f sel)
    -> CreateView Widget
createOneWholeSel subf specsel snfsel = let
    spec :: f (Model update) -> CreateView Widget
    spec fsub = specsel $ fmap (\sub -> (sub, contramap pure snfsel)) fsub
    getf :: f () -> Maybe (f sel)
    getf fu =
        case retrieveOne fu of
            SuccessResult _ -> Nothing
            FailureResult fn -> Just $ fmap never fn
    snfu :: SelectNotify (f ())
    snfu = mapMaybeSelectNotify getf snfsel
    in oneWholeView subf spec snfu
