module Changes.World.GNOME.GTK.Widget.Maybe
    ( createOneWhole
    , createOneWholeSel
    )
where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

oneWholeView ::
    forall f update.
    MonadInner f =>
    Model (FullResultOneUpdate f update) ->
    (f (Model update) -> GView 'Unlocked GI.Widget) ->
    SelectNotify (f ()) ->
    GView 'Unlocked GI.Widget
oneWholeView model baseView sn = do
    (box, widget) <- gvRunLocked $ gvNewWidget GI.Box [#orientation GI.:= GI.OrientationVertical]
    let
        addWidget :: GView 'Unlocked GI.Widget -> GView 'Unlocked ()
        addWidget cvw = do
            item <- cvw
            gvRunLocked $ do
                gvBoxPrepend box item
                GI.widgetShow item
    gvInnerWholeView model (\fmodel -> addWidget $ baseView fmodel) sn
    return widget

createOneWhole ::
    forall f update.
    MonadInner f =>
    Model (FullResultOneUpdate f update) ->
    (f (Model update) -> GView 'Unlocked GI.Widget) ->
    GView 'Unlocked GI.Widget
createOneWhole sub itemspec = oneWholeView sub itemspec mempty

createOneWholeSel ::
    forall sel f update.
    MonadInner f =>
    Model (FullResultOneUpdate f update) ->
    (f (Model update, SelectNotify sel) -> GView 'Unlocked GI.Widget) ->
    SelectNotify (f sel) ->
    GView 'Unlocked GI.Widget
createOneWholeSel subf specsel snfsel = let
    spec :: f (Model update) -> GView 'Unlocked GI.Widget
    spec fsub = specsel $ fmap (\sub -> (sub, contramap pure snfsel)) fsub
    getf :: f () -> Maybe (f sel)
    getf fu =
        case retrieveInner fu of
            SuccessResult _ -> Nothing
            FailureResult e -> Just $ throwExc e
    snfu :: SelectNotify (f ())
    snfu = mapMaybeSelectNotify getf snfsel
    in oneWholeView subf spec snfu
