module Changes.World.GNOME.GTK.Widget.Scale where

import Changes.Core
import GI.Gtk as Gtk
import Shapes hiding (get)
import Shapes.Numeric

import Changes.World.GNOME.GI

createScale :: GView 'Locked (Scale, Widget)
createScale = gvNewWidget Scale []

attachScaleMarks :: Scale -> Model (ROWUpdate [(Double, Bool, Maybe Text)]) -> GView 'Unlocked ()
attachScaleMarks widget model =
    gvBindReadOnlyWholeModel model $ \marks ->
        gvRunLocked $ do
            scaleClearMarks widget
            for_ marks $ \(v, p, mt) ->
                scaleAddMark
                    widget
                    v
                    ( if p
                        then PositionTypeRight
                        else PositionTypeLeft
                    )
                    mt

attachScaleHasOrigin :: Scale -> Model (ROWUpdate Bool) -> GView 'Unlocked ()
attachScaleHasOrigin widget model =
    gvBindReadOnlyWholeModel model $ \val -> gvRunLocked $ set widget [#hasOrigin := val]

attachScaleDrawValue :: Scale -> Model (ROWUpdate (Maybe (PositionType, Int32))) -> GView 'Unlocked ()
attachScaleDrawValue widget model =
    gvBindReadOnlyWholeModel model $ \mval ->
        gvRunLocked
            $ case mval of
                Nothing -> set widget [#drawValue := False]
                Just (pos, digits) -> set widget [#drawValue := True, #valuePos := pos, #digits := digits]
