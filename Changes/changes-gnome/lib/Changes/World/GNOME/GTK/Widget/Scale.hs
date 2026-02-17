module Changes.World.GNOME.GTK.Widget.Scale where

import Shapes.Numeric

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

createScale :: GView 'Locked (GI.Scale, GI.Widget)
createScale = gvNewWidget GI.Scale []

attachScaleMarks :: GI.Scale -> Model (ROWUpdate [(Double, Bool, Maybe Text)]) -> GView 'Unlocked ()
attachScaleMarks widget model =
    gvBindReadOnlyWholeModel model $ \marks ->
        gvRunLocked $ do
            GI.scaleClearMarks widget
            for_ marks $ \(v, p, mt) ->
                GI.scaleAddMark
                    widget
                    v
                    ( if p
                        then GI.PositionTypeRight
                        else GI.PositionTypeLeft
                    )
                    mt

attachScaleHasOrigin :: GI.Scale -> Model (ROWUpdate Bool) -> GView 'Unlocked ()
attachScaleHasOrigin widget model =
    gvBindReadOnlyWholeModel model $ \val -> gvRunLocked $ GI.set widget [#hasOrigin GI.:= val]

attachScaleDrawValue :: GI.Scale -> Model (ROWUpdate (Maybe (GI.PositionType, Int32))) -> GView 'Unlocked ()
attachScaleDrawValue widget model =
    gvBindReadOnlyWholeModel model $ \mval ->
        gvRunLocked
            $ case mval of
                Nothing -> GI.set widget [#drawValue GI.:= False]
                Just (pos, digits) -> GI.set widget [#drawValue GI.:= True, #valuePos GI.:= pos, #digits GI.:= digits]
