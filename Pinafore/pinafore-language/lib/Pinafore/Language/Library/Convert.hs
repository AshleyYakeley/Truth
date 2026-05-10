{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Convert
    (
    )
where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Type

-- IO
instance HasQType QPolyShim 'Positive a => HasQType QPolyShim 'Positive (IO a) where
    qType = mapPosShimWit (functionToShim "liftIO" $ liftIO @Action) qType

-- Subview
instance HasQType QPolyShim 'Positive a => HasQType QPolyShim 'Positive (Semiview a) where
    qType = mapPosShimWit (functionToShim "actionLiftView . lift" $ actionLiftView . lift) qType

-- View
instance HasQType QPolyShim 'Positive a => HasQType QPolyShim 'Positive (View a) where
    qType = mapPosShimWit (functionToShim "actionLiftView" actionLiftView) qType

-- Entity
instance HasQGroundType '[] Entity where
    qGroundType = entityGroundType
