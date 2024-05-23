{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Convert
    (
    ) where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Type

-- IO
instance (HasQType 'Positive a) => HasQType 'Positive (IO a) where
    qType = mapPosShimWit (functionToShim "liftIO" $ liftIO @Action) qType

-- View
instance (HasQType 'Positive a) => HasQType 'Positive (View a) where
    qType = mapPosShimWit (functionToShim "actionLiftView" actionLiftView) qType

-- Entity
instance HasQGroundType '[] Entity where
    qGroundType = entityGroundType

-- DynamicEntity
instance HasQGroundType '[] DynamicEntity where
    qGroundType = dynamicEntityStorableGroundType
