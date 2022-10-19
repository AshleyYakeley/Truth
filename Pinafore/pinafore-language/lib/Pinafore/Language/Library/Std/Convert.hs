{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Std.Convert
    (
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.Type
import Shapes

-- IO
instance (HasPinaforeType 'Positive a) => HasPinaforeType 'Positive (IO a) where
    pinaforeType = mapPosShimWit (functionToShim "liftIO" $ liftIO @PinaforeAction) pinaforeType

-- View
instance (HasPinaforeType 'Positive a) => HasPinaforeType 'Positive (View a) where
    pinaforeType = mapPosShimWit (functionToShim "actionLiftView" actionLiftView) pinaforeType

-- Entity
instance HasPinaforeGroundType '[] Entity where
    pinaforeGroundType = entityGroundType

-- DynamicEntity
instance HasPinaforeGroundType '[] DynamicEntity where
    pinaforeGroundType = dynamicEntityGroundType
