{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Std.Convert
    (
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.Type
import Pinafore.Language.Value
import Shapes

-- IO
instance (HasPinaforeType 'Positive a) => HasPinaforeType 'Positive (IO a) where
    pinaforeType = mapPosShimWit (functionToShim "liftIO" $ liftIO @PinaforeAction) pinaforeType

-- View
instance (HasPinaforeType 'Positive a) => HasPinaforeType 'Positive (View a) where
    pinaforeType = mapPosShimWit (functionToShim "liftToLifeCycle" $ liftToLifeCycle @CreateView) pinaforeType

-- CreateView
instance (HasPinaforeType 'Positive a) => HasPinaforeType 'Positive (CreateView a) where
    pinaforeType = mapPosShimWit (functionToShim "createViewPinaforeAction" createViewPinaforeAction) pinaforeType

-- WModel
instance (HasPinaforeType 'Positive t, HasPinaforeType 'Negative t) =>
             HasPinaforeType 'Positive (WModel (WholeUpdate (Know t))) where
    pinaforeType = mapPosShimWit (functionToShim "pinaforeRefToWholeRef" pinaforeRefToWholeRef) pinaforeType

instance (HasPinaforeType 'Positive t, HasPinaforeType 'Negative t) =>
             HasPinaforeType 'Negative (WModel (WholeUpdate (Know t))) where
    pinaforeType = mapNegShimWit (functionToShim "langWholeRefToValue" langWholeRefToValue) pinaforeType

-- PinaforeROWRef
instance (HasPinaforeType 'Negative a) => HasPinaforeType 'Negative (PinaforeROWRef (Know a)) where
    pinaforeType = mapNegShimWit (functionToShim "langWholeRefToReadOnlyValue" langWholeRefToReadOnlyValue) pinaforeType

instance (HasPinaforeType 'Positive a) => HasPinaforeType 'Positive (PinaforeROWRef (Know a)) where
    pinaforeType = mapPosShimWit (functionToShim "pinaforeROWRefToWholeRef" pinaforeROWRefToWholeRef) pinaforeType

-- LangMorphism
instance HasPinaforeGroundType '[ 'RangeCCRVariance, 'RangeCCRVariance] LangMorphism where
    pinaforeGroundType = morphismGroundType

-- Entity
instance HasPinaforeGroundType '[] Entity where
    pinaforeGroundType = entityGroundType
