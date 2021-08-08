{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Std.Convert
    (
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Language.Convert ()
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Language.Value
import Shapes

-- IO
instance (ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (IO a) where
    toPolarShimWit = mapPosShimWit (functionToShim "subtype" $ liftIO @PinaforeAction) toJMShimWit

-- View
instance (ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (View a) where
    toPolarShimWit = mapPosShimWit (functionToShim "subtype" $ liftToLifeCycle @CreateView) toJMShimWit

-- CreateView
instance (ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (CreateView a) where
    toPolarShimWit = mapPosShimWit (functionToShim "subtype" createViewPinaforeAction) toJMShimWit

-- WModel
instance ( FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) t
         , ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) t
         ) => ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (WModel (WholeUpdate (Know t))) where
    toPolarShimWit = mapPosShimWit (functionToShim "subtype" pinaforeRefToWholeRef) toJMShimWit

instance ( FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) t
         , ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) t
         ) => FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (WModel (WholeUpdate (Know t))) where
    fromPolarShimWit = mapNegShimWit (functionToShim "subtype" langWholeRefToValue) fromJMShimWit

-- PinaforeROWRef
instance (FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) t) =>
             FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (PinaforeROWRef (Know t)) where
    fromPolarShimWit = mapNegShimWit (functionToShim "subtype" langWholeRefToReadOnlyValue) fromJMShimWit

instance (ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) t) =>
             ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (PinaforeROWRef (Know t)) where
    toPolarShimWit = mapPosShimWit (functionToShim "subtype" pinaforeROWRefToWholeRef) toJMShimWit

-- LangMorphism
instance ( FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) pa
         , ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) qa
         , FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) pb
         , ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) qb
         ) =>
             ToPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (LangMorphism '( pa, qa) '( pb, qb)) where
    toPolarShimWit =
        unToRangeShimWit @_ @_ @pa @qa $ \ta conva ->
            unToRangeShimWit @_ @_ @pb @qb $ \tb convb ->
                mapPosShimWit (applyPolyShim RangevarianceType (applyPolyShim RangevarianceType cid conva) convb) $
                mkPolarShimWit $
                GroundedDolanSingularType morphismGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) pa
         , ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) qa
         , FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) pb
         , ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) qb
         ) => ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (LangMorphism '( pa, qa) '( pb, qb)) where
    toPolarShimWit = singleDolanShimWit toJMShimWit

instance ( ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) pa
         , FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) qa
         , ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) pb
         , FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) qb
         ) =>
             FromPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (LangMorphism '( pa, qa) '( pb, qb)) where
    fromPolarShimWit =
        unFromRangeShimWit $ \ta conva ->
            unFromRangeShimWit $ \tb convb ->
                mapNegShimWit (applyPolyShim RangevarianceType (applyPolyShim RangevarianceType cid conva) convb) $
                mkPolarShimWit $
                GroundedDolanSingularType morphismGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) pa
         , FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) qa
         , ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) pb
         , FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) qb
         ) => FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (LangMorphism '( pa, qa) '( pb, qb)) where
    fromPolarShimWit = singleDolanShimWit fromJMShimWit

-- Entity
instance ToPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) Entity where
    toPolarShimWit =
        mkPolarShimWit $
        GroundedDolanSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments

instance ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) Entity where
    toPolarShimWit = singleDolanShimWit toJMShimWit

instance FromPolarShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) Entity where
    fromPolarShimWit =
        mkPolarShimWit $
        GroundedDolanSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments

instance FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) Entity where
    fromPolarShimWit = singleDolanShimWit fromJMShimWit
