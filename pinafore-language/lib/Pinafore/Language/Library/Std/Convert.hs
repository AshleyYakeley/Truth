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
instance (ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (IO a) where
    toShimWit = mapPosShimWit (functionToShim "subtype" $ liftIO @PinaforeAction) toJMShimWit

-- View
instance (ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (View a) where
    toShimWit = mapPosShimWit (functionToShim "subtype" $ liftToLifeCycle @CreateView) toJMShimWit

-- CreateView
instance (ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a) =>
             ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (CreateView a) where
    toShimWit = mapPosShimWit (functionToShim "subtype" createViewPinaforeAction) toJMShimWit

-- WModel
instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) t
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) t
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (WModel (WholeUpdate (Know t))) where
    toShimWit = mapPosShimWit (functionToShim "subtype" pinaforeRefToWholeRef) toJMShimWit

instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) t
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) t
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (WModel (WholeUpdate (Know t))) where
    fromShimWit = mapNegShimWit (functionToShim "subtype" langWholeRefToValue) fromJMShimWit

-- PinaforeROWRef
instance (FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) t) =>
             FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (PinaforeROWRef (Know t)) where
    fromShimWit = mapNegShimWit (functionToShim "subtype" langWholeRefToReadOnlyValue) fromJMShimWit

instance (ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) t) =>
             ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (PinaforeROWRef (Know t)) where
    toShimWit = mapPosShimWit (functionToShim "subtype" pinaforeROWRefToWholeRef) toJMShimWit

-- LangMorphism
instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) pa
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) qa
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) pb
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) qb
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (LangMorphism '( pa, qa) '( pb, qb)) where
    toShimWit =
        unToRangeShimWit @_ @_ @pa @qa $ \ta conva ->
            unToRangeShimWit @_ @_ @pb @qb $ \tb convb ->
                mapPosShimWit (applyPolyShim RangevarianceType (applyPolyShim RangevarianceType cid conva) convb) $
                mkShimWit $
                GroundDolanSingularType morphismGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) pa
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) qa
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) pb
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) qb
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (LangMorphism '( pa, qa) '( pb, qb)) where
    toShimWit = singleDolanShimWit toJMShimWit

instance ( ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) pa
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) qa
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) pb
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) qb
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (LangMorphism '( pa, qa) '( pb, qb)) where
    fromShimWit =
        unFromRangeShimWit $ \ta conva ->
            unFromRangeShimWit $ \tb convb ->
                mapNegShimWit (applyPolyShim RangevarianceType (applyPolyShim RangevarianceType cid conva) convb) $
                mkShimWit $
                GroundDolanSingularType morphismGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) pa
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) qa
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) pb
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) qb
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (LangMorphism '( pa, qa) '( pb, qb)) where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- Entity
instance ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) Entity where
    toShimWit =
        mkShimWit $ GroundDolanSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments

instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) Entity where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) Entity where
    fromShimWit =
        mkShimWit $ GroundDolanSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) Entity where
    fromShimWit = singleDolanShimWit fromJMShimWit
