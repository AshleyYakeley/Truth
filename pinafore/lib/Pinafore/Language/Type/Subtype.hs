{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Type.Subtype
    ( SubtypeContext(..)
    , subtypeGroundTypes
    ) where

import Language.Expression.Dolan
import Language.Expression.Polarity
import Language.Expression.TypeF
import Pinafore.Base
import Pinafore.Language.EntityType
import Pinafore.Language.GroundType
import Pinafore.Language.Literal
import Pinafore.Language.OpenEntity
import Pinafore.Language.Scope
import Pinafore.Language.Show
import Pinafore.Language.Type.Type
import Shapes

data SubtypeContext baseedit m pola polb = MkSubtypeContext
    { subtypeTypes :: forall ta tb. PinaforeType baseedit pola ta -> PinaforeType baseedit polb tb -> m (ta -> tb)
    , subtypeLift :: forall a. PinaforeSourceScoped baseedit a -> m a
    , subtypeInverted :: SubtypeContext baseedit m (InvertPolarity polb) (InvertPolarity pola)
    }

subtypeVariance ::
       (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext baseedit m pola polb
    -> SingleVarianceType sv
    -> SingleArgument sv (PinaforeType baseedit) pola a
    -> SingleArgument sv (PinaforeType baseedit) polb b
    -> m (SingleVarianceFunc (->) sv a b)
subtypeVariance sc CovarianceType ta tb = subtypeTypes sc ta tb
subtypeVariance sc ContravarianceType ta tb = do
    ba <- subtypeTypes (subtypeInverted sc) tb ta
    return $ MkCatDual ba
subtypeVariance sc RangevarianceType (MkRangeType tpa tqa) (MkRangeType tpb tqb) = do
    pba <- subtypeTypes (subtypeInverted sc) tpb tpa
    qab <- subtypeTypes sc tqa tqb
    return $ MkWithRange pba qab

subtypeArguments ::
       forall baseedit m pola polb dv gta gtb ta tb. (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext baseedit m pola polb
    -> DolanVarianceType dv
    -> DolanVarianceMap (->) dv gta
    -> DolanArguments dv (PinaforeType baseedit) gta pola ta
    -> DolanArguments dv (PinaforeType baseedit) gtb polb tb
    -> m (KindFunction (DolanVarianceKind dv) gta gtb -> ta -> tb)
subtypeArguments _ NilListType NilDolanVarianceMap NilDolanArguments NilDolanArguments = pure id
subtypeArguments sc (ConsListType (svt :: SingleVarianceType sv) (dvt :: DolanVarianceType dv')) (ConsDolanVarianceMap svm dvm) (ConsDolanArguments sta dta) (ConsDolanArguments stb dtb) = do
    sfunc <- subtypeVariance sc svt sta stb
    f <- subtypeArguments sc dvt dvm dta dtb
    pure $
        case dolanVarianceKMCategory @(->) dvt of
            Dict -> \(MkNestedMorphism conv) -> f (conv . svm sfunc)

pinaforeSubtypeArguments ::
       forall baseedit m pola polb pol dv gt argsa argsb. (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext baseedit m pola polb
    -> PinaforeGroundType baseedit pol dv gt
    -> DolanArguments dv (PinaforeType baseedit) gt pola argsa
    -> DolanArguments dv (PinaforeType baseedit) gt polb argsb
    -> m (argsa -> argsb)
pinaforeSubtypeArguments sc gt argsa argsb = let
    vkt = pinaforeGroundTypeVarianceType gt
    in case dolanVarianceKMCategory @(->) vkt of
           Dict -> fmap (\f -> f id) $ subtypeArguments sc vkt (pinaforeGroundTypeVarianceMap gt) argsa argsb

entitySubtypeArguments ::
       forall baseedit m pola polb dv f a b. (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext baseedit m pola polb
    -> CovaryType dv
    -> EntityGroundType f
    -> DolanArguments dv (PinaforeType baseedit) f pola a
    -> DolanArguments dv (PinaforeType baseedit) f polb b
    -> m (a -> b)
entitySubtypeArguments sc ct gt argsa argsb = let
    dvt = mapListType (\Refl -> CovarianceType) ct
    in case dolanVarianceKMCategory @(->) dvt of
           Dict ->
               fmap (\f -> f id) $
               subtypeArguments sc dvt (covaryToDolanVarianceMap ct $ entityGroundTypeCovaryMap gt) argsa argsb

entityGroundSubtype ::
       forall baseedit m pola polb dva fa a dvb fb b. (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext baseedit m pola polb
    -> CovaryType dva
    -> EntityGroundType fa
    -> DolanArguments dva (PinaforeType baseedit) fa pola a
    -> CovaryType dvb
    -> EntityGroundType fb
    -> DolanArguments dvb (PinaforeType baseedit) fb polb b
    -> m (a -> b)
entityGroundSubtype _ ct gt args NilListType TopEntityGroundType NilDolanArguments
    | Just ebij <- pinaforeEntityToEntityType ct gt args =
        case ebij of
            MkTypeF et conv ->
                pure $
                entityAdapterConvert (entityAdapter et) .
                case representative @_ @_ @pola of
                    PositiveType -> biForwards conv
                    NegativeType -> biBackwards conv
entityGroundSubtype _ NilListType (LiteralEntityGroundType t1) NilDolanArguments NilListType (LiteralEntityGroundType t2) NilDolanArguments
    | Just conv <- isSubtype t1 t2 = pure conv
entityGroundSubtype _ NilListType NewEntityGroundType NilDolanArguments NilListType NewEntityGroundType NilDolanArguments =
    pure id
entityGroundSubtype _ NilListType NewEntityGroundType NilDolanArguments NilListType (OpenEntityGroundType _ _) NilDolanArguments =
    pure $ MkOpenEntity . unNewEntity
entityGroundSubtype sc NilListType (OpenEntityGroundType n1 t1) NilDolanArguments NilListType (OpenEntityGroundType n2 t2) NilDolanArguments =
    subtypeLift sc $ getEntitySubtype n1 t1 n2 t2
entityGroundSubtype _ NilListType (ClosedEntityGroundType _ sa ta) NilDolanArguments NilListType (ClosedEntityGroundType _ sb tb) NilDolanArguments
    | Just Refl <- testEquality sa sb
    , Just Refl <- testEquality ta tb = pure id
entityGroundSubtype sc cta ga argsa ctb gb argsb
    | Just HRefl <- entityGroundTypeTestEquality ga gb
    , Just Refl <- testEquality cta ctb = entitySubtypeArguments sc cta ga argsa argsb
entityGroundSubtype sc cta ga argsa ctb gb argsb =
    subtypeLift sc $
    convertFailure
        (unpack $ exprShow $ GroundPinaforeSingularType (EntityPinaforeGroundType cta ga) argsa)
        (unpack $ exprShow $ GroundPinaforeSingularType (EntityPinaforeGroundType ctb gb) argsb)

subtypeGroundTypes ::
       forall baseedit m pola polb dva gta a dvb gtb b. (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext baseedit m pola polb
    -> PinaforeGroundType baseedit pola dva gta
    -> DolanArguments dva (PinaforeType baseedit) gta pola a
    -> PinaforeGroundType baseedit polb dvb gtb
    -> DolanArguments dvb (PinaforeType baseedit) gtb polb b
    -> m (a -> b)
subtypeGroundTypes sc (EntityPinaforeGroundType cta ga) argsa (EntityPinaforeGroundType ctb gb) argsb =
    entityGroundSubtype sc cta ga argsa ctb gb argsb
subtypeGroundTypes sc ga argsa gb argsb
    | Just (Refl, HRefl) <- pinaforeGroundTypeTestEquality ga gb = pinaforeSubtypeArguments sc ga argsa argsb
subtypeGroundTypes sc ga argsa gb argsb =
    subtypeLift sc $
    convertFailure
        (unpack $ exprShow $ GroundPinaforeSingularType ga argsa)
        (unpack $ exprShow $ GroundPinaforeSingularType gb argsb)
