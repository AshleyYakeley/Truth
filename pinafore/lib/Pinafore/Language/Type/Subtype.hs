{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Type.Subtype
    ( SubtypeContext(..)
    , subtypeGroundTypes
    ) where

import Data.Shim
import Language.Expression.Dolan
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
    { subtypeTypes :: forall ta tb. PinaforeType baseedit pola ta -> PinaforeType baseedit polb tb -> m (JMShim ta tb)
    , subtypeLift :: forall a. PinaforeSourceScoped baseedit a -> m a
    , subtypeInverted :: SubtypeContext baseedit m (InvertPolarity polb) (InvertPolarity pola)
    }

subtypeVariance ::
       (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext baseedit m pola polb
    -> VarianceType sv
    -> SingleArgument sv (PinaforeType baseedit) pola a
    -> SingleArgument sv (PinaforeType baseedit) polb b
    -> m (VarianceCategory JMShim sv a b)
subtypeVariance sc CovarianceType ta tb = subtypeTypes sc ta tb
subtypeVariance sc ContravarianceType ta tb = do
    ba <- subtypeTypes (subtypeInverted sc) tb ta
    return $ MkCatDual ba
subtypeVariance sc RangevarianceType (MkRangeType tpa tqa) (MkRangeType tpb tqb) = do
    pba <- subtypeTypes (subtypeInverted sc) tpb tpa
    qab <- subtypeTypes sc tqa tqb
    return $ MkCatRange pba qab

subtypeArguments ::
       forall baseedit m pola polb dv gta gtb ta tb. (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext baseedit m pola polb
    -> DolanVarianceType dv
    -> DolanVarianceMap JMShim dv gta
    -> DolanVarianceMap JMShim dv gtb
    -> DolanArguments dv (PinaforeType baseedit) gta pola ta
    -> DolanArguments dv (PinaforeType baseedit) gtb polb tb
    -> m (JMShim gta gtb -> JMShim ta tb)
subtypeArguments _ NilListType NilDolanVarianceMap NilDolanVarianceMap NilDolanArguments NilDolanArguments = pure id
subtypeArguments sc (ConsListType svt dvt) (ConsDolanVarianceMap mrra svma dvma) (ConsDolanVarianceMap mrrb _ dvmb) (ConsDolanArguments sta dta) (ConsDolanArguments stb dtb) =
    case applyFunctionKindWitness (inKind @_ @gta) sta of
        Dict ->
            case applyFunctionKindWitness (inKind @_ @gtb) stb of
                Dict ->
                    case applyFunctionKindWitness (inKind @_ @gta) stb of
                        Dict ->
                            case varianceCoercibleKind svt of
                                Dict ->
                                    case dolanVarianceInCategory @JMShim dvt of
                                        Dict -> do
                                            sfunc <- subtypeVariance sc svt sta stb
                                            f <- subtypeArguments sc dvt dvma dvmb dta dtb
                                            pure $ \conv -> f (coLift mrra mrrb conv <.> svma sfunc)

pinaforeSubtypeArguments ::
       forall baseedit m pola polb pol dv gt argsa argsb. (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext baseedit m pola polb
    -> PinaforeGroundType baseedit pol dv gt
    -> DolanArguments dv (PinaforeType baseedit) gt pola argsa
    -> DolanArguments dv (PinaforeType baseedit) gt polb argsb
    -> m (JMShim argsa argsb)
pinaforeSubtypeArguments sc gt argsa argsb = let
    vkt = pinaforeGroundTypeVarianceType gt
    dvm = pinaforeGroundTypeVarianceMap gt
    in case dolanVarianceMapInKind dvm of
           Dict ->
               case dolanVarianceInCategory @JMShim vkt of
                   Dict -> fmap (\f -> f cid) $ subtypeArguments sc vkt dvm dvm argsa argsb

entitySubtypeArguments ::
       forall baseedit m pola polb dv f a b. (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext baseedit m pola polb
    -> CovaryType dv
    -> EntityGroundType f
    -> DolanArguments dv (PinaforeType baseedit) f pola a
    -> DolanArguments dv (PinaforeType baseedit) f polb b
    -> m (JMShim a b)
entitySubtypeArguments sc ct gt argsa argsb = let
    dvt = mapListType (\Refl -> CovarianceType) ct
    dvm = covaryToDolanVarianceMap ct $ entityGroundTypeCovaryMap gt
    in case dolanVarianceMapInKind dvm of
           Dict ->
               case dolanVarianceInCategory @JMShim dvt of
                   Dict -> fmap (\f -> f cid) $ subtypeArguments sc dvt dvm dvm argsa argsb

topEntityType :: forall baseedit pol. PinaforeType baseedit pol (JoinMeetType pol Entity (LimitType pol))
topEntityType =
    ConsPinaforeType
        (GroundPinaforeSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments)
        NilPinaforeType

jml1 ::
       forall pol cat t. (JoinMeetCategory cat, Is PolarityType pol)
    => cat (JoinMeetType pol t (LimitType pol)) t
jml1 =
    case representative @_ @_ @pol of
        PositiveType -> unjoin1
        NegativeType -> meet1

entityGroundSubtype ::
       forall baseedit m pola polb dva fa a dvb fb b. (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext baseedit m pola polb
    -> CovaryType dva
    -> EntityGroundType fa
    -> DolanArguments dva (PinaforeType baseedit) fa pola a
    -> CovaryType dvb
    -> EntityGroundType fb
    -> DolanArguments dvb (PinaforeType baseedit) fb polb b
    -> m (PinaforeShim a b)
entityGroundSubtype _ NilListType TopEntityGroundType NilDolanArguments NilListType TopEntityGroundType NilDolanArguments =
    pure id
entityGroundSubtype sc (ConsListType Refl NilListType) MaybeEntityGroundType (ConsDolanArguments t NilDolanArguments) NilListType TopEntityGroundType NilDolanArguments = do
    let
        convE =
            entityAdapterConvert $
            entityGroundTypeAdapter MaybeEntityGroundType $
            ConsArguments (MkEntityType TopEntityGroundType NilArguments) NilArguments
    conv <- subtypeTypes sc t $ topEntityType @baseedit @polb
    pure $ toEnhanced "subtype" convE . cfmap (jml1 @polb . conv)
entityGroundSubtype sc (ConsListType Refl NilListType) ListEntityGroundType (ConsDolanArguments t NilDolanArguments) NilListType TopEntityGroundType NilDolanArguments = do
    let
        convE =
            entityAdapterConvert $
            entityGroundTypeAdapter ListEntityGroundType $
            ConsArguments (MkEntityType TopEntityGroundType NilArguments) NilArguments
    conv <- subtypeTypes sc t $ topEntityType @baseedit @polb
    pure $ toEnhanced "subtype" convE . cfmap (jml1 @polb . conv)
entityGroundSubtype sc (ConsListType Refl (ConsListType Refl NilListType)) PairEntityGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) NilListType TopEntityGroundType NilDolanArguments = do
    let
        convE =
            entityAdapterConvert $
            entityGroundTypeAdapter PairEntityGroundType $
            ConsArguments (MkEntityType TopEntityGroundType NilArguments) $
            ConsArguments (MkEntityType TopEntityGroundType NilArguments) NilArguments
    convA <- subtypeTypes sc ta $ topEntityType @baseedit @polb
    convB <- subtypeTypes sc tb $ topEntityType @baseedit @polb
    pure $ toEnhanced "subtype" convE . apShimFuncR CovarianceType (cfmap (jml1 @polb . convA)) (jml1 @polb . convB)
entityGroundSubtype sc (ConsListType Refl (ConsListType Refl NilListType)) EitherEntityGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) NilListType TopEntityGroundType NilDolanArguments = do
    let
        convE =
            entityAdapterConvert $
            entityGroundTypeAdapter EitherEntityGroundType $
            ConsArguments (MkEntityType TopEntityGroundType NilArguments) $
            ConsArguments (MkEntityType TopEntityGroundType NilArguments) NilArguments
    convA <- subtypeTypes sc ta $ topEntityType @baseedit @polb
    convB <- subtypeTypes sc tb $ topEntityType @baseedit @polb
    pure $ toEnhanced "subtype" convE . apShimFuncR CovarianceType (cfmap (jml1 @polb . convA)) (jml1 @polb . convB)
entityGroundSubtype _ ct gt args NilListType TopEntityGroundType NilDolanArguments
    | Just ebij <- pinaforeEntityToEntityType ct gt args =
        case ebij of
            MkShimWit et conv ->
                pure $
                toEnhanced "subtype" (entityAdapterConvert (entityAdapter et)) <.>
                case representative @_ @_ @pola of
                    PositiveType -> isoForwards (unJMIsoShim conv)
                    NegativeType -> isoBackwards (unJMIsoShim conv)
entityGroundSubtype _ NilListType (LiteralEntityGroundType t1) NilDolanArguments NilListType (LiteralEntityGroundType t2) NilDolanArguments
    | Just conv <- isSubtype t1 t2 = pure conv
entityGroundSubtype _ NilListType NewEntityGroundType NilDolanArguments NilListType NewEntityGroundType NilDolanArguments =
    pure id
entityGroundSubtype _ NilListType NewEntityGroundType NilDolanArguments NilListType (OpenEntityGroundType _ _) NilDolanArguments =
    pure $ coerceEnhanced "subtype"
entityGroundSubtype sc NilListType (OpenEntityGroundType n1 t1) NilDolanArguments NilListType (OpenEntityGroundType n2 t2) NilDolanArguments =
    subtypeLift sc $ fmap (coercionEnhanced "subtype") $ getEntitySubtype n1 t1 n2 t2
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
    -> m (JMShim a b)
subtypeGroundTypes sc (EntityPinaforeGroundType cta ga) argsa (EntityPinaforeGroundType ctb gb) argsb =
    entityGroundSubtype sc cta ga argsa ctb gb argsb
subtypeGroundTypes sc ga argsa gb argsb
    | Just (Refl, HRefl) <- pinaforeGroundTypeTestEquality ga gb = pinaforeSubtypeArguments sc ga argsa argsb
subtypeGroundTypes sc ga argsa gb argsb =
    subtypeLift sc $
    convertFailure
        (unpack $ exprShow $ GroundPinaforeSingularType ga argsa)
        (unpack $ exprShow $ GroundPinaforeSingularType gb argsb)
