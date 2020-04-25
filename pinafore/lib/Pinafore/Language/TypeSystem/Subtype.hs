{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.TypeSystem.Subtype
    ( SubtypeContext(..)
    , subtypeGroundTypes
    ) where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Scope
import Pinafore.Language.Type.Entity
import Pinafore.Language.Type.EntityAdapter
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Literal
import Pinafore.Language.TypeSystem.Show
import Pinafore.Language.TypeSystem.Type
import Pinafore.Language.Value
import Shapes

--import Pinafore.Language.Subtype
data SubtypeContext baseupdate m pola polb = MkSubtypeContext
    { subtypeTypes :: forall ta tb.
                              PinaforeType baseupdate pola ta -> PinaforeType baseupdate polb tb -> m (JMShim ta tb)
    , subtypeLift :: forall a. PinaforeSourceScoped baseupdate a -> m a
    , subtypeInverted :: SubtypeContext baseupdate m (InvertPolarity polb) (InvertPolarity pola)
    }

subtypeVariance ::
       (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext baseupdate m pola polb
    -> VarianceType sv
    -> SingleArgument sv (PinaforeType baseupdate) pola a
    -> SingleArgument sv (PinaforeType baseupdate) polb b
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
       forall baseupdate m pola polb dv gta gtb ta tb. (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext baseupdate m pola polb
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gta
    -> DolanVarianceMap dv gtb
    -> DolanArguments dv (PinaforeType baseupdate) gta pola ta
    -> DolanArguments dv (PinaforeType baseupdate) gtb polb tb
    -> m (JMShim gta gtb -> JMShim ta tb)
subtypeArguments _ NilListType NilDolanVarianceMap NilDolanVarianceMap NilDolanArguments NilDolanArguments = pure id
subtypeArguments sc (ConsListType svt dvt) (ConsDolanVarianceMap dvma) (ConsDolanVarianceMap dvmb) (ConsDolanArguments sta dta) (ConsDolanArguments stb dtb) =
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
                                            pure $ \conv -> f (consShimFunc svt conv sfunc)

pinaforeSubtypeArguments ::
       forall baseupdate m pola polb pol dv gt argsa argsb. (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext baseupdate m pola polb
    -> PinaforeGroundType baseupdate dv pol gt
    -> DolanArguments dv (PinaforeType baseupdate) gt pola argsa
    -> DolanArguments dv (PinaforeType baseupdate) gt polb argsb
    -> m (JMShim argsa argsb)
pinaforeSubtypeArguments sc gt argsa argsb = let
    vkt = pinaforeGroundTypeVarianceType gt
    dvm = pinaforeGroundTypeVarianceMap gt
    in case dolanVarianceMapInKind dvm of
           Dict ->
               case dolanVarianceInCategory @JMShim vkt of
                   Dict -> fmap (\f -> f cid) $ subtypeArguments sc vkt dvm dvm argsa argsb

topEntityType :: forall baseupdate pol. PinaforeType baseupdate pol (JoinMeetType pol Entity (LimitType pol))
topEntityType =
    ConsPinaforeType
        (GroundPinaforeSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments)
        NilPinaforeType

entityGroundSubtype ::
       forall baseupdate m pola polb dva fa a dvb fb b. (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext baseupdate m pola polb
    -> CovaryType dva
    -> EntityGroundType fa
    -> DolanArguments dva (PinaforeType baseupdate) fa pola a
    -> CovaryType dvb
    -> EntityGroundType fb
    -> DolanArguments dvb (PinaforeType baseupdate) fb polb b
    -> m (PinaforeShim a b)
-- Entity <= Entity
entityGroundSubtype _ NilListType TopEntityGroundType NilDolanArguments NilListType TopEntityGroundType NilDolanArguments =
    pure id
-- Maybe Entity <= Entity
entityGroundSubtype sc (ConsListType Refl NilListType) MaybeEntityGroundType (ConsDolanArguments t NilDolanArguments) NilListType TopEntityGroundType NilDolanArguments = do
    let
        convE =
            concreteToEntityShim $
            MkConcreteType MaybeEntityGroundType $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
    conv <- subtypeTypes sc t $ topEntityType @baseupdate @polb
    pure $ convE . cfmap (unjoinmeet1 @polb . conv)
-- [Entity] <= Entity
entityGroundSubtype sc (ConsListType Refl NilListType) ListEntityGroundType (ConsDolanArguments t NilDolanArguments) NilListType TopEntityGroundType NilDolanArguments = do
    let
        convE =
            concreteToEntityShim $
            MkConcreteType ListEntityGroundType $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
    conv <- subtypeTypes sc t $ topEntityType @baseupdate @polb
    pure $ convE . cfmap (unjoinmeet1 @polb . conv)
-- (Entity, Entity) <= Entity
entityGroundSubtype sc (ConsListType Refl (ConsListType Refl NilListType)) PairEntityGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) NilListType TopEntityGroundType NilDolanArguments = do
    let
        convE =
            concreteToEntityShim $
            MkConcreteType PairEntityGroundType $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
    convA <- subtypeTypes sc ta $ topEntityType @baseupdate @polb
    convB <- subtypeTypes sc tb $ topEntityType @baseupdate @polb
    pure $ convE . consShimFunc CovarianceType (cfmap (unjoinmeet1 @polb . convA)) (unjoinmeet1 @polb . convB)
-- Either Entity Entity <= Entity
entityGroundSubtype sc (ConsListType Refl (ConsListType Refl NilListType)) EitherEntityGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) NilListType TopEntityGroundType NilDolanArguments = do
    let
        convE =
            concreteToEntityShim $
            MkConcreteType EitherEntityGroundType $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
    convA <- subtypeTypes sc ta $ topEntityType @baseupdate @polb
    convB <- subtypeTypes sc tb $ topEntityType @baseupdate @polb
    pure $ convE . consShimFunc CovarianceType (cfmap (unjoinmeet1 @polb . convA)) (unjoinmeet1 @polb . convB)
-- (entity type) <= Entity
entityGroundSubtype _ ct gt args NilListType TopEntityGroundType NilDolanArguments
    | Just ebij <- pinaforeEntityToConcreteEntityType ct gt args =
        case ebij of
            MkShimWit et conv ->
                pure $
                concreteToEntityShim et <.>
                case representative @_ @_ @pola of
                    PositiveType -> isoForwards (unJMIsoShim conv)
                    NegativeType -> isoBackwards (unJMIsoShim conv)
-- (literal type) <= (literal type)
entityGroundSubtype _ NilListType (LiteralEntityGroundType t1) NilDolanArguments NilListType (LiteralEntityGroundType t2) NilDolanArguments
    | Just conv <- isSubtype t1 t2 = pure conv
-- NewEntity <= NewEntity
entityGroundSubtype _ NilListType NewEntityGroundType NilDolanArguments NilListType NewEntityGroundType NilDolanArguments =
    pure id
-- NewEntity <= (open entity type)
entityGroundSubtype _ NilListType NewEntityGroundType NilDolanArguments NilListType (OpenEntityGroundType _ _) NilDolanArguments =
    pure $ coerceEnhanced "subtype"
-- (open entity type) <= (open entity type)
entityGroundSubtype sc NilListType (OpenEntityGroundType n1 t1) NilDolanArguments NilListType (OpenEntityGroundType n2 t2) NilDolanArguments =
    subtypeLift sc $ getOpenEntitySubtype n1 t1 n2 t2
-- (closed entity type) <= (closed entity type)
entityGroundSubtype _ NilListType (ClosedEntityGroundType _ sa ta) NilDolanArguments NilListType (ClosedEntityGroundType _ sb tb) NilDolanArguments
    | Just Refl <- testEquality sa sb
    , Just Refl <- testEquality ta tb = pure id
-- type conversion error
entityGroundSubtype sc cta ga argsa ctb gb argsb =
    subtypeLift sc $
    convertFailure
        (exprShow $ GroundPinaforeSingularType (EntityPinaforeGroundType cta ga) argsa)
        (exprShow $ GroundPinaforeSingularType (EntityPinaforeGroundType ctb gb) argsb)

subtypeGroundTypes ::
       forall baseupdate m pola polb dva gta a dvb gtb b. (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext baseupdate m pola polb
    -> PinaforeGroundType baseupdate dva pola gta
    -> DolanArguments dva (PinaforeType baseupdate) gta pola a
    -> PinaforeGroundType baseupdate dvb polb gtb
    -> DolanArguments dvb (PinaforeType baseupdate) gtb polb b
    -> m (JMShim a b)
-- f a0... <= f b0...
subtypeGroundTypes sc ga argsa gb argsb
    | Just (Refl, HRefl) <- pinaforeGroundTypeTestEquality ga gb = pinaforeSubtypeArguments sc ga argsa argsb
-- (entity type) <= (entity type)
subtypeGroundTypes sc (EntityPinaforeGroundType cta ga) argsa (EntityPinaforeGroundType ctb gb) argsb =
    entityGroundSubtype sc cta ga argsa ctb gb argsb
-- FiniteSetRef -a <= SetRef a
subtypeGroundTypes sc FiniteSetRefPinaforeGroundType (ConsDolanArguments (MkRangeType t1 _) NilDolanArguments) SetRefPinaforeGroundType (ConsDolanArguments t2 NilDolanArguments) = do
    shim <- subtypeTypes (subtypeInverted sc) t2 t1
    return $ toEnhanced "FiniteSetRef to SetRef" $ contramap (fromEnhanced shim) . langFiniteSetRefToSetRef
-- type conversion error
subtypeGroundTypes sc ga argsa gb argsb =
    subtypeLift sc $
    convertFailure (exprShow $ GroundPinaforeSingularType ga argsa) (exprShow $ GroundPinaforeSingularType gb argsb)
