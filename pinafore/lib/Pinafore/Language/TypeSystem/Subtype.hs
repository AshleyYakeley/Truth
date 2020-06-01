{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.TypeSystem.Subtype
    ( SubtypeContext(..)
    , subtypeGroundTypes
    ) where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Scope
import Pinafore.Language.Shim
import Pinafore.Language.Type.Entity
import Pinafore.Language.Type.EntityAdapter
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Literal
import Pinafore.Language.TypeSystem.Show
import Pinafore.Language.TypeSystem.Type
import Pinafore.Language.Value
import Shapes

--import Pinafore.Language.Subtype
data SubtypeContext m pola polb = MkSubtypeContext
    { subtypeTypes :: forall ta tb. PinaforeType pola ta -> PinaforeType polb tb -> m (PinaforeShim Type ta tb)
    , subtypeLift :: forall a. PinaforeSourceScoped a -> m a
    , subtypeInverted :: SubtypeContext m (InvertPolarity polb) (InvertPolarity pola)
    }

subtypeVariance ::
       (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext m pola polb
    -> VarianceType sv
    -> SingleArgument sv PinaforeType pola a
    -> SingleArgument sv PinaforeType polb b
    -> m (VarianceCategory (PinaforeShim Type) sv a b)
subtypeVariance sc CovarianceType ta tb = subtypeTypes sc ta tb
subtypeVariance sc ContravarianceType ta tb = do
    ba <- subtypeTypes (subtypeInverted sc) tb ta
    return $ MkCatDual ba
subtypeVariance sc RangevarianceType (MkRangeType tpa tqa) (MkRangeType tpb tqb) = do
    pba <- subtypeTypes (subtypeInverted sc) tpb tpa
    qab <- subtypeTypes sc tqa tqb
    return $ MkCatRange pba qab

subtypeArguments ::
       forall m pola polb dv (gta :: DolanVarianceKind dv) (gtb :: DolanVarianceKind dv) ta tb.
       (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext m pola polb
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gta
    -> DolanVarianceMap dv gtb
    -> DolanArguments dv PinaforeType gta pola ta
    -> DolanArguments dv PinaforeType gtb polb tb
    -> m (PinaforeShim (DolanVarianceKind dv) gta gtb -> PinaforeShim Type ta tb)
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
                                    case dolanVarianceInCategory @PinaforeShim dvt of
                                        Dict -> do
                                            sfunc <- subtypeVariance sc svt sta stb
                                            f <- subtypeArguments sc dvt dvma dvmb dta dtb
                                            pure $ \conv -> f (applyPolyShim svt conv sfunc)

pinaforeSubtypeArguments ::
       forall m pola polb dv gt argsa argsb. (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext m pola polb
    -> PinaforeGroundType dv gt
    -> DolanArguments dv PinaforeType gt pola argsa
    -> DolanArguments dv PinaforeType gt polb argsb
    -> m (PinaforeShim Type argsa argsb)
pinaforeSubtypeArguments sc gt argsa argsb = let
    vkt = pinaforeGroundTypeVarianceType gt
    dvm = pinaforeGroundTypeVarianceMap gt
    in case dolanVarianceMapInKind dvm of
           Dict ->
               case dolanVarianceInCategory @PinaforeShim vkt of
                   Dict -> fmap (\f -> f cid) $ subtypeArguments sc vkt dvm dvm argsa argsb

topEntityType :: forall pol. PinaforeType pol (JoinMeetType pol Entity (LimitType pol))
topEntityType =
    ConsPinaforeType
        (GroundPinaforeSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments)
        NilPinaforeType

entityGroundSubtype ::
       forall m pola polb dva fa a dvb fb b. (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext m pola polb
    -> CovaryType dva
    -> EntityGroundType fa
    -> DolanArguments dva PinaforeType fa pola a
    -> CovaryType dvb
    -> EntityGroundType fb
    -> DolanArguments dvb PinaforeType fb polb b
    -> m (PinaforeShim Type a b)
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
    conv <- subtypeTypes sc t $ topEntityType @polb
    pure $ convE . cfmap (unjoinmeet1 @polb . conv)
-- [Entity] <= Entity
entityGroundSubtype sc (ConsListType Refl NilListType) ListEntityGroundType (ConsDolanArguments t NilDolanArguments) NilListType TopEntityGroundType NilDolanArguments = do
    let
        convE =
            concreteToEntityShim $
            MkConcreteType ListEntityGroundType $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
    conv <- subtypeTypes sc t $ topEntityType @polb
    pure $ convE . cfmap (unjoinmeet1 @polb . conv)
-- (Entity, Entity) <= Entity
entityGroundSubtype sc (ConsListType Refl (ConsListType Refl NilListType)) PairEntityGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) NilListType TopEntityGroundType NilDolanArguments = do
    let
        convE =
            concreteToEntityShim $
            MkConcreteType PairEntityGroundType $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
    convA <- subtypeTypes sc ta $ topEntityType @polb
    convB <- subtypeTypes sc tb $ topEntityType @polb
    pure $ convE . applyPolyShim CovarianceType (cfmap (unjoinmeet1 @polb . convA)) (unjoinmeet1 @polb . convB)
-- Either Entity Entity <= Entity
entityGroundSubtype sc (ConsListType Refl (ConsListType Refl NilListType)) EitherEntityGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) NilListType TopEntityGroundType NilDolanArguments = do
    let
        convE =
            concreteToEntityShim $
            MkConcreteType EitherEntityGroundType $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
    convA <- subtypeTypes sc ta $ topEntityType @polb
    convB <- subtypeTypes sc tb $ topEntityType @polb
    pure $ convE . applyPolyShim CovarianceType (cfmap (unjoinmeet1 @polb . convA)) (unjoinmeet1 @polb . convB)
-- (entity type) <= Entity
entityGroundSubtype _ ct gt args NilListType TopEntityGroundType NilDolanArguments
    | Just ebij <- pinaforeEntityToConcreteEntityType ct gt args =
        case ebij of
            MkShimWit et conv -> pure $ concreteToEntityShim et <.> polarPolyIsoSingle conv
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
       forall m pola polb dva gta a dvb gtb b. (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext m pola polb
    -> PinaforeGroundType dva gta
    -> DolanArguments dva PinaforeType gta pola a
    -> PinaforeGroundType dvb gtb
    -> DolanArguments dvb PinaforeType gtb polb b
    -> m (PinaforeShim Type a b)
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
