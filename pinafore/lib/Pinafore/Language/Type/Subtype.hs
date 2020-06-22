{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Type.Subtype
    (
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Name
import Pinafore.Language.Scope
import Pinafore.Language.Shim
import Pinafore.Language.Type.Entity
import Pinafore.Language.Type.EntityAdapter
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Literal
import Pinafore.Language.Type.Show
import Pinafore.Language.Type.Type
import Pinafore.Language.Value
import Shapes

topEntityType :: forall pol. PinaforeType pol (JoinMeetType pol Entity (LimitType pol))
topEntityType =
    PlainDolanType $
    ConsDolanPlainType
        (GroundDolanSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments)
        NilDolanPlainType

entityGroundSubtype ::
       forall m pola polb dva fa a dvb fb b. (Applicative m, Is PolarityType pola, Is PolarityType polb)
    => MFunction PinaforeSourceScoped m
    -> SubtypeContext PinaforeType (PinaforePolyShim Type) m pola polb
    -> CovaryType dva
    -> EntityGroundType fa
    -> DolanArguments dva PinaforeType fa pola a
    -> CovaryType dvb
    -> EntityGroundType fb
    -> DolanArguments dvb PinaforeType fb polb b
    -> m (PinaforePolyShim Type a b)
-- Entity <= Entity
entityGroundSubtype _ _ NilListType TopEntityGroundType NilDolanArguments NilListType TopEntityGroundType NilDolanArguments =
    pure id
-- Maybe Entity <= Entity
entityGroundSubtype _ sc (ConsListType Refl NilListType) MaybeEntityGroundType (ConsDolanArguments t NilDolanArguments) NilListType TopEntityGroundType NilDolanArguments = do
    let
        convE =
            concreteToEntityShim $
            MkConcreteType MaybeEntityGroundType $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
    conv <- subtypeTypes sc t $ topEntityType @polb
    pure $ convE . cfmap (iJoinMeetL1 @polb . conv)
-- [Entity] <= Entity
entityGroundSubtype _ sc (ConsListType Refl NilListType) ListEntityGroundType (ConsDolanArguments t NilDolanArguments) NilListType TopEntityGroundType NilDolanArguments = do
    let
        convE =
            concreteToEntityShim $
            MkConcreteType ListEntityGroundType $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
    conv <- subtypeTypes sc t $ topEntityType @polb
    pure $ convE . cfmap (iJoinMeetL1 @polb . conv)
-- (Entity, Entity) <= Entity
entityGroundSubtype _ sc (ConsListType Refl (ConsListType Refl NilListType)) PairEntityGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) NilListType TopEntityGroundType NilDolanArguments = do
    let
        convE =
            concreteToEntityShim $
            MkConcreteType PairEntityGroundType $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
    convA <- subtypeTypes sc ta $ topEntityType @polb
    convB <- subtypeTypes sc tb $ topEntityType @polb
    pure $ convE . applyPolyShim CovarianceType (cfmap (iJoinMeetL1 @polb . convA)) (iJoinMeetL1 @polb . convB)
-- Either Entity Entity <= Entity
entityGroundSubtype _ sc (ConsListType Refl (ConsListType Refl NilListType)) EitherEntityGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) NilListType TopEntityGroundType NilDolanArguments = do
    let
        convE =
            concreteToEntityShim $
            MkConcreteType EitherEntityGroundType $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
    convA <- subtypeTypes sc ta $ topEntityType @polb
    convB <- subtypeTypes sc tb $ topEntityType @polb
    pure $ convE . applyPolyShim CovarianceType (cfmap (iJoinMeetL1 @polb . convA)) (iJoinMeetL1 @polb . convB)
-- (entity type) <= Entity
entityGroundSubtype _ _ ct gt args NilListType TopEntityGroundType NilDolanArguments
    | Just ebij <- dolanToConcreteSimpleType ct gt args =
        case ebij of
            MkShimWit et conv -> pure $ concreteToEntityShim et <.> polarPolyIsoSingle conv
-- (literal type) <= (literal type)
entityGroundSubtype _ _ NilListType (LiteralEntityGroundType t1) NilDolanArguments NilListType (LiteralEntityGroundType t2) NilDolanArguments
    | Just conv <- isSubtype t1 t2 = pure conv
-- NewEntity <= NewEntity
entityGroundSubtype _ _ NilListType NewEntityGroundType NilDolanArguments NilListType NewEntityGroundType NilDolanArguments =
    pure id
-- NewEntity <= (open entity type)
entityGroundSubtype _ _ NilListType NewEntityGroundType NilDolanArguments NilListType (OpenEntityGroundType _ _) NilDolanArguments =
    pure $ coerceEnhanced "subtype"
-- (open entity type) <= (open entity type)
entityGroundSubtype sslift _ NilListType (OpenEntityGroundType n1 t1) NilDolanArguments NilListType (OpenEntityGroundType n2 t2) NilDolanArguments =
    sslift $ getOpenEntitySubtype n1 t1 n2 t2
-- (closed entity type) <= (closed entity type)
entityGroundSubtype _ _ NilListType (ClosedEntityGroundType _ sa ta) NilDolanArguments NilListType (ClosedEntityGroundType _ sb tb) NilDolanArguments
    | Just Refl <- testEquality sa sb
    , Just Refl <- testEquality ta tb = pure id
-- type conversion error
entityGroundSubtype sslift _ cta ga argsa ctb gb argsb =
    sslift $
    convertFailure
        (exprShow $ GroundDolanSingularType (EntityPinaforeGroundType cta ga) argsa)
        (exprShow $ GroundDolanSingularType (EntityPinaforeGroundType ctb gb) argsb)

instance IsDolanSubtypeGroundType PinaforeGroundType where
    type DolanM PinaforeGroundType = PinaforeSourceScoped
    subtypeGroundTypes ::
           forall m pola polb dva gta a dvb gtb b. (Applicative m, Is PolarityType pola, Is PolarityType polb)
        => MFunction PinaforeSourceScoped m
        -> SubtypeContext PinaforeType (PinaforePolyShim Type) m pola polb
        -> PinaforeGroundType dva gta
        -> DolanArguments dva PinaforeType gta pola a
        -> PinaforeGroundType dvb gtb
        -> DolanArguments dvb PinaforeType gtb polb b
        -> m (PinaforePolyShim Type a b)
    -- f a0... <= f b0...
    subtypeGroundTypes _ sc ga argsa gb argsb
        | Just (Refl, HRefl) <- groundTypeTestEquality ga gb = subtypeDolanArguments sc ga argsa argsb
    -- (entity type) <= (entity type)
    subtypeGroundTypes sslift sc (EntityPinaforeGroundType cta ga) argsa (EntityPinaforeGroundType ctb gb) argsb =
        entityGroundSubtype sslift sc cta ga argsa ctb gb argsb
    -- FiniteSetRef -a <= SetRef a
    subtypeGroundTypes _ sc FiniteSetRefPinaforeGroundType (ConsDolanArguments (MkRangeType t1 _) NilDolanArguments) SetRefPinaforeGroundType (ConsDolanArguments t2 NilDolanArguments) = do
        shim <- subtypeTypes (subtypeInverted sc) t2 t1
        return $ toEnhanced "FiniteSetRef to SetRef" $ contramap (fromEnhanced shim) . langFiniteSetRefToSetRef
    -- type conversion error
    subtypeGroundTypes sslift _ ga argsa gb argsb =
        sslift $
        convertFailure (exprShow $ GroundDolanSingularType ga argsa) (exprShow $ GroundDolanSingularType gb argsb)
    throwTypeRecursiveError vn tp = throw $ TypeRecursiveError (symbolTypeToName vn) (exprShow tp)
    throwTypeConvertInverseError tp tq = throw $ TypeConvertInverseError (exprShow tp) (exprShow tq)
    throwTypeSubsumeError ::
           forall polarity tinf tdecl a. Is PolarityType polarity
        => PinaforeSingularType polarity tinf
        -> PinaforePlainType polarity tdecl
        -> PinaforeSourceScoped a
    throwTypeSubsumeError tinf tdecl = let
        pol =
            case polarityType @polarity of
                PositiveType -> Positive
                NegativeType -> Negative
        in throw $ TypeSubsumeError pol (exprShow tinf) (exprShow tdecl)
    throwTypeNotInvertible t = throw $ TypeNotInvertibleError $ exprShow t

type instance DolanName PinaforeGroundType = Name

instance IsDolanFunctionGroundType PinaforeGroundType where
    functionGroundType = FuncPinaforeGroundType
