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
    ConsDolanType
        (GroundDolanSingularType (EntityPinaforeGroundType NilListType TopEntityGroundType) NilDolanArguments)
        NilDolanType

entityGroundSubtype ::
       forall solver pola polb dva fa a dvb fb b.
       (WrappedApplicative solver, WAWrapper solver ~ PinaforeSourceScoped, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext PinaforeType (PinaforePolyShim Type) solver pola polb
    -> CovaryType dva
    -> EntityGroundType fa
    -> DolanArguments dva PinaforeType fa pola a
    -> CovaryType dvb
    -> EntityGroundType fb
    -> DolanArguments dvb PinaforeType fb polb b
    -> solver (PinaforePolyShim Type a b)
-- Entity <: Entity
entityGroundSubtype _ NilListType TopEntityGroundType NilDolanArguments NilListType TopEntityGroundType NilDolanArguments =
    pure id
-- Maybe Entity <: Entity
entityGroundSubtype sc (ConsListType Refl NilListType) MaybeEntityGroundType (ConsDolanArguments t NilDolanArguments) NilListType TopEntityGroundType NilDolanArguments = do
    let
        convE =
            concreteToEntityShim $
            MkConcreteType MaybeEntityGroundType $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
    conv <- subtypeConvert sc t $ topEntityType @polb
    pure $ convE . cfmap (iJoinMeetL1 @polb . conv)
-- [Entity] <: Entity
entityGroundSubtype sc (ConsListType Refl NilListType) ListEntityGroundType (ConsDolanArguments t NilDolanArguments) NilListType TopEntityGroundType NilDolanArguments = do
    let
        convE =
            concreteToEntityShim $
            MkConcreteType ListEntityGroundType $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
    conv <- subtypeConvert sc t $ topEntityType @polb
    pure $ convE . cfmap (iJoinMeetL1 @polb . conv)
-- (Entity, Entity) <: Entity
entityGroundSubtype sc (ConsListType Refl (ConsListType Refl NilListType)) PairEntityGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) NilListType TopEntityGroundType NilDolanArguments = do
    let
        convE =
            concreteToEntityShim $
            MkConcreteType PairEntityGroundType $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
    convA <- subtypeConvert sc ta $ topEntityType @polb
    convB <- subtypeConvert sc tb $ topEntityType @polb
    pure $ convE . applyCoPolyShim (cfmap (iJoinMeetL1 @polb . convA)) (iJoinMeetL1 @polb . convB)
-- Either Entity Entity <: Entity
entityGroundSubtype sc (ConsListType Refl (ConsListType Refl NilListType)) EitherEntityGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) NilListType TopEntityGroundType NilDolanArguments = do
    let
        convE =
            concreteToEntityShim $
            MkConcreteType EitherEntityGroundType $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) $
            ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
    convA <- subtypeConvert sc ta $ topEntityType @polb
    convB <- subtypeConvert sc tb $ topEntityType @polb
    pure $ convE . applyCoPolyShim (cfmap (iJoinMeetL1 @polb . convA)) (iJoinMeetL1 @polb . convB)
-- (entity type) <: Entity
entityGroundSubtype _ ct gt args NilListType TopEntityGroundType NilDolanArguments
    | Just ebij <- dolanToConcreteSimpleType ct gt args =
        case ebij of
            MkShimWit et conv -> pure $ concreteToEntityShim et <.> polarPolyIsoPositive conv
-- (literal type) <: (literal type)
entityGroundSubtype _ NilListType (LiteralEntityGroundType t1) NilDolanArguments NilListType (LiteralEntityGroundType t2) NilDolanArguments
    | Just conv <- isSubtype t1 t2 = pure conv
-- (open entity type) <: (open entity type)
entityGroundSubtype _ NilListType (OpenEntityGroundType t1) NilDolanArguments NilListType (OpenEntityGroundType t2) NilDolanArguments =
    wlift $ getOpenEntitySubtype t1 t2
-- (closed entity type) <: (closed entity type)
entityGroundSubtype _ NilListType (ClosedEntityGroundType _ sa ta) NilDolanArguments NilListType (ClosedEntityGroundType _ sb tb) NilDolanArguments
    | Just Refl <- testEquality sa sb
    , Just Refl <- testEquality ta tb = pure id
-- type conversion error
entityGroundSubtype _ cta ga argsa ctb gb argsb =
    wlift $
    convertFailure
        (exprShow $ GroundDolanSingularType (EntityPinaforeGroundType cta ga) argsa)
        (exprShow $ GroundDolanSingularType (EntityPinaforeGroundType ctb gb) argsb)

{-
finiteSetSetCE :: SubypeConversionEntry PinaforeGroundType
finiteSetSetCE =
    MkSubypeConversionEntry FiniteSetRefPinaforeGroundType SetRefPinaforeGroundType $ \_ sc (ConsDolanArguments (MkRangeType t1 _) NilDolanArguments) (ConsDolanArguments t2 NilDolanArguments) -> do
        shim <- subtypeConvert (subtypeInverted sc) t2 t1
        return $ applyContraPolyShim cid shim . functionToShim "FiniteSetRef to SetRef" langFiniteSetRefToSetRef
-}
showGroundType :: PinaforeGroundType dv gt -> Text
showGroundType t =
    newUVar "_" $ \var ->
        fst $
        saturatedGroundTypeShowPrec
            @PinaforeGroundType
            (MkAnyW $ singleDolanType @PinaforeGroundType $ VarDolanSingularType var)
            t

instance IsDolanSubtypeGroundType PinaforeGroundType where
    subtypeGroundTypes ::
           forall solver pola polb dva gta a dvb gtb b.
           ( WrappedApplicative solver
           , WAWrapper solver ~ PinaforeSourceScoped
           , Is PolarityType pola
           , Is PolarityType polb
           )
        => SubtypeContext PinaforeType (PinaforePolyShim Type) solver pola polb
        -> PinaforeGroundType dva gta
        -> DolanArguments dva PinaforeType gta pola a
        -> PinaforeGroundType dvb gtb
        -> DolanArguments dvb PinaforeType gtb polb b
        -> solver (PinaforePolyShim Type a b)
    -- f a0... <: f b0...
    subtypeGroundTypes sc ga argsa gb argsb
        | Just (Refl, HRefl) <- groundTypeTestEquality ga gb = subtypeDolanArguments sc ga argsa argsb
    -- (entity type) <: (entity type)
    subtypeGroundTypes sc (EntityPinaforeGroundType cta ga) argsa (EntityPinaforeGroundType ctb gb) argsb =
        entityGroundSubtype sc cta ga argsa ctb gb argsb
    -- FiniteSetRef -a <: SetRef a
    subtypeGroundTypes sc FiniteSetRefPinaforeGroundType (ConsDolanArguments (MkRangeType t1 _) NilDolanArguments) SetRefPinaforeGroundType (ConsDolanArguments t2 NilDolanArguments) = do
        shim <- subtypeConvert (subtypeInverted sc) t2 t1
        return $ applyContraPolyShim cid shim . functionToShim "FiniteSetRef to SetRef" langFiniteSetRefToSetRef
    -- a -> (a -> Ordering) <: RefOrder a
    subtypeGroundTypes sc FuncPinaforeGroundType (ConsDolanArguments t1 (ConsDolanArguments t2o NilDolanArguments)) RefOrderPinaforeGroundType (ConsDolanArguments a NilDolanArguments) = do
        conv1 <- subtypeConvert (subtypeInverted sc) a t1
        conv2 <-
            subtypeConvert sc t2o $
            singleDolanType $
            GroundDolanSingularType FuncPinaforeGroundType $
            ConsDolanArguments a $
            ConsDolanArguments
                (singleDolanType $
                 GroundDolanSingularType
                     (EntityPinaforeGroundType NilListType $ LiteralEntityGroundType OrderingLiteralType)
                     NilDolanArguments)
                NilDolanArguments
        return $
            (functionToShim "Order to RefOrder" pureRefOrder) .
            applyCoPolyShim
                (applyContraPolyShim cid conv1)
                (applyCoPolyShim cid (iJoinMeetL1 @polb) . iJoinMeetL1 @polb . conv2)
    -- type conversion error
    subtypeGroundTypes _ ga argsa gb argsb =
        wlift $
        convertFailure (exprShow $ GroundDolanSingularType ga argsa) (exprShow $ GroundDolanSingularType gb argsb)
    throwTypeConvertError tp tq = convertFailure (showGroundType tp) (showGroundType tq)
    throwTypeConvertInverseError tp tq = throw $ TypeConvertInverseError (exprShow tp) (exprShow tq)
    throwTypeSubsumeError ::
           forall polarity tinf tdecl a. Is PolarityType polarity
        => PinaforeSingularType polarity tinf
        -> PinaforeType polarity tdecl
        -> PinaforeSourceScoped a
    throwTypeSubsumeError tinf tdecl = let
        pol =
            case polarityType @polarity of
                PositiveType -> Positive
                NegativeType -> Negative
        in throw $ TypeSubsumeError pol (exprShow tinf) (exprShow tdecl)
    throwTypeNotInvertible t = throw $ TypeNotInvertibleError $ exprShow t

instance IsDolanFunctionGroundType PinaforeGroundType where
    functionGroundType = FuncPinaforeGroundType
