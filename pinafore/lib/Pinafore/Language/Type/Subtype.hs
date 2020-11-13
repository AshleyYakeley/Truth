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

showGroundType :: PinaforeGroundType dv gt -> Text
showGroundType t =
    newUVar "_" $ \var ->
        fst $
        saturatedGroundTypeShowPrec
            @PinaforeGroundType
            (MkAnyW $ singleDolanType @PinaforeGroundType $ VarDolanSingularType var)
            t

instance IsDolanSubtypeGroundType PinaforeGroundType where
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

literalSubtypeConversionEntry ::
       LiteralType a -> LiteralType b -> PinaforePolyShim Type a b -> SubypeConversionEntry PinaforeGroundType
literalSubtypeConversionEntry ta tb conv =
    simpleSubtypeConversionEntry
        (EntityPinaforeGroundType NilListType (LiteralEntityGroundType ta))
        (EntityPinaforeGroundType NilListType (LiteralEntityGroundType tb)) $
    nilSubtypeConversion conv

instance IsDolanSubtypeEntriesGroundType PinaforeGroundType where
    subtypeConversionEntries = do
        entries <- liftSourcePos getSubtypeConversions
        return $
            entries <>
            [ simpleSubtypeConversionEntry FiniteSetRefPinaforeGroundType SetRefPinaforeGroundType $
              MkSubtypeConversion $ \_ (ConsDolanArguments (MkRangeType t _) NilDolanArguments) ->
                  return $
                  MkSubtypeArguments (ConsDolanArguments t NilDolanArguments) $
                  pure $ functionToShim "FiniteSetRef to SetRef" $ langFiniteSetRefToSetRef
            , simpleSubtypeConversionEntry
                  (EntityPinaforeGroundType (ConsListType Refl NilListType) MaybeEntityGroundType)
                  (EntityPinaforeGroundType NilListType TopEntityGroundType) $
              MkSubtypeConversion $ \(sc :: _ pola polb) (ConsDolanArguments t NilDolanArguments) ->
                  return $
                  MkSubtypeArguments NilDolanArguments $ do
                      let
                          convE =
                              concreteToEntityShim $
                              MkConcreteType MaybeEntityGroundType $
                              ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
                      conv <- subtypeConvert sc t $ topEntityType
                      pure $ convE . cfmap (iJoinMeetL1 @polb . conv)
            , simpleSubtypeConversionEntry
                  (EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType)
                  (EntityPinaforeGroundType NilListType TopEntityGroundType) $
              MkSubtypeConversion $ \(sc :: _ pola polb) (ConsDolanArguments t NilDolanArguments) ->
                  return $
                  MkSubtypeArguments NilDolanArguments $ do
                      let
                          convE =
                              concreteToEntityShim $
                              MkConcreteType ListEntityGroundType $
                              ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
                      conv <- subtypeConvert sc t $ topEntityType
                      pure $ convE . cfmap (iJoinMeetL1 @polb . conv)
            , simpleSubtypeConversionEntry
                  (EntityPinaforeGroundType (ConsListType Refl (ConsListType Refl NilListType)) PairEntityGroundType)
                  (EntityPinaforeGroundType NilListType TopEntityGroundType) $
              MkSubtypeConversion $ \(sc :: _ pola polb) (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) ->
                  return $
                  MkSubtypeArguments NilDolanArguments $ do
                      let
                          convE =
                              concreteToEntityShim $
                              MkConcreteType PairEntityGroundType $
                              ConsArguments (MkConcreteType TopEntityGroundType NilArguments) $
                              ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
                      convA <- subtypeConvert sc ta $ topEntityType
                      convB <- subtypeConvert sc tb $ topEntityType
                      pure $ convE . applyCoPolyShim (cfmap (iJoinMeetL1 @polb . convA)) (iJoinMeetL1 @polb . convB)
            , simpleSubtypeConversionEntry
                  (EntityPinaforeGroundType (ConsListType Refl (ConsListType Refl NilListType)) EitherEntityGroundType)
                  (EntityPinaforeGroundType NilListType TopEntityGroundType) $
              MkSubtypeConversion $ \(sc :: _ pola polb) (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) ->
                  return $
                  MkSubtypeArguments NilDolanArguments $ do
                      let
                          convE =
                              concreteToEntityShim $
                              MkConcreteType EitherEntityGroundType $
                              ConsArguments (MkConcreteType TopEntityGroundType NilArguments) $
                              ConsArguments (MkConcreteType TopEntityGroundType NilArguments) NilArguments
                      convA <- subtypeConvert sc ta $ topEntityType
                      convB <- subtypeConvert sc tb $ topEntityType
                      pure $ convE . applyCoPolyShim (cfmap (iJoinMeetL1 @polb . convA)) (iJoinMeetL1 @polb . convB)
            , MkSubypeConversionEntry
                  (EntityPinaforeGroundType NilListType (LiteralEntityGroundType LiteralLiteralType)) $ \case
                  EntityPinaforeGroundType NilListType (LiteralEntityGroundType t) ->
                      case literalTypeAsLiteral t of
                          Dict -> Just $ nilSubtypeConversion $ functionToShim "literal to Literal" toLiteral
                  _ -> Nothing
            , literalSubtypeConversionEntry IntegerLiteralType RationalLiteralType $
              functionToShim "Integer to Rational" integerToSafeRational
            , literalSubtypeConversionEntry RationalLiteralType NumberLiteralType $
              functionToShim "Rational to Number" safeRationalToNumber
            , MkSubypeConversionEntry (EntityPinaforeGroundType NilListType TopEntityGroundType) $ \case
                  EntityPinaforeGroundType NilListType t -> Just $ nilSubtypeConversion $ entitySubtypeShim t
                  _ -> Nothing
            , MkSubypeConversionEntry (EntityPinaforeGroundType NilListType TopDynamicEntityGroundType) $ \case
                  EntityPinaforeGroundType NilListType (ADynamicEntityGroundType _ _) -> Just $ nilSubtypeConversion id
                  _ -> Nothing
            , simpleSubtypeConversionEntry FuncPinaforeGroundType RefOrderPinaforeGroundType $
              MkSubtypeConversion $ \(sc :: _ pola polb) (ConsDolanArguments t1 (ConsDolanArguments t2o NilDolanArguments)) ->
                  invertPolarity @pola $
                  invertPolarity @polb $ do
                      MkVarType var <- varRenamerTGenerateUVar []
                      let
                          vara :: PinaforeType (InvertPolarity pola) _
                          vara = singleDolanType $ VarDolanSingularType var
                          varb :: PinaforeType (InvertPolarity polb) _
                          varb = singleDolanType $ VarDolanSingularType var
                          vconv = iJoinMeetR1 @(InvertPolarity polb) . iJoinMeetL1 @(InvertPolarity pola)
                      return $
                          MkSubtypeArguments (ConsDolanArguments vara NilDolanArguments) $ do
                              conv1 <- subtypeConvert (subtypeInverted sc) varb t1
                              conv2 <-
                                  subtypeConvert sc t2o $
                                  singleDolanType $
                                  GroundDolanSingularType FuncPinaforeGroundType $
                                  ConsDolanArguments varb $
                                  ConsDolanArguments
                                      (singleDolanType $
                                       GroundDolanSingularType
                                           (EntityPinaforeGroundType NilListType $
                                            LiteralEntityGroundType OrderingLiteralType)
                                           NilDolanArguments)
                                      NilDolanArguments
                              return $
                                  (functionToShim "Order to RefOrder" pureRefOrder) .
                                  applyCoPolyShim
                                      (applyContraPolyShim cid $ conv1 . vconv)
                                      (applyCoPolyShim (applyContraPolyShim cid vconv) (iJoinMeetL1 @polb) .
                                       iJoinMeetL1 @polb . conv2)
            ]
    throwTypeConvertError tp tq = convertFailure (showGroundType tp) (showGroundType tq)

instance IsDolanFunctionGroundType PinaforeGroundType where
    functionGroundType = FuncPinaforeGroundType
