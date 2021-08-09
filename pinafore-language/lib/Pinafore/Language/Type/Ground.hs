module Pinafore.Language.Type.Ground where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.ExprShow
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type.Entity
import Pinafore.Language.Type.EntityAdapter
import Pinafore.Language.Type.OpenEntity
import Pinafore.Language.Type.Show
import Pinafore.Language.Value
import Pinafore.Markdown
import Shapes

newtype WitKind =
    MkWitKind (forall (k :: Type). k -> Type)

data ProvidedType :: forall k. k -> Type where
    MkProvidedType
        :: forall (w :: forall k. k -> Type) (k :: Type) (t :: k). TestHetEquality w
        => IOWitness ('MkWitKind w)
        -> w t
        -> ProvidedType t

instance TestHetEquality ProvidedType where
    testHetEquality (MkProvidedType wa ta) (MkProvidedType wb tb) = do
        Refl <- testEquality wa wb
        HRefl <- testHetEquality ta tb
        return HRefl

type PinaforeGroundType :: GroundTypeKind
data PinaforeGroundType dv t where
    ProvidedGroundType
        :: forall (dv :: DolanVariance) (t :: DolanVarianceKind dv).
           DolanVarianceType dv
        -> DolanVarianceMap dv t
        -> ListTypeExprShow dv
        -> ProvidedType t
        -> PinaforeGroundType dv t
    EntityPinaforeGroundType :: CovaryType dv -> EntityGroundType t -> PinaforeGroundType dv t

singleGroundType ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). HasDolanVariance dv t
    => IOWitness ('MkWitKind (HetEqual t))
    -> ListTypeExprShow dv
    -> PinaforeGroundType dv t
singleGroundType wit showexp = ProvidedGroundType representative dolanVarianceMap showexp $ MkProvidedType wit HetRefl

stdSingleGroundType ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). HasDolanVariance dv t
    => IOWitness ('MkWitKind (HetEqual t))
    -> Text
    -> PinaforeGroundType dv t
stdSingleGroundType wit name = singleGroundType wit $ standardListTypeExprShow @dv name

actionGroundType :: PinaforeGroundType '[ 'Covariance] PinaforeAction
actionGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (HetEqual PinaforeAction)|]) "Action"

wholeRefGroundType :: PinaforeGroundType '[ 'Rangevariance] LangWholeRef
wholeRefGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (HetEqual LangWholeRef)|]) "WholeRef"

funcGroundType :: PinaforeGroundType '[ 'Contravariance, 'Covariance] (->)
funcGroundType =
    singleGroundType $(iowitness [t|'MkWitKind (HetEqual (->))|]) $ \ta tb ->
        (precShow 2 ta <> " -> " <> precShow 3 tb, 3)

listGroundType :: PinaforeGroundType '[ 'Covariance] []
listGroundType = EntityPinaforeGroundType (ConsListType Refl NilListType) ListEntityGroundType

morphismGroundType :: PinaforeGroundType '[ 'Rangevariance, 'Rangevariance] LangMorphism
morphismGroundType =
    singleGroundType $(iowitness [t|'MkWitKind (HetEqual LangMorphism)|]) $ \ta tb ->
        (precShow 2 ta <> " ~> " <> precShow 3 tb, 3)

type PinaforeTypeSystem = DolanTypeSystem PinaforeGroundType

type instance TSBindingData PinaforeTypeSystem = Markdown

type instance InterpreterGroundType PinaforeTypeSystem =
     PinaforeGroundType

type instance DolanPolyShim PinaforeGroundType = PinaforePolyShim

instance IsDolanGroundType PinaforeGroundType where
    type DolanName PinaforeGroundType = Name
    type DolanM PinaforeGroundType = SourceInterpreter PinaforeTypeSystem
    groundTypeVarianceMap ::
           forall (dv :: DolanVariance) (f :: DolanVarianceKind dv). PinaforeGroundType dv f -> DolanVarianceMap dv f
    groundTypeVarianceMap (ProvidedGroundType _ dvm _ _) = dvm
    groundTypeVarianceMap (EntityPinaforeGroundType dvcovary gt) =
        covaryToDolanVarianceMap dvcovary $ groundTypeCovaryMap gt
    groundTypeVarianceType :: PinaforeGroundType dv t -> DolanVarianceType dv
    groundTypeVarianceType (ProvidedGroundType dvt _ _ _) = dvt
    groundTypeVarianceType (EntityPinaforeGroundType lt _) = mapListType (\Refl -> CovarianceType) lt
    groundTypeTestEquality :: PinaforeGroundType dka ta -> PinaforeGroundType dkb tb -> Maybe (dka :~: dkb, ta :~~: tb)
    groundTypeTestEquality (ProvidedGroundType dva _ _ ta) (ProvidedGroundType dvb _ _ tb) = do
        Refl <- testEquality dva dvb
        HRefl <- testHetEquality ta tb
        Just (Refl, HRefl)
    groundTypeTestEquality (EntityPinaforeGroundType la gta) (EntityPinaforeGroundType lb gtb) = do
        Refl <- testEquality la lb
        (HRefl, _) <- entityGroundTypeTestEquality gta gtb
        Just (Refl, HRefl)
    groundTypeTestEquality _ _ = Nothing

instance CovarySubtype PinaforeGroundType EntityGroundType where
    dolanToMonoGroundType :: forall dv t. PinaforeGroundType dv t -> Maybe (CovaryType dv, EntityGroundType t)
    dolanToMonoGroundType (EntityPinaforeGroundType lc et) = Just (lc, et)
    dolanToMonoGroundType _ = Nothing
    monoToDolanGroundType :: forall dv t. CovaryType dv -> EntityGroundType t -> PinaforeGroundType dv t
    monoToDolanGroundType = EntityPinaforeGroundType

showPrecVariance ::
       forall w polarity sv t.
       ( Is PolarityType polarity
       , forall a polarity'. Is PolarityType polarity' => ExprShow (w polarity' a)
       , forall a. ExprShow (RangeType w polarity a)
       )
    => VarianceType sv
    -> SingleArgument sv w polarity t
    -> (Text, Int)
showPrecVariance CovarianceType t = exprShowPrec t
showPrecVariance ContravarianceType t = invertPolarity @polarity $ exprShowPrec t
showPrecVariance RangevarianceType t = exprShowPrec t

showPrecDolanVariance ::
       forall w polarity dv f t.
       ( Is PolarityType polarity
       , forall a polarity'. Is PolarityType polarity' => ExprShow (w polarity' a)
       , forall a. ExprShow (RangeType w polarity a)
       )
    => ListTypeExprShow dv
    -> DolanVarianceType dv
    -> DolanArguments dv w f polarity t
    -> (Text, Int)
showPrecDolanVariance f NilListType NilDolanArguments = f
showPrecDolanVariance f (ConsListType sv dv) (ConsDolanArguments t1 tr) =
    showPrecDolanVariance (f (showPrecVariance @w @polarity sv t1)) dv tr

instance GroundExprShow PinaforeGroundType where
    groundTypeShowPrec ::
           forall w polarity dv f t.
           ( Is PolarityType polarity
           , forall a polarity'. Is PolarityType polarity' => ExprShow (w polarity' a)
           , forall a. ExprShow (RangeType w polarity a)
           )
        => PinaforeGroundType dv f
        -> DolanArguments dv w f polarity t
        -> (Text, Int)
    groundTypeShowPrec (ProvidedGroundType dv _ n _) args = showPrecDolanVariance n dv args
    groundTypeShowPrec (EntityPinaforeGroundType lt gt) dargs =
        case dolanArgumentsToArguments @PinaforePolyShim mkPolarShimWit lt (groundTypeCovaryMap gt) dargs of
            MkShimWit args _ -> entityGroundTypeShowPrec exprShowPrec gt args

withEntitySubtype ::
       forall (t :: Type) tidb a.
       EntityGroundType t
    -> OpenEntityType tidb
    -> Interpreter PinaforeTypeSystem a
    -> Interpreter PinaforeTypeSystem a
withEntitySubtype ta tb =
    withSubtypeConversions $
    pure $
    simpleSubtypeConversionEntry
        (EntityPinaforeGroundType NilListType ta)
        (EntityPinaforeGroundType NilListType $ OpenEntityGroundType tb) $
    nilSubtypeConversion $ coerceEnhanced "open entity" . entitySubtypeShim ta

instance Is PolarityType polarity => Show (DolanType PinaforeGroundType polarity a) where
    show t = unpack $ exprShow t

instance Is PolarityType polarity => AllWitnessConstraint Show (DolanType PinaforeGroundType polarity) where
    allWitnessConstraint = Dict
