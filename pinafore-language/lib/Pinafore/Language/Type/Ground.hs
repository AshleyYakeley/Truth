module Pinafore.Language.Type.Ground where

import Data.Shim
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
    SimpleGroundType
        :: forall (dv :: DolanVariance) (t :: DolanVarianceKind dv).
           DolanVarianceType dv
        -> DolanVarianceMap dv t
        -> ListTypeExprShow dv
        -> ProvidedType t
        -> PinaforeGroundType dv t
    FuncPinaforeGroundType :: PinaforeGroundType '[ 'Contravariance, 'Covariance] (->)
    EntityPinaforeGroundType :: CovaryType dv -> EntityGroundType t -> PinaforeGroundType dv t
    RefOrderPinaforeGroundType :: PinaforeGroundType '[ 'Contravariance] (LangRefOrder)
    ActionPinaforeGroundType :: PinaforeGroundType '[ 'Covariance] PinaforeAction
    -- Reference
    WholeRefPinaforeGroundType :: PinaforeGroundType '[ 'Rangevariance] LangWholeRef
    TextRefPinaforeGroundType :: PinaforeGroundType '[] LangTextRef
    SetRefPinaforeGroundType :: PinaforeGroundType '[ 'Contravariance] LangSetRef
    FiniteSetRefPinaforeGroundType :: PinaforeGroundType '[ 'Rangevariance] LangFiniteSetRef
    MorphismPinaforeGroundType :: PinaforeGroundType '[ 'Rangevariance, 'Rangevariance] LangMorphism

type PinaforeTypeSystem = DolanTypeSystem PinaforeGroundType

type instance InterpreterGroundType PinaforeTypeSystem =
     PinaforeGroundType

type instance DolanPolyShim PinaforeGroundType = PinaforePolyShim

instance IsDolanGroundType PinaforeGroundType where
    type DolanName PinaforeGroundType = Name
    type DolanM PinaforeGroundType = SourceInterpreter PinaforeTypeSystem
    groundTypeVarianceMap ::
           forall (dv :: DolanVariance) (f :: DolanVarianceKind dv). PinaforeGroundType dv f -> DolanVarianceMap dv f
    groundTypeVarianceMap (SimpleGroundType _ dvm _ _) = dvm
    groundTypeVarianceMap FuncPinaforeGroundType = dolanVary @dv
    groundTypeVarianceMap (EntityPinaforeGroundType dvcovary gt) =
        covaryToDolanVarianceMap dvcovary $ groundTypeCovaryMap gt
    groundTypeVarianceMap RefOrderPinaforeGroundType = dolanVary @dv
    groundTypeVarianceMap ActionPinaforeGroundType = dolanVary @dv
    groundTypeVarianceMap WholeRefPinaforeGroundType = dolanVary @dv
    groundTypeVarianceMap TextRefPinaforeGroundType = dolanVary @dv
    groundTypeVarianceMap SetRefPinaforeGroundType = dolanVary @dv
    groundTypeVarianceMap FiniteSetRefPinaforeGroundType = dolanVary @dv
    groundTypeVarianceMap MorphismPinaforeGroundType = dolanVary @dv
    groundTypeVarianceType :: PinaforeGroundType dv t -> DolanVarianceType dv
    groundTypeVarianceType (SimpleGroundType dvt _ _ _) = dvt
    groundTypeVarianceType FuncPinaforeGroundType = representative
    groundTypeVarianceType (EntityPinaforeGroundType lt _) = mapListType (\Refl -> CovarianceType) lt
    groundTypeVarianceType RefOrderPinaforeGroundType = representative
    groundTypeVarianceType ActionPinaforeGroundType = representative
    groundTypeVarianceType WholeRefPinaforeGroundType = representative
    groundTypeVarianceType TextRefPinaforeGroundType = representative
    groundTypeVarianceType SetRefPinaforeGroundType = representative
    groundTypeVarianceType FiniteSetRefPinaforeGroundType = representative
    groundTypeVarianceType MorphismPinaforeGroundType = representative
    groundTypeTestEquality :: PinaforeGroundType dka ta -> PinaforeGroundType dkb tb -> Maybe (dka :~: dkb, ta :~~: tb)
    groundTypeTestEquality (SimpleGroundType dva _ _ ta) (SimpleGroundType dvb _ _ tb) = do
        Refl <- testEquality dva dvb
        HRefl <- testHetEquality ta tb
        Just (Refl, HRefl)
    groundTypeTestEquality FuncPinaforeGroundType FuncPinaforeGroundType = Just (Refl, HRefl)
    groundTypeTestEquality (EntityPinaforeGroundType la gta) (EntityPinaforeGroundType lb gtb) = do
        Refl <- testEquality la lb
        (HRefl, _) <- entityGroundTypeTestEquality gta gtb
        Just (Refl, HRefl)
    groundTypeTestEquality RefOrderPinaforeGroundType RefOrderPinaforeGroundType = Just (Refl, HRefl)
    groundTypeTestEquality ActionPinaforeGroundType ActionPinaforeGroundType = Just (Refl, HRefl)
    groundTypeTestEquality WholeRefPinaforeGroundType WholeRefPinaforeGroundType = Just (Refl, HRefl)
    groundTypeTestEquality TextRefPinaforeGroundType TextRefPinaforeGroundType = Just (Refl, HRefl)
    groundTypeTestEquality SetRefPinaforeGroundType SetRefPinaforeGroundType = Just (Refl, HRefl)
    groundTypeTestEquality FiniteSetRefPinaforeGroundType FiniteSetRefPinaforeGroundType = Just (Refl, HRefl)
    groundTypeTestEquality MorphismPinaforeGroundType MorphismPinaforeGroundType = Just (Refl, HRefl)
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
    groundTypeShowPrec (SimpleGroundType dv _ n _) args = showPrecDolanVariance n dv args
    groundTypeShowPrec FuncPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) =
        invertPolarity @polarity (exprPrecShow 2 ta <> " -> " <> exprPrecShow 3 tb, 3)
    groundTypeShowPrec (EntityPinaforeGroundType lt gt) dargs =
        case dolanArgumentsToArguments @PinaforePolyShim mkShimWit lt (groundTypeCovaryMap gt) dargs of
            MkShimWit args _ -> entityGroundTypeShowPrec exprShowPrec gt args
    groundTypeShowPrec RefOrderPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
        invertPolarity @polarity ("RefOrder " <> exprPrecShow 0 ta, 2)
    groundTypeShowPrec ActionPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
        ("Action " <> exprPrecShow 0 ta, 2)
    groundTypeShowPrec WholeRefPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
        ("WholeRef " <> exprPrecShow 0 ta, 2)
    groundTypeShowPrec TextRefPinaforeGroundType NilDolanArguments = ("TextRef", 0)
    groundTypeShowPrec SetRefPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
        invertPolarity @polarity ("SetRef " <> exprPrecShow 0 ta, 2)
    groundTypeShowPrec FiniteSetRefPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
        ("FiniteSetRef " <> exprPrecShow 0 ta, 2)
    groundTypeShowPrec MorphismPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) =
        invertPolarity @polarity (exprPrecShow 2 ta <> " ~> " <> exprPrecShow 3 tb, 3)

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
