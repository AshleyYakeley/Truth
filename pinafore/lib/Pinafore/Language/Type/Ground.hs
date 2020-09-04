module Pinafore.Language.Type.Ground where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Name
import Pinafore.Language.Scope
import Pinafore.Language.Shim
import Pinafore.Language.Type.Entity
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

instance TestHetEquality (ProvidedType) where
    testHetEquality (MkProvidedType wa ta) (MkProvidedType wb tb) = do
        Refl <- testEquality wa wb
        HRefl <- testHetEquality ta tb
        return HRefl

type PinaforeGroundType :: GroundTypeKind
data PinaforeGroundType dv t where
    -- a simple ground type is one with no special subtype relationships
    SimpleGroundType
        :: forall (dv :: DolanVariance) (t :: DolanVarianceKind dv).
           DolanVarianceType dv
        -> DolanVarianceMap dv t
        -> ListTypeExprShow dv
        -> ProvidedType t
        -> PinaforeGroundType dv t
    FuncPinaforeGroundType :: PinaforeGroundType '[ 'Contravariance, 'Covariance] (->)
    EntityPinaforeGroundType :: CovaryType dv -> EntityGroundType t -> PinaforeGroundType dv t
    OrderPinaforeGroundType :: PinaforeGroundType '[ 'Contravariance] (LangOrder)
    ActionPinaforeGroundType :: PinaforeGroundType '[ 'Covariance] PinaforeAction
    -- Reference
    RefPinaforeGroundType :: PinaforeGroundType '[ 'Rangevariance] LangRef
    ListRefPinaforeGroundType :: PinaforeGroundType '[ 'Rangevariance] LangListRef
    TextRefPinaforeGroundType :: PinaforeGroundType '[] LangTextRef
    SetRefPinaforeGroundType :: PinaforeGroundType '[ 'Contravariance] LangSetRef
    FiniteSetRefPinaforeGroundType :: PinaforeGroundType '[ 'Rangevariance] LangFiniteSetRef
    MorphismPinaforeGroundType :: PinaforeGroundType '[ 'Rangevariance, 'Rangevariance] LangMorphism
    -- UI
    UserInterfacePinaforeGroundType :: PinaforeGroundType '[] LangUI
    WindowPinaforeGroundType :: PinaforeGroundType '[] LangWindow
    MenuItemPinaforeGroundType :: PinaforeGroundType '[] LangMenuEntry

type PinaforeTypeSystem = DolanTypeSystem PinaforeGroundType

type instance DolanPolyShim PinaforeGroundType = PinaforePolyShim

instance IsDolanGroundType PinaforeGroundType where
    type DolanName PinaforeGroundType = Name
    type DolanM PinaforeGroundType = SourceScoped PinaforeTypeSystem
    groundTypeVarianceMap ::
           forall (dv :: DolanVariance) (f :: DolanVarianceKind dv). PinaforeGroundType dv f -> DolanVarianceMap dv f
    groundTypeVarianceMap (SimpleGroundType _ dvm _ _) = dvm
    groundTypeVarianceMap FuncPinaforeGroundType = dolanVary @dv
    groundTypeVarianceMap (EntityPinaforeGroundType dvcovary gt) =
        covaryToDolanVarianceMap dvcovary $ groundTypeCovaryMap gt
    groundTypeVarianceMap OrderPinaforeGroundType = dolanVary @dv
    groundTypeVarianceMap ActionPinaforeGroundType = dolanVary @dv
    groundTypeVarianceMap RefPinaforeGroundType = dolanVary @dv
    groundTypeVarianceMap ListRefPinaforeGroundType = dolanVary @dv
    groundTypeVarianceMap TextRefPinaforeGroundType = dolanVary @dv
    groundTypeVarianceMap SetRefPinaforeGroundType = dolanVary @dv
    groundTypeVarianceMap FiniteSetRefPinaforeGroundType = dolanVary @dv
    groundTypeVarianceMap MorphismPinaforeGroundType = dolanVary @dv
    groundTypeVarianceMap UserInterfacePinaforeGroundType = dolanVary @dv
    groundTypeVarianceMap WindowPinaforeGroundType = dolanVary @dv
    groundTypeVarianceMap MenuItemPinaforeGroundType = dolanVary @dv
    groundTypeVarianceType :: PinaforeGroundType dv t -> DolanVarianceType dv
    groundTypeVarianceType (SimpleGroundType dvt _ _ _) = dvt
    groundTypeVarianceType FuncPinaforeGroundType = representative
    groundTypeVarianceType (EntityPinaforeGroundType lt _) = mapListType (\Refl -> CovarianceType) lt
    groundTypeVarianceType OrderPinaforeGroundType = representative
    groundTypeVarianceType ActionPinaforeGroundType = representative
    groundTypeVarianceType RefPinaforeGroundType = representative
    groundTypeVarianceType ListRefPinaforeGroundType = representative
    groundTypeVarianceType TextRefPinaforeGroundType = representative
    groundTypeVarianceType SetRefPinaforeGroundType = representative
    groundTypeVarianceType FiniteSetRefPinaforeGroundType = representative
    groundTypeVarianceType MorphismPinaforeGroundType = representative
    groundTypeVarianceType UserInterfacePinaforeGroundType = representative
    groundTypeVarianceType WindowPinaforeGroundType = representative
    groundTypeVarianceType MenuItemPinaforeGroundType = representative
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
    groundTypeTestEquality OrderPinaforeGroundType OrderPinaforeGroundType = Just (Refl, HRefl)
    groundTypeTestEquality ActionPinaforeGroundType ActionPinaforeGroundType = Just (Refl, HRefl)
    groundTypeTestEquality RefPinaforeGroundType RefPinaforeGroundType = Just (Refl, HRefl)
    groundTypeTestEquality ListRefPinaforeGroundType ListRefPinaforeGroundType = Just (Refl, HRefl)
    groundTypeTestEquality TextRefPinaforeGroundType TextRefPinaforeGroundType = Just (Refl, HRefl)
    groundTypeTestEquality SetRefPinaforeGroundType SetRefPinaforeGroundType = Just (Refl, HRefl)
    groundTypeTestEquality FiniteSetRefPinaforeGroundType FiniteSetRefPinaforeGroundType = Just (Refl, HRefl)
    groundTypeTestEquality MorphismPinaforeGroundType MorphismPinaforeGroundType = Just (Refl, HRefl)
    groundTypeTestEquality UserInterfacePinaforeGroundType UserInterfacePinaforeGroundType = Just (Refl, HRefl)
    groundTypeTestEquality WindowPinaforeGroundType WindowPinaforeGroundType = Just (Refl, HRefl)
    groundTypeTestEquality MenuItemPinaforeGroundType MenuItemPinaforeGroundType = Just (Refl, HRefl)
    groundTypeTestEquality _ _ = Nothing

instance CovarySubtype PinaforeGroundType EntityGroundType where
    dolanToConcreteGroundType :: forall dv t. PinaforeGroundType dv t -> Maybe (CovaryType dv, EntityGroundType t)
    dolanToConcreteGroundType (EntityPinaforeGroundType lc et) = Just (lc, et)
    dolanToConcreteGroundType _ = Nothing
    concreteToDolanGroundType :: forall dv t. CovaryType dv -> EntityGroundType t -> PinaforeGroundType dv t
    concreteToDolanGroundType = EntityPinaforeGroundType

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
    groundTypeShowPrec OrderPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
        invertPolarity @polarity ("Order " <> exprPrecShow 0 ta, 2)
    groundTypeShowPrec ActionPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
        ("Action " <> exprPrecShow 0 ta, 2)
    groundTypeShowPrec RefPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
        ("Ref " <> exprPrecShow 0 ta, 2)
    groundTypeShowPrec ListRefPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
        ("ListRef " <> exprPrecShow 0 ta, 2)
    groundTypeShowPrec TextRefPinaforeGroundType NilDolanArguments = ("TextRef", 0)
    groundTypeShowPrec SetRefPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
        invertPolarity @polarity ("SetRef " <> exprPrecShow 0 ta, 2)
    groundTypeShowPrec FiniteSetRefPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
        ("FiniteSetRef " <> exprPrecShow 0 ta, 2)
    groundTypeShowPrec MorphismPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) =
        invertPolarity @polarity (exprPrecShow 2 ta <> " ~> " <> exprPrecShow 3 tb, 3)
    groundTypeShowPrec UserInterfacePinaforeGroundType NilDolanArguments = ("UI", 0)
    groundTypeShowPrec WindowPinaforeGroundType NilDolanArguments = ("Window", 0)
    groundTypeShowPrec MenuItemPinaforeGroundType NilDolanArguments = ("MenuItem", 0)
