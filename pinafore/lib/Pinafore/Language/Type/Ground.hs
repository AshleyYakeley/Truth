module Pinafore.Language.Type.Ground where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Type.Entity
import Pinafore.Language.TypeSystem.Show
import Pinafore.Language.Value
import Shapes
import Truth.Core

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

-- could really use https://github.com/ghc-proposals/ghc-proposals/pull/81
data PinaforeGroundType (dv :: DolanVariance) (t :: DolanVarianceKind dv) where
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
    NotifierPinaforeGroundType :: PinaforeGroundType '[ 'Contravariance] LangNotifier
    WindowPinaforeGroundType :: PinaforeGroundType '[] PinaforeWindow
    MenuItemPinaforeGroundType :: PinaforeGroundType '[] MenuEntry

pinaforeGroundTypeTestEquality ::
       PinaforeGroundType dka ta -> PinaforeGroundType dkb tb -> Maybe (dka :~: dkb, ta :~~: tb)
pinaforeGroundTypeTestEquality (SimpleGroundType dva _ _ ta) (SimpleGroundType dvb _ _ tb) = do
    Refl <- testEquality dva dvb
    HRefl <- testHetEquality ta tb
    Just (Refl, HRefl)
pinaforeGroundTypeTestEquality FuncPinaforeGroundType FuncPinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality (EntityPinaforeGroundType la gta) (EntityPinaforeGroundType lb gtb) = do
    Refl <- testEquality la lb
    (HRefl, _) <- entityGroundTypeTestEquality gta gtb
    Just (Refl, HRefl)
pinaforeGroundTypeTestEquality OrderPinaforeGroundType OrderPinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality ActionPinaforeGroundType ActionPinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality RefPinaforeGroundType RefPinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality ListRefPinaforeGroundType ListRefPinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality TextRefPinaforeGroundType TextRefPinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality SetRefPinaforeGroundType SetRefPinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality FiniteSetRefPinaforeGroundType FiniteSetRefPinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality MorphismPinaforeGroundType MorphismPinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality UserInterfacePinaforeGroundType UserInterfacePinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality NotifierPinaforeGroundType NotifierPinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality WindowPinaforeGroundType WindowPinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality MenuItemPinaforeGroundType MenuItemPinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality _ _ = Nothing

pinaforeGroundTypeVarianceMap ::
       forall (dv :: DolanVariance) (f :: DolanVarianceKind dv). PinaforeGroundType dv f -> DolanVarianceMap dv f
pinaforeGroundTypeVarianceMap (SimpleGroundType _ dvm _ _) = dvm
pinaforeGroundTypeVarianceMap FuncPinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap (EntityPinaforeGroundType dvcovary gt) =
    covaryToDolanVarianceMap dvcovary $ entityGroundTypeCovaryMap gt
pinaforeGroundTypeVarianceMap OrderPinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap ActionPinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap RefPinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap ListRefPinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap TextRefPinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap SetRefPinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap FiniteSetRefPinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap MorphismPinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap UserInterfacePinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap NotifierPinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap WindowPinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap MenuItemPinaforeGroundType = dolanVary @dv

pinaforeGroundTypeVarianceType :: PinaforeGroundType dv t -> DolanVarianceType dv
pinaforeGroundTypeVarianceType (SimpleGroundType dvt _ _ _) = dvt
pinaforeGroundTypeVarianceType FuncPinaforeGroundType = representative
pinaforeGroundTypeVarianceType (EntityPinaforeGroundType lt _) = mapListType (\Refl -> CovarianceType) lt
pinaforeGroundTypeVarianceType OrderPinaforeGroundType = representative
pinaforeGroundTypeVarianceType ActionPinaforeGroundType = representative
pinaforeGroundTypeVarianceType RefPinaforeGroundType = representative
pinaforeGroundTypeVarianceType ListRefPinaforeGroundType = representative
pinaforeGroundTypeVarianceType TextRefPinaforeGroundType = representative
pinaforeGroundTypeVarianceType SetRefPinaforeGroundType = representative
pinaforeGroundTypeVarianceType FiniteSetRefPinaforeGroundType = representative
pinaforeGroundTypeVarianceType MorphismPinaforeGroundType = representative
pinaforeGroundTypeVarianceType UserInterfacePinaforeGroundType = representative
pinaforeGroundTypeVarianceType NotifierPinaforeGroundType = representative
pinaforeGroundTypeVarianceType WindowPinaforeGroundType = representative
pinaforeGroundTypeVarianceType MenuItemPinaforeGroundType = representative

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

pinaforeGroundTypeShowPrec ::
       forall w polarity dv f t.
       ( Is PolarityType polarity
       , forall a polarity'. Is PolarityType polarity' => ExprShow (w polarity' a)
       , forall a. ExprShow (RangeType w polarity a)
       )
    => PinaforeGroundType dv f
    -> DolanArguments dv w f polarity t
    -> (Text, Int)
pinaforeGroundTypeShowPrec (SimpleGroundType dv _ n _) args = showPrecDolanVariance n dv args
pinaforeGroundTypeShowPrec FuncPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) =
    invertPolarity @polarity (exprPrecShow 2 ta <> " -> " <> exprPrecShow 3 tb, 3)
pinaforeGroundTypeShowPrec (EntityPinaforeGroundType lt gt) dargs =
    case dolanArgumentsToArguments @JMShim mkPShimWit lt (entityGroundTypeCovaryMap gt) dargs of
        MkShimWit args _ -> entityGroundTypeShowPrec exprShowPrec gt args
pinaforeGroundTypeShowPrec OrderPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    invertPolarity @polarity ("Order " <> exprPrecShow 0 ta, 2)
pinaforeGroundTypeShowPrec ActionPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    ("Action " <> exprPrecShow 0 ta, 2)
pinaforeGroundTypeShowPrec RefPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    ("Ref " <> exprPrecShow 0 ta, 2)
pinaforeGroundTypeShowPrec ListRefPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    ("ListRef " <> exprPrecShow 0 ta, 2)
pinaforeGroundTypeShowPrec TextRefPinaforeGroundType NilDolanArguments = ("TextRef", 0)
pinaforeGroundTypeShowPrec SetRefPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    invertPolarity @polarity ("SetRef " <> exprPrecShow 0 ta, 2)
pinaforeGroundTypeShowPrec FiniteSetRefPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    ("FiniteSetRef " <> exprPrecShow 0 ta, 2)
pinaforeGroundTypeShowPrec MorphismPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) =
    invertPolarity @polarity (exprPrecShow 2 ta <> " ~> " <> exprPrecShow 3 tb, 3)
pinaforeGroundTypeShowPrec UserInterfacePinaforeGroundType NilDolanArguments = ("UI", 0)
pinaforeGroundTypeShowPrec NotifierPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    invertPolarity @polarity ("Notifier " <> exprShow ta, 2)
pinaforeGroundTypeShowPrec WindowPinaforeGroundType NilDolanArguments = ("Window", 0)
pinaforeGroundTypeShowPrec MenuItemPinaforeGroundType NilDolanArguments = ("MenuItem", 0)
