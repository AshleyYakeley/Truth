module Pinafore.Language.GroundType where

import Language.Expression.Dolan
import Language.Expression.Polarity
import Language.Expression.TypeF
import Pinafore.Base
import Pinafore.Language.EntityType
import Pinafore.Language.Morphism
import Pinafore.Language.Order
import Pinafore.Language.Reference
import Pinafore.Language.Set
import Pinafore.Language.Show
import Pinafore.Language.UI
import Shapes
import Truth.Core

{-
-- could really use https://github.com/ghc-proposals/ghc-proposals/pull/81
-}
data PinaforeGroundType baseedit (polarity :: Polarity) (dv :: DolanVariance) (t :: DolanVarianceKind dv) where
    FuncPinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Contravariance, 'Covariance] (->)
    EntityPinaforeGroundType :: CovaryType dv -> EntityGroundType t -> PinaforeGroundType baseedit polarity dv t
    OrderPinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Contravariance] (PinaforeOrder baseedit)
    ActionPinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Covariance] (PinaforeAction baseedit)
    -- Reference
    ReferencePinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Rangevariance] (PinaforeReference baseedit)
    SetPinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Rangevariance] (PinaforeSet baseedit)
    MorphismPinaforeGroundType
        :: PinaforeGroundType baseedit polarity '[ 'Rangevariance, 'Rangevariance] (PinaforeMorphism baseedit)
    -- UI
    UserInterfacePinaforeGroundType :: PinaforeGroundType baseedit polarity '[ 'Covariance] (PinaforeUI baseedit)
    WindowPinaforeGroundType :: PinaforeGroundType baseedit polarity '[] PinaforeWindow
    MenuItemPinaforeGroundType :: PinaforeGroundType baseedit polarity '[] (MenuEntry baseedit)

pinaforeGroundTypeTestEquality ::
       PinaforeGroundType baseedit pola dka ta
    -> PinaforeGroundType baseedit polb dkb tb
    -> Maybe (dka :~: dkb, ta :~~: tb)
pinaforeGroundTypeTestEquality FuncPinaforeGroundType FuncPinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality (EntityPinaforeGroundType la gta) (EntityPinaforeGroundType lb gtb) = do
    Refl <- testEquality la lb
    HRefl <- entityGroundTypeTestEquality gta gtb
    Just (Refl, HRefl)
pinaforeGroundTypeTestEquality OrderPinaforeGroundType OrderPinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality ActionPinaforeGroundType ActionPinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality ReferencePinaforeGroundType ReferencePinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality SetPinaforeGroundType SetPinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality MorphismPinaforeGroundType MorphismPinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality UserInterfacePinaforeGroundType UserInterfacePinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality WindowPinaforeGroundType WindowPinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality MenuItemPinaforeGroundType MenuItemPinaforeGroundType = Just (Refl, HRefl)
pinaforeGroundTypeTestEquality _ _ = Nothing

pinaforeGroundTypeVarianceMap ::
       forall baseedit polarity (dv :: DolanVariance) (f :: DolanVarianceKind dv).
       PinaforeGroundType baseedit polarity dv f
    -> DolanVarianceMap (->) dv f
pinaforeGroundTypeVarianceMap FuncPinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap (EntityPinaforeGroundType dvcovary gt) =
    covaryToDolanVarianceMap dvcovary $ entityGroundTypeCovaryMap gt
pinaforeGroundTypeVarianceMap OrderPinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap ActionPinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap ReferencePinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap SetPinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap MorphismPinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap UserInterfacePinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap WindowPinaforeGroundType = dolanVary @dv
pinaforeGroundTypeVarianceMap MenuItemPinaforeGroundType = dolanVary @dv

pinaforeGroundTypeVarianceType :: PinaforeGroundType baseedit polarity dv t -> DolanVarianceType dv
pinaforeGroundTypeVarianceType FuncPinaforeGroundType = representative
pinaforeGroundTypeVarianceType (EntityPinaforeGroundType lt _) = mapListType (\Refl -> CovarianceType) lt
pinaforeGroundTypeVarianceType OrderPinaforeGroundType = representative
pinaforeGroundTypeVarianceType ActionPinaforeGroundType = representative
pinaforeGroundTypeVarianceType ReferencePinaforeGroundType = representative
pinaforeGroundTypeVarianceType SetPinaforeGroundType = representative
pinaforeGroundTypeVarianceType MorphismPinaforeGroundType = representative
pinaforeGroundTypeVarianceType UserInterfacePinaforeGroundType = representative
pinaforeGroundTypeVarianceType WindowPinaforeGroundType = representative
pinaforeGroundTypeVarianceType MenuItemPinaforeGroundType = representative

pinaforeGroundTypeInvertPolarity ::
       PinaforeGroundType baseedit polarity dv t -> Maybe (PinaforeGroundType baseedit (InvertPolarity polarity) dv t)
pinaforeGroundTypeInvertPolarity FuncPinaforeGroundType = Just FuncPinaforeGroundType
pinaforeGroundTypeInvertPolarity (EntityPinaforeGroundType lc t) = Just $ EntityPinaforeGroundType lc t
pinaforeGroundTypeInvertPolarity OrderPinaforeGroundType = Just OrderPinaforeGroundType
pinaforeGroundTypeInvertPolarity ActionPinaforeGroundType = Just ActionPinaforeGroundType
pinaforeGroundTypeInvertPolarity ReferencePinaforeGroundType = Just ReferencePinaforeGroundType
pinaforeGroundTypeInvertPolarity SetPinaforeGroundType = Just SetPinaforeGroundType
pinaforeGroundTypeInvertPolarity MorphismPinaforeGroundType = Just MorphismPinaforeGroundType
pinaforeGroundTypeInvertPolarity UserInterfacePinaforeGroundType = Just UserInterfacePinaforeGroundType
pinaforeGroundTypeInvertPolarity WindowPinaforeGroundType = Just WindowPinaforeGroundType
pinaforeGroundTypeInvertPolarity MenuItemPinaforeGroundType = Just MenuItemPinaforeGroundType

pinaforeGroundTypeShowPrec ::
       forall baseedit w polarity dv f t.
       ( Is PolarityType polarity
       , forall a polarity'. Is PolarityType polarity' => ExprShow (w polarity' a)
       , forall a. ExprShow (RangeType w polarity a)
       )
    => PinaforeGroundType baseedit polarity dv f
    -> DolanArguments dv w f polarity t
    -> (Text, Int)
pinaforeGroundTypeShowPrec FuncPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) =
    invertPolarity @polarity (exprPrecShow 2 ta <> " -> " <> exprPrecShow 3 tb, 3)
pinaforeGroundTypeShowPrec (EntityPinaforeGroundType lt gt) dargs =
    case dolanArgumentsToArguments mkGenPTypeF lt (entityGroundTypeCovaryMap gt) dargs of
        MkTypeF args _ -> entityGroundTypeShowPrec exprShowPrec gt args
pinaforeGroundTypeShowPrec OrderPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    invertPolarity @polarity ("Order " <> exprPrecShow 0 ta, 2)
pinaforeGroundTypeShowPrec ActionPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    ("Action " <> exprPrecShow 0 ta, 2)
pinaforeGroundTypeShowPrec ReferencePinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    ("Ref " <> exprPrecShow 0 ta, 2)
pinaforeGroundTypeShowPrec SetPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    ("Set " <> exprPrecShow 0 ta, 2)
pinaforeGroundTypeShowPrec MorphismPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) =
    invertPolarity @polarity (exprPrecShow 2 ta <> " ~> " <> exprPrecShow 3 tb, 3)
pinaforeGroundTypeShowPrec UserInterfacePinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    ("UI " <> exprShow ta, 2)
pinaforeGroundTypeShowPrec WindowPinaforeGroundType NilDolanArguments = ("Window", 0)
pinaforeGroundTypeShowPrec MenuItemPinaforeGroundType NilDolanArguments = ("MenuItem", 0)
