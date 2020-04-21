module Pinafore.Language.Type.Ground where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Name
import Pinafore.Language.Type.Entity
import Pinafore.Language.Type.TypeID
import Pinafore.Language.TypeSystem.Show
import Pinafore.Language.Value
import Shapes
import Truth.Core

-- could really use https://github.com/ghc-proposals/ghc-proposals/pull/81
data PinaforeGroundType baseupdate (dv :: DolanVariance) (polarity :: Polarity) (t :: DolanVarianceKind dv) where
    DataGroundType
        :: Name
        -> TypeIDType tid
        -> DataType baseupdate t
        -> PinaforeGroundType baseupdate '[] polarity (IdentifiedValue tid t)
    FuncPinaforeGroundType :: PinaforeGroundType baseupdate '[ 'Contravariance, 'Covariance] polarity (->)
    EntityPinaforeGroundType :: CovaryType dv -> EntityGroundType t -> PinaforeGroundType baseupdate dv polarity t
    OrderPinaforeGroundType :: PinaforeGroundType baseupdate '[ 'Contravariance] polarity (LangOrder baseupdate)
    ActionPinaforeGroundType :: PinaforeGroundType baseupdate '[ 'Covariance] polarity PinaforeAction
    -- Reference
    RefPinaforeGroundType :: PinaforeGroundType baseupdate '[ 'Rangevariance] polarity LangRef
    ListRefPinaforeGroundType :: PinaforeGroundType baseupdate '[ 'Rangevariance] polarity LangListRef
    TextRefPinaforeGroundType :: PinaforeGroundType baseupdate '[] polarity LangTextRef
    SetRefPinaforeGroundType :: PinaforeGroundType baseupdate '[ 'Contravariance] polarity LangSetRef
    FiniteSetRefPinaforeGroundType :: PinaforeGroundType baseupdate '[ 'Rangevariance] polarity LangFiniteSetRef
    MorphismPinaforeGroundType
        :: PinaforeGroundType baseupdate '[ 'Rangevariance, 'Rangevariance] polarity (LangMorphism baseupdate)
    -- UI
    UserInterfacePinaforeGroundType :: PinaforeGroundType baseupdate '[] polarity LangUI
    NotifierPinaforeGroundType :: PinaforeGroundType baseupdate '[ 'Contravariance] polarity LangNotifier
    WindowPinaforeGroundType :: PinaforeGroundType baseupdate '[] polarity PinaforeWindow
    MenuItemPinaforeGroundType :: PinaforeGroundType baseupdate '[] polarity MenuEntry

newtype AnyPolarity (f :: k -> Type) (polarity :: Polarity) (t :: k) =
    MkAnyPolarity (f t)

newtype AllPolarity (f :: Polarity -> k -> Type) (t :: k) =
    MkAllPolarity (forall polarity. Is PolarityType polarity => f polarity t)

instance TestEquality f => TestEquality (AnyPolarity f polarity) where
    testEquality (MkAnyPolarity fta) (MkAnyPolarity ftb) = testEquality fta ftb

instance TestEquality (f 'Positive) => TestEquality (AllPolarity f) where
    testEquality (MkAllPolarity fpta) (MkAllPolarity fptb) = do
        Refl <- testEquality (fpta @'Positive) (fptb @'Positive)
        return Refl

data ConcretePinaforeType baseupdate t where
    MkConcretePinaforeType
        :: AllPolarity (PinaforeGroundType baseupdate dv) gt
        -> AllPolarity (DolanArguments dv (AnyPolarity (ConcretePinaforeType baseupdate)) gt) t
        -> ConcretePinaforeType baseupdate t

instance TestEquality (ConcretePinaforeType baseupdate) where
    testEquality (MkConcretePinaforeType (MkAllPolarity gta) (MkAllPolarity ta)) (MkConcretePinaforeType (MkAllPolarity gtb) (MkAllPolarity tb)) = do
        (Refl, HRefl) <- pinaforeGroundTypeTestEquality (gta @'Positive) (gtb @'Positive)
        Refl <- dolanTestEquality (pinaforeGroundTypeVarianceType (gta @'Positive)) (ta @'Positive) tb
        return Refl

data DataType baseupdate (t :: Type) where
    NilDataType :: DataType baseupdate None
    ConsDataType
        :: ListType (ConcretePinaforeType baseupdate) tl
        -> DataType baseupdate tt
        -> DataType baseupdate (Either (HList tl) tt)

instance TestEquality (DataType baseupdate) where
    testEquality NilDataType NilDataType = Just Refl
    testEquality (ConsDataType l1 t1) (ConsDataType l2 t2) = do
        Refl <- testEquality l1 l2
        Refl <- testEquality t1 t2
        return Refl
    testEquality _ _ = Nothing

pinaforeGroundTypeTestEquality ::
       PinaforeGroundType baseupdate dka pola ta
    -> PinaforeGroundType baseupdate dkb polb tb
    -> Maybe (dka :~: dkb, ta :~~: tb)
pinaforeGroundTypeTestEquality (DataGroundType _ ta dta) (DataGroundType _ tb dtb) = do
    Refl <- testEquality ta tb
    Refl <- testEquality dta dtb
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
       forall baseupdate polarity (dv :: DolanVariance) (f :: DolanVarianceKind dv).
       PinaforeGroundType baseupdate dv polarity f
    -> DolanVarianceMap JMShim dv f
pinaforeGroundTypeVarianceMap (DataGroundType _ _ _) = dolanVary @dv
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

pinaforeGroundTypeVarianceType :: PinaforeGroundType baseupdate dv polarity t -> DolanVarianceType dv
pinaforeGroundTypeVarianceType (DataGroundType _ _ _) = representative
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

pinaforeGroundTypeInvertPolarity ::
       PinaforeGroundType baseupdate dv polarity t
    -> Maybe (PinaforeGroundType baseupdate dv (InvertPolarity polarity) t)
pinaforeGroundTypeInvertPolarity (DataGroundType n tid w) = Just $ DataGroundType n tid w
pinaforeGroundTypeInvertPolarity FuncPinaforeGroundType = Just FuncPinaforeGroundType
pinaforeGroundTypeInvertPolarity (EntityPinaforeGroundType lc t) = Just $ EntityPinaforeGroundType lc t
pinaforeGroundTypeInvertPolarity OrderPinaforeGroundType = Just OrderPinaforeGroundType
pinaforeGroundTypeInvertPolarity ActionPinaforeGroundType = Just ActionPinaforeGroundType
pinaforeGroundTypeInvertPolarity RefPinaforeGroundType = Just RefPinaforeGroundType
pinaforeGroundTypeInvertPolarity ListRefPinaforeGroundType = Just ListRefPinaforeGroundType
pinaforeGroundTypeInvertPolarity TextRefPinaforeGroundType = Just TextRefPinaforeGroundType
pinaforeGroundTypeInvertPolarity SetRefPinaforeGroundType = Just SetRefPinaforeGroundType
pinaforeGroundTypeInvertPolarity FiniteSetRefPinaforeGroundType = Just FiniteSetRefPinaforeGroundType
pinaforeGroundTypeInvertPolarity MorphismPinaforeGroundType = Just MorphismPinaforeGroundType
pinaforeGroundTypeInvertPolarity UserInterfacePinaforeGroundType = Just UserInterfacePinaforeGroundType
pinaforeGroundTypeInvertPolarity NotifierPinaforeGroundType = Just NotifierPinaforeGroundType
pinaforeGroundTypeInvertPolarity WindowPinaforeGroundType = Just WindowPinaforeGroundType
pinaforeGroundTypeInvertPolarity MenuItemPinaforeGroundType = Just MenuItemPinaforeGroundType

pinaforeGroundTypeShowPrec ::
       forall baseupdate w polarity dv f t.
       ( Is PolarityType polarity
       , forall a polarity'. Is PolarityType polarity' => ExprShow (w polarity' a)
       , forall a. ExprShow (RangeType w polarity a)
       )
    => PinaforeGroundType baseupdate dv polarity f
    -> DolanArguments dv w f polarity t
    -> (Text, Int)
pinaforeGroundTypeShowPrec (DataGroundType n _ _) NilDolanArguments = exprShowPrec n
pinaforeGroundTypeShowPrec FuncPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) =
    invertPolarity @polarity (exprPrecShow 2 ta <> " -> " <> exprPrecShow 3 tb, 3)
pinaforeGroundTypeShowPrec (EntityPinaforeGroundType lt gt) dargs =
    case dolanArgumentsToArguments mkPShimWit lt (entityGroundTypeCovaryMap gt) dargs of
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
