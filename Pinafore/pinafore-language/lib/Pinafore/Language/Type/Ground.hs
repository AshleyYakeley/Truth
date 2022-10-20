module Pinafore.Language.Type.Ground where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Language.ExprShow
import Pinafore.Language.Interpreter
import Pinafore.Language.Shim
import Pinafore.Language.Type.DynamicSupertype
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Show
import Pinafore.Language.VarID
import Pinafore.Markdown
import Shapes

type QGroundType :: GroundTypeKind
data QGroundType dv gt = MkPinaforeGroundType
    { pgtVarianceType :: DolanVarianceType dv
    , pgtVarianceMap :: DolanVarianceMap dv gt
    , pgtShowType :: ListTypeExprShow dv
    , pgtFamilyType :: FamilialType gt
    , pgtSubtypeGroup :: Maybe (SubtypeGroup QGroundType)
    , pgtGreatestDynamicSupertype :: PinaforePolyGreatestDynamicSupertype dv gt
    }

instance ExprShow (QGroundType dv gt) where
    exprShowPrec = exprShowPrecGroundType

instance Show (QGroundType dv gt) where
    show t = unpack $ showGroundType t

type PinaforePolyGreatestDynamicSupertype :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
type PinaforePolyGreatestDynamicSupertype dv gt = PolyGreatestDynamicSupertype QGroundType dv gt

type PinaforeNonpolarType :: Type -> Type
type PinaforeNonpolarType = NonpolarDolanType QGroundType

singleGroundType' ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). HasDolanVariance dv t
    => FamilialType t
    -> ListTypeExprShow dv
    -> QGroundType dv t
singleGroundType' ft showexp =
    MkPinaforeGroundType
        { pgtVarianceType = representative
        , pgtVarianceMap = dolanVarianceMap
        , pgtShowType = showexp
        , pgtFamilyType = ft
        , pgtSubtypeGroup = Nothing
        , pgtGreatestDynamicSupertype = nullPolyGreatestDynamicSupertype
        }

singleGroundType ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). HasDolanVariance dv t
    => IOWitness ('MkWitKind (SingletonFamily t))
    -> ListTypeExprShow dv
    -> QGroundType dv t
singleGroundType wit = singleGroundType' $ MkFamilialType wit HetRefl

stdSingleGroundType ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). HasDolanVariance dv t
    => IOWitness ('MkWitKind (SingletonFamily t))
    -> Text
    -> QGroundType dv t
stdSingleGroundType wit name = singleGroundType wit $ standardListTypeExprShow @dv name

type QTypeSystem = DolanTypeSystem QGroundType

type instance TSBindingData QTypeSystem = Markdown

type instance DolanPolyShim QGroundType = QPolyShim

instance IsDolanGroundType QGroundType where
    type DolanVarID QGroundType = VarID
    type DolanM QGroundType = Interpreter QTypeSystem
    groundTypeVarianceMap ::
           forall (dv :: DolanVariance) (f :: DolanVarianceKind dv). QGroundType dv f -> DolanVarianceMap dv f
    groundTypeVarianceMap = pgtVarianceMap
    groundTypeVarianceType :: QGroundType dv t -> DolanVarianceType dv
    groundTypeVarianceType = pgtVarianceType
    groundTypeTestEquality :: QGroundType dka ta -> QGroundType dkb tb -> Maybe (dka :~: dkb, ta :~~: tb)
    groundTypeTestEquality ta tb = do
        Refl <- testEquality (pgtVarianceType ta) (pgtVarianceType tb)
        HRefl <- testHetEquality (pgtFamilyType ta) (pgtFamilyType tb)
        Just (Refl, HRefl)

instance ExprShow (SomeGroundType QGroundType) where
    exprShowPrec (MkSomeGroundType t) = exprShowPrec t

showPrecVariance ::
       forall w polarity sv t.
       ( Is PolarityType polarity
       , forall a polarity'. Is PolarityType polarity' => ExprShow (w polarity' a)
       , forall a. ExprShow (RangeType w polarity a)
       )
    => CCRPolarArgument w polarity sv t
    -> (Text, Int)
showPrecVariance (CoCCRPolarArgument t) = exprShowPrec t
showPrecVariance (ContraCCRPolarArgument t) = invertPolarity @polarity $ exprShowPrec t
showPrecVariance (RangeCCRPolarArgument p q) = exprShowPrec (MkRangeType p q)

showPrecDolanVariance ::
       forall w polarity dv f t.
       ( Is PolarityType polarity
       , forall a polarity'. Is PolarityType polarity' => ExprShow (w polarity' a)
       , forall a. ExprShow (RangeType w polarity a)
       )
    => ListTypeExprShow dv
    -> DolanArguments dv w f polarity t
    -> (Text, Int)
showPrecDolanVariance f NilCCRArguments = f
showPrecDolanVariance f (ConsCCRArguments t1 tr) = showPrecDolanVariance (f (showPrecVariance @w @polarity t1)) tr

instance GroundExprShow QGroundType where
    groundTypeShowPrec ::
           forall w polarity dv f t.
           ( Is PolarityType polarity
           , forall a polarity'. Is PolarityType polarity' => ExprShow (w polarity' a)
           , forall a. ExprShow (RangeType w polarity a)
           )
        => QGroundType dv f
        -> DolanArguments dv w f polarity t
        -> (Text, Int)
    groundTypeShowPrec t args = showPrecDolanVariance (pgtShowType t) args

instance Is PolarityType polarity => Show (DolanType QGroundType polarity a) where
    show t = unpack $ exprShow t

instance Is PolarityType polarity => AllConstraint Show (DolanType QGroundType polarity) where
    allConstraint = Dict

instance Is PolarityType polarity => Show (DolanGroundedType QGroundType polarity a) where
    show t = unpack $ exprShow t

instance Is PolarityType polarity => AllConstraint Show (DolanGroundedType QGroundType polarity) where
    allConstraint = Dict
