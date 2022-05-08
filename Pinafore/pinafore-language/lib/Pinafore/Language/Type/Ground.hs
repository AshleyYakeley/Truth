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

{-
data SubtypeGroup = MkSubtypeGroup
    {
        sgIdentifer :: Unique,
        sgSubtype ::
            FamilialType gta -> FamilialType gtb -> Maybe (FamilialType gtb)
    }
-}
type PinaforeGroundType :: GroundTypeKind
data PinaforeGroundType dv gt = MkPinaforeGroundType
    { pgtVarianceType :: DolanVarianceType dv
    , pgtVarianceMap :: DolanVarianceMap dv gt
    , pgtShowType :: ListTypeExprShow dv
    , pgtFamilyType :: FamilialType gt
    , pgtGreatestDynamicSupertype :: PinaforePolyGreatestDynamicSupertype dv gt
    }

type PinaforePolyGreatestDynamicSupertype :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
type PinaforePolyGreatestDynamicSupertype dv gt = PolyGreatestDynamicSupertype PinaforeGroundType dv gt

type PinaforeNonpolarType :: Type -> Type
type PinaforeNonpolarType = NonpolarDolanType PinaforeGroundType

singleGroundType' ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). HasDolanVariance dv t
    => FamilialType t
    -> ListTypeExprShow dv
    -> PinaforeGroundType dv t
singleGroundType' ft showexp =
    MkPinaforeGroundType
        { pgtVarianceType = representative
        , pgtVarianceMap = dolanVarianceMap
        , pgtShowType = showexp
        , pgtFamilyType = ft
        , pgtGreatestDynamicSupertype = \_ -> Nothing
        }

singleGroundType ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). HasDolanVariance dv t
    => IOWitness ('MkWitKind (SingletonFamily t))
    -> ListTypeExprShow dv
    -> PinaforeGroundType dv t
singleGroundType wit = singleGroundType' $ MkFamilialType wit HetRefl

stdSingleGroundType ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). HasDolanVariance dv t
    => IOWitness ('MkWitKind (SingletonFamily t))
    -> Text
    -> PinaforeGroundType dv t
stdSingleGroundType wit name = singleGroundType wit $ standardListTypeExprShow @dv name

type PinaforeTypeSystem = DolanTypeSystem PinaforeGroundType

type instance TSBindingData PinaforeTypeSystem = Markdown

type instance InterpreterGroundType PinaforeTypeSystem =
     PinaforeGroundType

type instance DolanPolyShim PinaforeGroundType = PinaforePolyShim

instance IsDolanGroundType PinaforeGroundType where
    type DolanVarID PinaforeGroundType = VarID
    type DolanM PinaforeGroundType = Interpreter PinaforeTypeSystem
    groundTypeVarianceMap ::
           forall (dv :: DolanVariance) (f :: DolanVarianceKind dv). PinaforeGroundType dv f -> DolanVarianceMap dv f
    groundTypeVarianceMap = pgtVarianceMap
    groundTypeVarianceType :: PinaforeGroundType dv t -> DolanVarianceType dv
    groundTypeVarianceType = pgtVarianceType
    groundTypeTestEquality :: PinaforeGroundType dka ta -> PinaforeGroundType dkb tb -> Maybe (dka :~: dkb, ta :~~: tb)
    groundTypeTestEquality ta tb = do
        Refl <- testEquality (pgtVarianceType ta) (pgtVarianceType tb)
        HRefl <- testHetEquality (pgtFamilyType ta) (pgtFamilyType tb)
        Just (Refl, HRefl)

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
    groundTypeShowPrec t args = showPrecDolanVariance (pgtShowType t) args

instance Is PolarityType polarity => Show (DolanType PinaforeGroundType polarity a) where
    show t = unpack $ exprShow t

instance Is PolarityType polarity => AllConstraint Show (DolanType PinaforeGroundType polarity) where
    allConstraint = Dict
