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

type PinaforeGroundType :: GroundTypeKind
data PinaforeGroundType dv gt = MkPinaforeGroundType
    { pgtVarianceType :: DolanVarianceType dv
    , pgtVarianceMap :: DolanVarianceMap dv gt
    , pgtShowType :: ListTypeExprShow dv
    , pgtFamilyType :: FamilyType gt
    , pgtGreatestDynamicSupertype :: PinaforePolyGreatestDynamicSupertype dv gt
    }

type PinaforePolyGreatestDynamicSupertype :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
type PinaforePolyGreatestDynamicSupertype dv gt = PolyGreatestDynamicSupertype PinaforeGroundType PinaforePolyShim dv gt

type PinaforeGreatestDynamicSupertype :: Type -> Type
type PinaforeGreatestDynamicSupertype t = GreatestDynamicSupertype PinaforeGroundType PinaforePolyShim t

singleGroundType' ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). HasDolanVariance dv t
    => FamilyType t
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
singleGroundType wit = singleGroundType' $ MkFamilyType wit HetRefl

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
    => CCRVarianceType sv
    -> SingleArgument sv w polarity t
    -> (Text, Int)
showPrecVariance CoCCRVarianceType t = exprShowPrec t
showPrecVariance ContraCCRVarianceType t = invertPolarity @polarity $ exprShowPrec t
showPrecVariance RangeCCRVarianceType t = exprShowPrec t

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
    groundTypeShowPrec t args = showPrecDolanVariance (pgtShowType t) (pgtVarianceType t) args

instance Is PolarityType polarity => Show (DolanType PinaforeGroundType polarity a) where
    show t = unpack $ exprShow t

instance Is PolarityType polarity => AllWitnessConstraint Show (DolanType PinaforeGroundType polarity) where
    allWitnessConstraint = Dict
