module Pinafore.Language.Type.Ground where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type.DynamicSupertype
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Show
import Pinafore.Language.VarID
import Pinafore.Markdown
import Shapes

type QGroundType :: GroundTypeKind
data QGroundType dv gt = MkQGroundType
    { qgtVarianceType :: DolanVarianceType dv
    , qgtVarianceMap :: DolanVarianceMap dv gt
    , qgtShowType :: ListTypeExprShow dv
    , qgtFamilyType :: FamilialType gt
    , qgtSubtypeGroup :: Maybe (SubtypeGroup QGroundType)
    , qgtGreatestDynamicSupertype :: PinaforePolyGreatestDynamicSupertype dv gt
    }

instance ExprShow (QGroundType dv gt) where
    exprShowPrec = exprShowPrecGroundType

instance Show (QGroundType dv gt) where
    show t = unpack $ toText $ showGroundType t

type PinaforePolyGreatestDynamicSupertype :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
type PinaforePolyGreatestDynamicSupertype dv gt = PolyGreatestDynamicSupertype QGroundType dv gt

type QNonpolarType :: Type -> Type
type QNonpolarType = NonpolarDolanType QGroundType

singleGroundType' ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). HasDolanVariance dv t
    => FamilialType t
    -> ListTypeExprShow dv
    -> QGroundType dv t
singleGroundType' ft showexp =
    MkQGroundType
        { qgtVarianceType = representative
        , qgtVarianceMap = dolanVarianceMap
        , qgtShowType = showexp
        , qgtFamilyType = ft
        , qgtSubtypeGroup = Nothing
        , qgtGreatestDynamicSupertype = nullPolyGreatestDynamicSupertype
        }

singleGroundType ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). HasDolanVariance dv t
    => IOWitness ('MkWitKind (SingletonFamily t))
    -> ListTypeExprShow dv
    -> QGroundType dv t
singleGroundType wit = singleGroundType' $ MkFamilialType wit HetRefl

standardListTypeExprShow ::
       forall (dv :: [CCRVariance]). Is DolanVarianceType dv
    => NamedText
    -> ListTypeExprShow dv
standardListTypeExprShow = let
    sh :: forall (dv' :: [CCRVariance]). Int -> DolanVarianceType dv' -> NamedText -> ListTypeExprShow dv'
    sh i NilListType (MkNamedText nt) = MkPrecNamedText $ \fnt -> (nt fnt, i)
    sh _ (ConsListType _ lt) t = \ta -> sh 2 lt (t <> " " <> precNamedText 0 ta)
    in sh 0 $ representative @_ @_ @dv

stdSingleGroundType ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). HasDolanVariance dv t
    => IOWitness ('MkWitKind (SingletonFamily t))
    -> FullName
    -> QGroundType dv t
stdSingleGroundType wit name = singleGroundType wit $ standardListTypeExprShow @dv $ toNamedText name

type QTypeSystem = DolanTypeSystem QGroundType

type instance TSBindingData QTypeSystem = RawMarkdown

type instance DolanPolyShim QGroundType = QPolyShim

instance IsDolanGroundType QGroundType where
    type DolanVarID QGroundType = VarID
    type DolanM QGroundType = Interpreter QTypeSystem
    groundTypeVarianceMap ::
           forall (dv :: DolanVariance) (f :: DolanVarianceKind dv). QGroundType dv f -> DolanVarianceMap dv f
    groundTypeVarianceMap = qgtVarianceMap
    groundTypeVarianceType :: QGroundType dv t -> DolanVarianceType dv
    groundTypeVarianceType = qgtVarianceType
    groundTypeTestEquality :: QGroundType dka ta -> QGroundType dkb tb -> Maybe (dka :~: dkb, ta :~~: tb)
    groundTypeTestEquality ta tb = do
        Refl <- testEquality (qgtVarianceType ta) (qgtVarianceType tb)
        HRefl <- testHetEquality (qgtFamilyType ta) (qgtFamilyType tb)
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
    -> PrecNamedText
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
    -> PrecNamedText
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
        -> PrecNamedText
    groundTypeShowPrec t args = showPrecDolanVariance (qgtShowType t) args

instance Is PolarityType polarity => Show (DolanType QGroundType polarity a) where
    show t = unpack $ toText $ exprShow t

instance Is PolarityType polarity => AllConstraint Show (DolanType QGroundType polarity) where
    allConstraint = Dict

instance Is PolarityType polarity => Show (DolanGroundedType QGroundType polarity a) where
    show t = unpack $ toText $ exprShow t

instance Is PolarityType polarity => AllConstraint Show (DolanGroundedType QGroundType polarity) where
    allConstraint = Dict
