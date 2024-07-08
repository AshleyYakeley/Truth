module Pinafore.Language.Type.Ground where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Shim
import Pinafore.Language.Type.DynamicSupertype
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Show
import Pinafore.Language.VarID

type GroundProperties :: GroundTypeKind
newtype GroundProperties dv gt =
    MkGroundProperties (forall (prop :: GroundTypeKind). IOWitness (prop '[] ()) -> Maybe (prop dv gt))

instance forall (dv :: CCRVariances) (gt :: CCRVariancesKind dv). Semigroup (GroundProperties dv gt) where
    MkGroundProperties a <> MkGroundProperties b = MkGroundProperties $ \k -> a k <|> b k

instance forall (dv :: CCRVariances) (gt :: CCRVariancesKind dv). Monoid (GroundProperties dv gt) where
    mempty = MkGroundProperties $ \_ -> Nothing

singleGroundProperty ::
       forall (prop :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
       IOWitness (prop '[] ())
    -> prop dv gt
    -> GroundProperties dv gt
singleGroundProperty k v =
    MkGroundProperties $ \k' -> do
        Refl <- testEquality k k'
        return v

type QGroundType :: GroundTypeKind
data QGroundType dv gt = MkQGroundType
    { qgtVarianceType :: CCRVariancesType dv
    , qgtVarianceMap :: CCRVariancesMap dv gt
    , qgtShowType :: ListTypeExprShow dv
    , qgtFamilyType :: FamilialType gt
    , qgtProperties :: GroundProperties dv gt
    , qgtSubtypeGroup :: Maybe (SubtypeGroup QGroundType dv gt)
    , qgtGreatestDynamicSupertype :: QPolyGreatestDynamicSupertype dv gt
    }

instance TestEquality (QGroundType '[]) where
    testEquality gta gtb = do
        HRefl <- testHetEquality (qgtFamilyType gta) (qgtFamilyType gtb)
        return Refl

getGroundFamily ::
       forall (dv :: CCRVariances) (gt :: CCRVariancesKind dv) (fam :: FamilyKind).
       IOWitness ('MkWitKind fam)
    -> QGroundType dv gt
    -> Maybe (fam gt)
getGroundFamily wit gt = matchFamilyType wit $ qgtFamilyType gt

getGroundProperty ::
       forall (dv :: CCRVariances) (gt :: CCRVariancesKind dv) (prop :: GroundTypeKind).
       IOWitness (prop '[] ())
    -> QGroundType dv gt
    -> Maybe (prop dv gt)
getGroundProperty prop gt =
    case qgtProperties gt of
        MkGroundProperties f -> f prop

class ( Monad Interpreter
      , MonadException Interpreter
      , Show (Exc Interpreter)
      , MonadIO Interpreter
      , MonadThrow PatternError Interpreter
      , MonadThrow (ExpressionError QVar) Interpreter
      , MonadThrow QErrorType Interpreter
      , MonadThrow QError Interpreter
      , MonadCatch QError Interpreter
      ) => HasInterpreter where
    type Interpreter :: Type -> Type
    getSubtypeConversions :: Interpreter [QSubtypeConversionEntry]
    mkErrorMessage :: Interpreter (QErrorType -> QError)

instance HasInterpreter => ExprShow (QGroundType dv gt) where
    exprShowPrec = exprShowPrecGroundType

instance HasInterpreter => AllConstraint ExprShow (QGroundType '[]) where
    allConstraint = Dict

instance HasInterpreter => Show (QGroundType dv gt) where
    show t = unpack $ toText $ showGroundType t

type QPolyGreatestDynamicSupertype :: forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Type
type QPolyGreatestDynamicSupertype dv gt = PolyGreatestDynamicSupertype QGroundType dv gt

type QNonpolarType :: Type -> Type
type QNonpolarType = NonpolarType QGroundType

singleGroundType' ::
       forall (dv :: CCRVariances) (t :: CCRVariancesKind dv). (HasInterpreter, HasCCRVariances dv t)
    => FamilialType t
    -> GroundProperties dv t
    -> ListTypeExprShow dv
    -> QGroundType dv t
singleGroundType' ft props showexp =
    MkQGroundType
        { qgtVarianceType = representative
        , qgtVarianceMap = ccrVariancesMap
        , qgtShowType = showexp
        , qgtFamilyType = ft
        , qgtSubtypeGroup = Nothing
        , qgtProperties = props
        , qgtGreatestDynamicSupertype = nullPolyGreatestDynamicSupertype
        }

singleGroundType ::
       forall (dv :: CCRVariances) (t :: CCRVariancesKind dv). (HasInterpreter, HasCCRVariances dv t)
    => IOWitness ('MkWitKind (SingletonFamily t))
    -> ListTypeExprShow dv
    -> QGroundType dv t
singleGroundType wit = singleGroundType' (MkFamilialType wit HetRefl) mempty

standardListTypeExprShow ::
       forall (dv :: [CCRVariance]). Is CCRVariancesType dv
    => NamedText
    -> ListTypeExprShow dv
standardListTypeExprShow = let
    sh :: forall (dv' :: [CCRVariance]). Int -> CCRVariancesType dv' -> NamedText -> ListTypeExprShow dv'
    sh i NilListType (MkNamedText nt) = MkPrecNamedText $ \fnt -> (nt fnt, i)
    sh _ (ConsListType _ lt) t = \ta -> sh 2 lt (t <> " " <> precNamedText 0 ta)
    in sh 0 $ representative @_ @_ @dv

stdSingleGroundType ::
       forall (dv :: CCRVariances) (t :: CCRVariancesKind dv). (HasInterpreter, HasCCRVariances dv t)
    => IOWitness ('MkWitKind (SingletonFamily t))
    -> FullName
    -> QGroundType dv t
stdSingleGroundType wit name = singleGroundType wit $ standardListTypeExprShow @dv $ showNamedText name

type QTypeSystem = DolanTypeSystem QGroundType

type instance TSBindingData QTypeSystem = DefDoc

type instance DolanPolyShim QGroundType = QPolyShim

type QSomeGroundType :: Type
type QSomeGroundType = SomeGroundType QGroundType

type QSingularType :: Polarity -> Type -> Type
type QSingularType = DolanSingularType QGroundType

type QSingularShimWit :: Polarity -> Type -> Type
type QSingularShimWit polarity = DolanSingularShimWit QGroundType polarity

type QGroundedType :: Polarity -> Type -> Type
type QGroundedType = DolanGroundedType QGroundType

type QGroundedShimWit :: Polarity -> Type -> Type
type QGroundedShimWit polarity = DolanGroundedShimWit QGroundType polarity

type QType :: Polarity -> Type -> Type
type QType = DolanType QGroundType

type QShimWit :: Polarity -> Type -> Type
type QShimWit polarity = DolanShimWit QGroundType polarity

type QIsoShimWit :: Polarity -> Type -> Type
type QIsoShimWit polarity = DolanIsoShimWit QGroundType polarity

type QArgumentsShimWit :: forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Polarity -> Type -> Type
type QArgumentsShimWit dv gt polarity = CCRPolarArgumentsShimWit QPolyShim dv QType gt polarity

type QValue = TSValue QTypeSystem

type QVar = TSVar QTypeSystem

type QValueF f = TSValueF QTypeSystem f

type QOpenExpression = TSOpenExpression QTypeSystem

type QExpression = TSSealedExpression QTypeSystem

type QFExpression = TSSealedFExpression QTypeSystem

type QPartialExpression = TSSealedPartialExpression QTypeSystem

type QMatch = TSMatch QTypeSystem

type QPatternConstructor = TSExpressionPatternConstructor QTypeSystem

type QOpenPattern = TSOpenPattern QTypeSystem

type QPattern = TSSealedExpressionPattern QTypeSystem

type QBinding = TSBinding QTypeSystem

type QSubtypeConversionEntry = SubtypeConversionEntry QGroundType

instance HasInterpreter => IsDolanGroundType QGroundType where
    type DolanVarID QGroundType = VarID
    type DolanM QGroundType = Interpreter
    groundTypeVarianceMap ::
           forall (dv :: CCRVariances) (f :: CCRVariancesKind dv). QGroundType dv f -> CCRVariancesMap dv f
    groundTypeVarianceMap = qgtVarianceMap
    groundTypeVarianceType :: QGroundType dv t -> CCRVariancesType dv
    groundTypeVarianceType = qgtVarianceType
    groundTypeTestEquality :: QGroundType dka ta -> QGroundType dkb tb -> Maybe (dka :~: dkb, ta :~~: tb)
    groundTypeTestEquality ta tb = do
        Refl <- testEquality (qgtVarianceType ta) (qgtVarianceType tb)
        HRefl <- testHetEquality (qgtFamilyType ta) (qgtFamilyType tb)
        Just (Refl, HRefl)

instance HasInterpreter => ExprShow (SomeGroundType QGroundType) where
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
showPrecVariance (ContraCCRPolarArgument t) = withInvertPolarity @polarity $ exprShowPrec t
showPrecVariance (RangeCCRPolarArgument p q) = exprShowPrec (MkRangeType p q)

showPrecCCRVariances ::
       forall w polarity dv f t.
       ( Is PolarityType polarity
       , forall a polarity'. Is PolarityType polarity' => ExprShow (w polarity' a)
       , forall a. ExprShow (RangeType w polarity a)
       )
    => ListTypeExprShow dv
    -> CCRPolarArguments dv w f polarity t
    -> PrecNamedText
showPrecCCRVariances f NilCCRArguments = f
showPrecCCRVariances f (ConsCCRArguments t1 tr) = showPrecCCRVariances (f (showPrecVariance @w @polarity t1)) tr

instance GroundExprShow QGroundType where
    groundTypeShowPrec ::
           forall w polarity dv f t.
           ( Is PolarityType polarity
           , forall a polarity'. Is PolarityType polarity' => ExprShow (w polarity' a)
           , forall a. ExprShow (RangeType w polarity a)
           )
        => QGroundType dv f
        -> CCRPolarArguments dv w f polarity t
        -> PrecNamedText
    groundTypeShowPrec t args = showPrecCCRVariances (qgtShowType t) args

instance Is PolarityType polarity => ShowNamedText (DolanType QGroundType polarity a) where
    showNamedText t = toNamedText $ exprShow t

instance Is PolarityType polarity => AllConstraint ShowNamedText (DolanType QGroundType polarity) where
    allConstraint = Dict

instance Is PolarityType polarity => Show (DolanType QGroundType polarity a) where
    show t = unpack $ toText $ exprShow t

instance Is PolarityType polarity => AllConstraint Show (DolanType QGroundType polarity) where
    allConstraint = Dict

instance Is PolarityType polarity => Show (DolanSingularType QGroundType polarity a) where
    show t = unpack $ toText $ exprShow t

instance Is PolarityType polarity => AllConstraint Show (DolanSingularType QGroundType polarity) where
    allConstraint = Dict

instance Is PolarityType polarity => Show (DolanGroundedType QGroundType polarity a) where
    show t = unpack $ toText $ exprShow t

instance Is PolarityType polarity => AllConstraint Show (DolanGroundedType QGroundType polarity) where
    allConstraint = Dict
