module Language.Expression.Dolan.Argument where

import Data.Shim
import Language.Expression.Dolan.FreeVars
import Language.Expression.Dolan.PShimWit
import Shapes

type CCRArgumentKind = forall (sv :: CCRVariance) -> CCRVarianceKind sv -> Type

class IsCCRArg (w :: CCRArgumentKind) where
    ccrArgumentType :: forall (sv :: CCRVariance) (t :: CCRVarianceKind sv). w sv t -> CCRVarianceType sv
    ccrArgumentTestEquality ::
           forall (sv :: CCRVariance) (a :: CCRVarianceKind sv) (b :: CCRVarianceKind sv).
           w sv a
        -> w sv b
        -> Maybe (a :~: b)

type CCRArgumentShimWit :: (Type -> Type -> Type) -> CCRArgumentKind -> Polarity -> CCRArgumentKind
type CCRArgumentShimWit shim w polarity sv = ShimWit (CCRVarianceCategory (PolarMap shim polarity) sv) (w sv)

type CCRPolarArgument :: (Polarity -> Type -> Type) -> Polarity -> CCRArgumentKind
data CCRPolarArgument ft polarity sv t where
    CoCCRPolarArgument :: ft polarity t -> CCRPolarArgument ft polarity CoCCRVariance t
    ContraCCRPolarArgument :: ft (InvertPolarity polarity) t -> CCRPolarArgument ft polarity ContraCCRVariance t
    RangeCCRPolarArgument
        :: ft (InvertPolarity polarity) p -> ft polarity q -> CCRPolarArgument ft polarity 'RangeCCRVariance '( p, q)

instance (forall polarity' t'. FreeTypeVariables (ft polarity' t')) =>
             FreeTypeVariables (CCRPolarArgument ft polarity sv t) where
    freeTypeVariables (CoCCRPolarArgument t) = freeTypeVariables t
    freeTypeVariables (ContraCCRPolarArgument t) = freeTypeVariables t
    freeTypeVariables (RangeCCRPolarArgument p q) = freeTypeVariables p <> freeTypeVariables q

instance forall ft polarity. (TestEquality (ft 'Positive), TestEquality (ft 'Negative), Is PolarityType polarity) =>
             IsCCRArg (CCRPolarArgument ft polarity) where
    ccrArgumentType (CoCCRPolarArgument _) = CoCCRVarianceType
    ccrArgumentType (ContraCCRPolarArgument _) = ContraCCRVarianceType
    ccrArgumentType (RangeCCRPolarArgument _ _) = RangeCCRVarianceType
    ccrArgumentTestEquality (CoCCRPolarArgument arg1) (CoCCRPolarArgument arg2) =
        case polarityType @polarity of
            PositiveType -> testEquality arg1 arg2
            NegativeType -> testEquality arg1 arg2
    ccrArgumentTestEquality (ContraCCRPolarArgument arg1) (ContraCCRPolarArgument arg2) =
        case polarityType @polarity of
            PositiveType -> testEquality arg1 arg2
            NegativeType -> testEquality arg1 arg2
    ccrArgumentTestEquality (RangeCCRPolarArgument p1 q1) (RangeCCRPolarArgument p2 q2) =
        case polarityType @polarity of
            PositiveType -> do
                Refl <- testEquality p1 p2
                Refl <- testEquality q1 q2
                return Refl
            NegativeType -> do
                Refl <- testEquality p1 p2
                Refl <- testEquality q1 q2
                return Refl

forCCRPolarArgument ::
       forall polarity sv ft t r. (Is PolarityType polarity, Monoid r)
    => (forall polarity' t'. Is PolarityType polarity' => ft polarity' t' -> r)
    -> CCRPolarArgument ft polarity sv t
    -> r
forCCRPolarArgument call (CoCCRPolarArgument t) = call t
forCCRPolarArgument call (ContraCCRPolarArgument t) = withInvertPolarity @polarity $ call t
forCCRPolarArgument call (RangeCCRPolarArgument p q) = withInvertPolarity @polarity $ call p <> call q

type CCRPolarArgumentShimWit :: (Type -> Type -> Type) -> (Polarity -> Type -> Type) -> Polarity -> CCRArgumentKind
type CCRPolarArgumentShimWit shim ft polarity sv = CCRArgumentShimWit shim (CCRPolarArgument ft polarity) polarity sv

coCCRArgument :: PShimWit shim ft polarity t -> CCRPolarArgumentShimWit shim ft polarity CoCCRVariance t
coCCRArgument (MkShimWit ft conv) = MkShimWit (CoCCRPolarArgument ft) conv

contraCCRArgument ::
       Is PolarityType polarity
    => PShimWit shim ft (InvertPolarity polarity) t
    -> CCRPolarArgumentShimWit shim ft polarity ContraCCRVariance t
contraCCRArgument (MkShimWit ft conv) = MkShimWit (ContraCCRPolarArgument ft) $ MkCatDual $ uninvertPolarMap conv

rangeCCRArgument ::
       Is PolarityType polarity
    => PShimWit shim ft (InvertPolarity polarity) p
    -> PShimWit shim ft polarity q
    -> CCRPolarArgumentShimWit shim ft polarity 'RangeCCRVariance '( p, q)
rangeCCRArgument (MkShimWit pt pconv) (MkShimWit qt qconv) =
    MkShimWit (RangeCCRPolarArgument pt qt) $ MkCatRange (uninvertPolarMap pconv) qconv

mapCCRPolarArgumentShimWit ::
       forall m (shim :: Type -> Type -> Type) (fta :: Polarity -> Type -> Type) (ftb :: Polarity -> Type -> Type) sv polarity t.
       (Monad m, Is PolarityType polarity)
    => (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> m (PShimWit shim ftb polarity' t'))
    -> CCRPolarArgument fta polarity sv t
    -> m (CCRPolarArgumentShimWit shim ftb polarity sv t)
mapCCRPolarArgumentShimWit f (CoCCRPolarArgument arg) = do
    tf <- f arg
    return $ unPolarShimWit tf $ \arg' -> MkShimWit (CoCCRPolarArgument arg')
mapCCRPolarArgumentShimWit f (ContraCCRPolarArgument arg) =
    withInvertPolarity @polarity $ do
        MkShimWit arg' conv <- f arg
        return $ MkShimWit (ContraCCRPolarArgument arg') $ mkContravariantPolarMap conv
mapCCRPolarArgumentShimWit f (RangeCCRPolarArgument tp tq) =
    withInvertPolarity @polarity $ do
        MkShimWit tp' convp <- f tp
        MkShimWit tq' convq <- f tq
        return $ MkShimWit (RangeCCRPolarArgument tp' tq') $ mkRangevariantPolarMap convp convq

mapInvertCCRPolarArgumentShimWit ::
       forall m shim fta ftb sv polarity t. (Monad m, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' => fta polarity' t' -> m (PShimWit shim ftb (InvertPolarity polarity') t'))
    -> CCRPolarArgument fta polarity sv t
    -> m (CCRPolarArgumentShimWit shim ftb (InvertPolarity polarity) sv t)
mapInvertCCRPolarArgumentShimWit f (CoCCRPolarArgument arg) = do
    MkShimWit arg' conv <- f arg
    return $ MkShimWit (CoCCRPolarArgument arg') conv
mapInvertCCRPolarArgumentShimWit f (ContraCCRPolarArgument arg) =
    withInvertPolarity @polarity $ do
        MkShimWit arg' conv <- f arg
        return $ MkShimWit (ContraCCRPolarArgument arg') $ mkContravariantPolarMap conv
mapInvertCCRPolarArgumentShimWit f (RangeCCRPolarArgument tp tq) =
    withInvertPolarity @polarity $ do
        MkShimWit tp' convp <- f tp
        MkShimWit tq' convq <- f tq
        return $ MkShimWit (RangeCCRPolarArgument tp' tq') $ mkRangevariantPolarMap convp convq

type SVJoinMeetType :: forall (sv :: CCRVariance) ->
                               Polarity -> CCRVarianceKind sv -> CCRVarianceKind sv -> CCRVarianceKind sv
type family SVJoinMeetType sv polarity a b where
    SVJoinMeetType CoCCRVariance polarity a b = JoinMeetType polarity a b
    SVJoinMeetType ContraCCRVariance polarity a b = JoinMeetType (InvertPolarity polarity) a b
    SVJoinMeetType 'RangeCCRVariance polarity a b = '( JoinMeetType (InvertPolarity polarity) (Contra a) (Contra b), JoinMeetType polarity (Co a) (Co b))

mergeCCRPolarArgumentShimWit ::
       forall m shim fta ftb ftab sv polarity ta tb. (Monad m, Is PolarityType polarity)
    => (forall polarity' ta' tb'.
            Is PolarityType polarity' =>
                    fta polarity' ta' -> ftb polarity' tb' -> m (PShimWit shim ftab polarity' (JoinMeetType polarity' ta' tb')))
    -> CCRPolarArgument fta polarity sv ta
    -> CCRPolarArgument ftb polarity sv tb
    -> m (CCRPolarArgumentShimWit shim ftab polarity sv (SVJoinMeetType sv polarity ta tb))
mergeCCRPolarArgumentShimWit f (CoCCRPolarArgument arga) (CoCCRPolarArgument argb) = do
    MkShimWit argab conv <- f arga argb
    return $ MkShimWit (CoCCRPolarArgument argab) conv
mergeCCRPolarArgumentShimWit f (ContraCCRPolarArgument arga) (ContraCCRPolarArgument argb) =
    withInvertPolarity @polarity $ do
        MkShimWit argab conv <- f arga argb
        return $ MkShimWit (ContraCCRPolarArgument argab) $ mkContravariantPolarMap conv
mergeCCRPolarArgumentShimWit f (RangeCCRPolarArgument tpa tqa) (RangeCCRPolarArgument tpb tqb) =
    withInvertPolarity @polarity $ do
        MkShimWit tpab convp <- f tpa tpb
        MkShimWit tqab convq <- f tqa tqb
        return $ MkShimWit (RangeCCRPolarArgument tpab tqab) $ mkRangevariantPolarMap convp convq

ccrPolar1 ::
       forall shim polarity sv a b. (JoinMeetCategory shim, Is PolarityType polarity)
    => CCRVarianceType sv
    -> PolarVarianceMap shim polarity sv a (SVJoinMeetType sv polarity a b)
ccrPolar1 CoCCRVarianceType = polar1
ccrPolar1 ContraCCRVarianceType = withInvertPolarity @polarity $ mkContravariantPolarMap polar1
ccrPolar1 RangeCCRVarianceType =
    withInvertPolarity @polarity $
    case (unsafeTypeIsPair @_ @_ @a, unsafeTypeIsPair @_ @_ @b) of
        (Refl, Refl) -> mkRangevariantPolarMap polar1 polar1

ccrPolar2 ::
       forall shim polarity sv a b. (JoinMeetCategory shim, Is PolarityType polarity)
    => CCRVarianceType sv
    -> PolarVarianceMap shim polarity sv b (SVJoinMeetType sv polarity a b)
ccrPolar2 CoCCRVarianceType = polar2
ccrPolar2 ContraCCRVarianceType = withInvertPolarity @polarity $ mkContravariantPolarMap polar2
ccrPolar2 RangeCCRVarianceType =
    withInvertPolarity @polarity $
    case (unsafeTypeIsPair @_ @_ @a, unsafeTypeIsPair @_ @_ @b) of
        (Refl, Refl) -> mkRangevariantPolarMap polar2 polar2
