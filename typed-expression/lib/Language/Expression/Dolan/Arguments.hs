module Language.Expression.Dolan.Arguments
    ( SingleArgument
    , DolanArguments(..)
    , forDolanArguments
    , saturateArgsConstraint
    , mapDolanArgumentsType
    , mapDolanArgumentsM
    , mapDolanArguments
    , mapInvertDolanArgumentsM
    , mergeDolanArgumentsM
    , mergeDolanArguments
    , dolanTestEquality
    , Arguments(..)
    , dolanArgumentsToArgumentsM
    , dolanArgumentsToArguments
    , argumentsToDolanArgumentsM
    , argumentsToDolanArguments
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Covariance
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Variance
import Shapes

type SingleArgument :: forall (sv :: Variance) -> (Polarity -> Type -> Type) -> Polarity -> VarianceKind sv -> Type
type family SingleArgument sv ft polarity where
    SingleArgument 'Covariance ft polarity = ft polarity
    SingleArgument 'Contravariance ft polarity = ft (InvertPolarity polarity)
    SingleArgument 'Rangevariance ft polarity = RangeType ft polarity

type DolanArguments :: forall (dv :: DolanVariance) ->
                               (Polarity -> Type -> Type) -> DolanVarianceKind dv -> Polarity -> Type -> Type
data DolanArguments dv ft gt polarity ta where
    NilDolanArguments :: DolanArguments '[] ft t polarity t
    ConsDolanArguments
        :: forall sv dv ft gt polarity a ta. InVarianceKind sv a
        => SingleArgument sv ft polarity a
        -> DolanArguments dv ft (gt a) polarity ta
        -> DolanArguments (sv ': dv) ft gt polarity ta

forDolanArgument ::
       forall polarity sv ft t r. (Is PolarityType polarity, Monoid r)
    => (forall polarity' t'. Is PolarityType polarity' => ft polarity' t' -> r)
    -> VarianceType sv
    -> SingleArgument sv ft polarity t
    -> r
forDolanArgument call CovarianceType t = call t
forDolanArgument call ContravarianceType t = invertPolarity @polarity $ call t
forDolanArgument call RangevarianceType (MkRangeType p q) = invertPolarity @polarity $ call p <> call q

forDolanArguments ::
       forall polarity dv ft gt t r. (Is PolarityType polarity, Monoid r)
    => (forall polarity' t'. Is PolarityType polarity' => ft polarity' t' -> r)
    -> DolanVarianceType dv
    -> DolanArguments dv ft gt polarity t
    -> r
forDolanArguments _call NilListType NilDolanArguments = mempty
forDolanArguments call (ConsListType sv dv) (ConsDolanArguments arg args) =
    forDolanArgument @polarity call sv arg <> forDolanArguments call dv args

saturateArgsConstraint ::
       forall (w :: Type -> Type) dv ft gt polarity (t :: Type).
       SaturatedWitness w gt
    -> DolanArguments dv ft gt polarity t
    -> w t
saturateArgsConstraint (NilSaturatedWitness wt) NilDolanArguments = wt
saturateArgsConstraint (ConsSaturatedWitness sw) (ConsDolanArguments _ args) = saturateArgsConstraint sw args

type ArgTypeF :: (Type -> Type -> Type) -> forall (sv :: Variance) ->
                                                   (Polarity -> Type -> Type) -> Polarity -> VarianceKind sv -> Type
data ArgTypeF shim sv ft polarity t where
    MkArgTypeF
        :: InVarianceKind sv t'
        => SingleArgument sv ft polarity t'
        -> PolarVarianceMap shim polarity sv t t'
        -> ArgTypeF shim sv ft polarity t

mapArgTypeF ::
       forall m (shim :: Type -> Type -> Type) (fta :: Polarity -> Type -> Type) (ftb :: Polarity -> Type -> Type) sv polarity t.
       (Monad m, Is PolarityType polarity)
    => VarianceType sv
    -> (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> m (PShimWit shim ftb polarity' t'))
    -> SingleArgument sv fta polarity t
    -> m (ArgTypeF shim sv ftb polarity t)
mapArgTypeF CovarianceType f arg = do
    tf <- f arg
    return $ unPolarShimWit tf MkArgTypeF
mapArgTypeF ContravarianceType f arg =
    invertPolarity @polarity $ do
        MkShimWit arg' conv <- f arg
        return $ MkArgTypeF arg' $ mkContravariantPolarMap conv
mapArgTypeF RangevarianceType f (MkRangeType tp tq) =
    invertPolarity @polarity $ do
        MkShimWit tp' convp <- f tp
        MkShimWit tq' convq <- f tq
        return $ MkArgTypeF (MkRangeType tp' tq') $ mkRangevariantPolarMap convp convq

mapArgsTypeF ::
       forall m (pshim :: PolyShimKind) fta ftb dv polarity (gt :: DolanVarianceKind dv) (gt' :: DolanVarianceKind dv) t.
       (Monad m, DolanVarianceInCategory pshim, Is PolarityType polarity)
    => (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> m (PShimWit (pshim Type) ftb polarity' t'))
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> DolanVarianceMap dv gt'
    -> DolanArguments dv fta gt polarity t
    -> PolarMap (pshim (DolanVarianceKind dv)) polarity gt gt'
    -> m (PShimWit (pshim Type) (DolanArguments dv ftb gt') polarity t)
mapArgsTypeF _ NilListType NilDolanVarianceMap NilDolanVarianceMap NilDolanArguments conv =
    return $ MkShimWit NilDolanArguments conv
mapArgsTypeF f (ConsListType svt dvt) (ConsDolanVarianceMap dvm) (ConsDolanVarianceMap dvm') (ConsDolanArguments sta dta) conv = do
    MkArgTypeF sta' svf <- mapArgTypeF @m @(pshim Type) @fta @ftb @_ @polarity svt f sta
    Dict <- return $ varianceCoercibleKind svt
    Dict <- return $ dolanVarianceInCategory @pshim dvt
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gt) sta
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gt) sta'
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gt') sta'
    MkShimWit dta' conv' <-
        mapArgsTypeF @m @pshim @fta @ftb @_ @polarity f dvt dvm dvm' dta (polarMapTypeApply svt conv svf)
    return $ MkShimWit (ConsDolanArguments sta' dta') conv'

mapDolanArgumentsType ::
       forall (pshim :: PolyShimKind) ft dv polarity (gt :: DolanVarianceKind dv) (gt' :: DolanVarianceKind dv) t.
       (DolanVarianceInCategory pshim, Is PolarityType polarity)
    => DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> DolanVarianceMap dv gt'
    -> DolanArguments dv ft gt polarity t
    -> PolarMap (pshim (DolanVarianceKind dv)) polarity gt gt'
    -> PShimWit (pshim Type) (DolanArguments dv ft gt') polarity t
mapDolanArgumentsType dt dvma dvmb args f = runIdentity $ mapArgsTypeF (pure . mkPolarShimWit) dt dvma dvmb args f

mapDolanArgumentsM ::
       forall m (pshim :: PolyShimKind) fta ftb dv polarity gt t.
       (Monad m, DolanVarianceInCategory pshim, Is PolarityType polarity)
    => (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> m (PShimWit (pshim Type) ftb polarity' t'))
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> DolanArguments dv fta gt polarity t
    -> m (PShimWit (pshim Type) (DolanArguments dv ftb gt) polarity t)
mapDolanArgumentsM f dvt dvm args =
    case dolanVarianceInCategory @pshim dvt of
        Dict ->
            case dolanVarianceMapInKind dvm of
                Dict -> mapArgsTypeF f dvt dvm dvm args cid

mapDolanArguments ::
       forall (pshim :: PolyShimKind) fta ftb dv polarity gt t.
       (DolanVarianceInCategory pshim, Is PolarityType polarity)
    => (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> PShimWit (pshim Type) ftb polarity' t')
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> DolanArguments dv fta gt polarity t
    -> PShimWit (pshim Type) (DolanArguments dv ftb gt) polarity t
mapDolanArguments f dvt kv args = runIdentity $ mapDolanArgumentsM (\t -> return $ f t) dvt kv args

mapInvertArgTypeF ::
       forall m shim fta ftb sv polarity t. (Monad m, Is PolarityType polarity)
    => VarianceType sv
    -> (forall polarity' t'.
            Is PolarityType polarity' => fta polarity' t' -> m (PShimWit shim ftb (InvertPolarity polarity') t'))
    -> SingleArgument sv fta polarity t
    -> m (ArgTypeF shim sv ftb (InvertPolarity polarity) t)
mapInvertArgTypeF CovarianceType f arg = do
    MkShimWit arg' conv <- f arg
    return $ MkArgTypeF arg' conv
mapInvertArgTypeF ContravarianceType f arg =
    invertPolarity @polarity $ do
        MkShimWit arg' conv <- f arg
        return $ MkArgTypeF arg' $ mkContravariantPolarMap conv
mapInvertArgTypeF RangevarianceType f (MkRangeType tp tq) =
    invertPolarity @polarity $ do
        MkShimWit tp' convp <- f tp
        MkShimWit tq' convq <- f tq
        return $ MkArgTypeF (MkRangeType tp' tq') $ mkRangevariantPolarMap convp convq

mapInvertArgsTypeF ::
       forall m (pshim :: PolyShimKind) fta ftb dv polarity (gt :: DolanVarianceKind dv) (gt' :: DolanVarianceKind dv) t.
       (Monad m, DolanVarianceInCategory pshim, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' => fta polarity' t' -> m (PShimWit (pshim Type) ftb (InvertPolarity polarity') t'))
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> DolanVarianceMap dv gt'
    -> DolanArguments dv fta gt polarity t
    -> PolarMap (pshim (DolanVarianceKind dv)) (InvertPolarity polarity) gt gt'
    -> m (PShimWit (pshim Type) (DolanArguments dv ftb gt') (InvertPolarity polarity) t)
mapInvertArgsTypeF _ NilListType NilDolanVarianceMap NilDolanVarianceMap NilDolanArguments conv =
    return $ MkShimWit NilDolanArguments conv
mapInvertArgsTypeF f (ConsListType svt dvt) (ConsDolanVarianceMap dvm) (ConsDolanVarianceMap dvm') (ConsDolanArguments sta dta) conv = do
    MkArgTypeF sta' svf <- mapInvertArgTypeF @m @(pshim Type) @fta @ftb @_ @polarity svt f sta
    Dict <- return $ varianceCoercibleKind svt
    Dict <- return $ dolanVarianceInCategory @pshim dvt
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gt) sta
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gt) sta'
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gt') sta'
    MkShimWit dta' conv' <-
        invertPolarity @polarity $
        mapInvertArgsTypeF @m @pshim @fta @ftb @_ @polarity f dvt dvm dvm' dta (polarMapTypeApply svt conv svf)
    return $ MkShimWit (ConsDolanArguments sta' dta') conv'

mapInvertDolanArgumentsM ::
       forall m (pshim :: PolyShimKind) fta ftb dv polarity gt t.
       (Monad m, DolanVarianceInCategory pshim, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' => fta polarity' t' -> m (PShimWit (pshim Type) ftb (InvertPolarity polarity') t'))
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> DolanArguments dv fta gt polarity t
    -> m (PShimWit (pshim Type) (DolanArguments dv ftb gt) (InvertPolarity polarity) t)
mapInvertDolanArgumentsM f dvt dvm args =
    case dolanVarianceInCategory @pshim dvt of
        Dict ->
            case dolanVarianceMapInKind dvm of
                Dict -> invertPolarity @polarity $ mapInvertArgsTypeF f dvt dvm dvm args cid

type SVJoinMeetType :: forall (sv :: Variance) -> Polarity -> VarianceKind sv -> VarianceKind sv -> VarianceKind sv
type family SVJoinMeetType sv polarity a b where
    SVJoinMeetType 'Covariance polarity a b = JoinMeetType polarity a b
    SVJoinMeetType 'Contravariance polarity a b = JoinMeetType (InvertPolarity polarity) a b
    SVJoinMeetType 'Rangevariance polarity a b = '( JoinMeetType (InvertPolarity polarity) (Contra a) (Contra b), JoinMeetType polarity (Co a) (Co b))

svJoinMeetTypeInKind :: forall sv polarity a b. VarianceType sv -> Dict (InKind (SVJoinMeetType sv polarity a b))
svJoinMeetTypeInKind CovarianceType = Dict
svJoinMeetTypeInKind ContravarianceType = Dict
svJoinMeetTypeInKind RangevarianceType = Dict

mergeArgTypeF ::
       forall m shim fta ftb ftab sv polarity ta tb. (Monad m, Is PolarityType polarity)
    => VarianceType sv
    -> (forall polarity' ta' tb'.
            Is PolarityType polarity' =>
                    fta polarity' ta' -> ftb polarity' tb' -> m (PShimWit shim ftab polarity' (JoinMeetType polarity' ta' tb')))
    -> SingleArgument sv fta polarity ta
    -> SingleArgument sv ftb polarity tb
    -> m (ArgTypeF shim sv ftab polarity (SVJoinMeetType sv polarity ta tb))
mergeArgTypeF CovarianceType f arga argb = do
    MkShimWit argab conv <- f arga argb
    return $ MkArgTypeF argab conv
mergeArgTypeF ContravarianceType f arga argb =
    invertPolarity @polarity $ do
        MkShimWit argab conv <- f arga argb
        return $ MkArgTypeF argab $ mkContravariantPolarMap conv
mergeArgTypeF RangevarianceType f (MkRangeType tpa tqa) (MkRangeType tpb tqb) =
    invertPolarity @polarity $ do
        MkShimWit tpab convp <- f tpa tpb
        MkShimWit tqab convq <- f tqa tqb
        return $ MkArgTypeF (MkRangeType tpab tqab) $ mkRangevariantPolarMap convp convq

varPolar1 ::
       forall shim polarity sv a b.
       (JoinMeetCategory shim, Is PolarityType polarity, InVarianceKind sv a, InVarianceKind sv b)
    => VarianceType sv
    -> PolarVarianceMap shim polarity sv a (SVJoinMeetType sv polarity a b)
varPolar1 CovarianceType = polar1
varPolar1 ContravarianceType = invertPolarity @polarity $ mkContravariantPolarMap polar1
varPolar1 RangevarianceType =
    invertPolarity @polarity $
    case (inKind @_ @a, inKind @_ @b) of
        (MkPairWitness, MkPairWitness) -> mkRangevariantPolarMap polar1 polar1

varPolar2 ::
       forall shim polarity sv a b.
       (JoinMeetCategory shim, Is PolarityType polarity, InVarianceKind sv a, InVarianceKind sv b)
    => VarianceType sv
    -> PolarVarianceMap shim polarity sv b (SVJoinMeetType sv polarity a b)
varPolar2 CovarianceType = polar2
varPolar2 ContravarianceType = invertPolarity @polarity $ mkContravariantPolarMap polar2
varPolar2 RangevarianceType =
    invertPolarity @polarity $
    case (inKind @_ @a, inKind @_ @b) of
        (MkPairWitness, MkPairWitness) -> mkRangevariantPolarMap polar2 polar2

mergeArgsTypeF ::
       forall (m :: Type -> Type) (pshim :: PolyShimKind) (fta :: Polarity -> Type -> Type) (ftb :: Polarity -> Type -> Type) (ftab :: Polarity -> Type -> Type) (dv :: DolanVariance) (polarity :: Polarity) (gta :: DolanVarianceKind dv) (gtb :: DolanVarianceKind dv) (gtab :: DolanVarianceKind dv) (ta :: Type) (tb :: Type).
       (Monad m, DolanVarianceInCategory pshim, JoinMeetCategory (pshim Type), Is PolarityType polarity)
    => (forall polarity' ta' tb'.
            Is PolarityType polarity' =>
                    fta polarity' ta' -> ftb polarity' tb' -> m (PShimWit (pshim Type) ftab polarity' (JoinMeetType polarity' ta' tb')))
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gta
    -> DolanVarianceMap dv gtb
    -> DolanVarianceMap dv gtab
    -> DolanArguments dv fta gta polarity ta
    -> DolanArguments dv ftb gtb polarity tb
    -> PolarMap (pshim (DolanVarianceKind dv)) polarity gta gtab
    -> PolarMap (pshim (DolanVarianceKind dv)) polarity gtb gtab
    -> m (PShimWit (pshim Type) (DolanArguments dv ftab gtab) polarity (JoinMeetType polarity ta tb))
mergeArgsTypeF _ NilListType NilDolanVarianceMap NilDolanVarianceMap NilDolanVarianceMap NilDolanArguments NilDolanArguments conva convb =
    return $ MkShimWit NilDolanArguments $ polarF conva convb
mergeArgsTypeF f (ConsListType svt dvt) (ConsDolanVarianceMap dvma) (ConsDolanVarianceMap dvmb) (ConsDolanVarianceMap dvmab) (ConsDolanArguments (sta :: _ ta0) dta) (ConsDolanArguments (stb :: _ tb0) dtb) conva convb = do
    Dict <- return $ varianceCoercibleKind svt
    MkArgTypeF stab svf <- mergeArgTypeF @m @(pshim Type) @fta @ftb @ftab @_ @polarity svt f sta stb
    Dict <- return $ dolanVarianceInCategory @pshim dvt
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gta) sta
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gtb) stb
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gta) stab
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gtb) stab
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gtab) stab
    Dict <- return $ varianceInCategory @(PolarMap (pshim Type) polarity) svt
    Dict <- return $ svJoinMeetTypeInKind @_ @polarity @ta0 @tb0 svt
    MkShimWit dtab convab <-
        mergeArgsTypeF
            @m
            @pshim
            @fta
            @ftb
            @ftab
            @_
            @polarity
            f
            dvt
            dvma
            dvmb
            dvmab
            dta
            dtb
            (polarMapTypeApply svt conva $ svf <.> varPolar1 @(pshim Type) @polarity @_ @ta0 @tb0 svt)
            (polarMapTypeApply svt convb $ svf <.> varPolar2 @(pshim Type) @polarity @_ @ta0 @tb0 svt)
    return $ MkShimWit (ConsDolanArguments stab dtab) convab

mergeDolanArgumentsM ::
       forall m (pshim :: PolyShimKind) fta ftb ftab dv polarity gt ta tb.
       (Monad m, DolanVarianceInCategory pshim, JoinMeetCategory (pshim Type), Is PolarityType polarity)
    => (forall polarity' ta' tb'.
            Is PolarityType polarity' =>
                    fta polarity' ta' -> ftb polarity' tb' -> m (PShimWit (pshim Type) ftab polarity' (JoinMeetType polarity' ta' tb')))
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> DolanArguments dv fta gt polarity ta
    -> DolanArguments dv ftb gt polarity tb
    -> m (PShimWit (pshim Type) (DolanArguments dv ftab gt) polarity (JoinMeetType polarity ta tb))
mergeDolanArgumentsM f dvt dvm argsa argsb =
    case dolanVarianceInCategory @pshim dvt of
        Dict ->
            case dolanVarianceMapInKind dvm of
                Dict -> mergeArgsTypeF @m @pshim f dvt dvm dvm dvm argsa argsb cid cid

mergeDolanArguments ::
       forall (pshim :: PolyShimKind) fta ftb ftab dv polarity gt ta tb.
       (DolanVarianceInCategory pshim, JoinMeetCategory (pshim Type), Is PolarityType polarity)
    => (forall polarity' ta' tb'.
            Is PolarityType polarity' =>
                    fta polarity' ta' -> ftb polarity' tb' -> PShimWit (pshim Type) ftab polarity' (JoinMeetType polarity' ta' tb'))
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> DolanArguments dv fta gt polarity ta
    -> DolanArguments dv ftb gt polarity tb
    -> PShimWit (pshim Type) (DolanArguments dv ftab gt) polarity (JoinMeetType polarity ta tb)
mergeDolanArguments f dvt dvm argsa argsb =
    runIdentity $ mergeDolanArgumentsM (\a b -> Identity $ f a b) dvt dvm argsa argsb

dolanArgumentsToArgumentsM' ::
       forall m (pshim :: PolyShimKind) wa wb dv polarity (fa :: DolanVarianceKind dv) (fb :: DolanVarianceKind dv) t.
       (Monad m, DolanVarianceInCategory pshim, Is PolarityType polarity)
    => (forall t'. wa polarity t' -> m (PolarShimWit (pshim Type) wb polarity t'))
    -> CovaryType dv
    -> DolanVarianceMap dv fa
    -> DolanVarianceMap dv fb
    -> PolarMap (pshim (DolanVarianceKind dv)) polarity fa fb
    -> DolanArguments dv wa fa polarity t
    -> m (PolarShimWit (pshim Type) (Arguments wb fb) polarity t)
dolanArgumentsToArgumentsM' _ NilListType NilDolanVarianceMap NilDolanVarianceMap conv NilDolanArguments =
    return $ MkShimWit NilArguments conv
dolanArgumentsToArgumentsM' f (ConsListType Refl lc) (ConsDolanVarianceMap dvma) (ConsDolanVarianceMap dvmb) conv (ConsDolanArguments sta dta) = do
    Dict <- return $ covaryKMCategory @pshim lc
    Dict <- return $ applyFunctionKindWitness (inKind @_ @fa) sta
    MkShimWit ta conva <- f sta
    Dict <- return $ applyFunctionKindWitness (inKind @_ @fa) ta
    Dict <- return $ applyFunctionKindWitness (inKind @_ @fb) ta
    MkShimWit tfa convfa <- dolanArgumentsToArgumentsM' f lc dvma dvmb (polarMapTypeApply CovarianceType conv conva) dta
    return $ MkShimWit (ConsArguments ta tfa) convfa

dolanArgumentsToArgumentsM ::
       forall m (pshim :: PolyShimKind) wa wb dv polarity f t.
       (Monad m, DolanVarianceInCategory pshim, Is PolarityType polarity)
    => (forall t'. wa polarity t' -> m (PolarShimWit (pshim Type) wb polarity t'))
    -> CovaryType dv
    -> CovaryMap f
    -> DolanArguments dv wa f polarity t
    -> m (PolarShimWit (pshim Type) (Arguments wb f) polarity t)
dolanArgumentsToArgumentsM f lc covary args =
    case covaryMapInKind covary of
        Dict -> let
            dvm = covaryToDolanVarianceMap lc covary
            conv =
                case covaryKMCategory @pshim lc of
                    Dict -> cid
            in dolanArgumentsToArgumentsM' f lc dvm dvm conv args

dolanArgumentsToArguments ::
       forall (pshim :: PolyShimKind) wa wb dv polarity f t. (DolanVarianceInCategory pshim, Is PolarityType polarity)
    => (forall t'. wa polarity t' -> PolarShimWit (pshim Type) wb polarity t')
    -> CovaryType dv
    -> CovaryMap f
    -> DolanArguments dv wa f polarity t
    -> PolarShimWit (pshim Type) (Arguments wb f) polarity t
dolanArgumentsToArguments f lc covary args =
    runIdentity $ dolanArgumentsToArgumentsM (\wt -> Identity $ f wt) lc covary args

argumentsToDolanArgumentsM' ::
       forall m (pshim :: PolyShimKind) wa wb dv polarity (fa :: DolanVarianceKind dv) (fb :: DolanVarianceKind dv) t.
       (Monad m, DolanVarianceInCategory pshim, Is PolarityType polarity)
    => (forall t'. wa t' -> m (PShimWit (pshim Type) wb polarity t'))
    -> CovaryType dv
    -> CovaryMap fa
    -> CovaryMap fb
    -> PolarMap (pshim (DolanVarianceKind dv)) polarity fa fb
    -> Arguments wa fa t
    -> m (PShimWit (pshim Type) (DolanArguments dv wb fb) polarity t)
argumentsToDolanArgumentsM' _ NilListType NilCovaryMap NilCovaryMap conv NilArguments =
    return $ MkShimWit NilDolanArguments conv
argumentsToDolanArgumentsM' f (ConsListType Refl ct) (ConsCovaryMap mma) (ConsCovaryMap mmb) conv (ConsArguments arg args) = do
    Dict <- return $ covaryKMCategory @pshim ct
    Dict <- return $ applyFunctionKindWitness (inKind @_ @fa) arg
    MkShimWit ta conva <- f arg
    Dict <- return $ applyFunctionKindWitness (inKind @_ @fa) ta
    Dict <- return $ applyFunctionKindWitness (inKind @_ @fb) ta
    MkShimWit tfa convfa <- argumentsToDolanArgumentsM' f ct mma mmb (polarMapTypeApply CovarianceType conv conva) args
    return $ MkShimWit (ConsDolanArguments ta tfa) convfa

argumentsToDolanArgumentsM ::
       forall m (pshim :: PolyShimKind) wa wb dv polarity f t.
       (Monad m, DolanVarianceInCategory pshim, Is PolarityType polarity)
    => (forall t'. wa t' -> m (PShimWit (pshim Type) wb polarity t'))
    -> CovaryType dv
    -> CovaryMap f
    -> Arguments wa f t
    -> m (PShimWit (pshim Type) (DolanArguments dv wb f) polarity t)
argumentsToDolanArgumentsM f ct cm args =
    argumentsToDolanArgumentsM'
        f
        ct
        cm
        cm
        (case covaryKMCategory @pshim ct of
             Dict ->
                 case covaryMapInKind cm of
                     Dict -> cid)
        args

argumentsToDolanArguments ::
       forall (pshim :: PolyShimKind) wa wb dv polarity f t. (DolanVarianceInCategory pshim, Is PolarityType polarity)
    => (forall t'. wa t' -> PShimWit (pshim Type) wb polarity t')
    -> CovaryType dv
    -> CovaryMap f
    -> Arguments wa f t
    -> PShimWit (pshim Type) (DolanArguments dv wb f) polarity t
argumentsToDolanArguments f ct cm args = runIdentity $ argumentsToDolanArgumentsM (\wt -> Identity $ f wt) ct cm args

singleArgumentTestEquality ::
       forall sv f polarity a b. (TestEquality (f polarity), TestEquality (f (InvertPolarity polarity)))
    => VarianceType sv
    -> SingleArgument sv f polarity a
    -> SingleArgument sv f polarity b
    -> Maybe (a :~: b)
singleArgumentTestEquality CovarianceType = testEquality
singleArgumentTestEquality ContravarianceType = testEquality
singleArgumentTestEquality RangevarianceType = testEquality

dolanTestEquality ::
       forall dv f gt polarity a b. (TestEquality (f polarity), TestEquality (f (InvertPolarity polarity)))
    => DolanVarianceType dv
    -> DolanArguments dv f gt polarity a
    -> DolanArguments dv f gt polarity b
    -> Maybe (a :~: b)
dolanTestEquality NilListType NilDolanArguments NilDolanArguments = Just Refl
dolanTestEquality (ConsListType sv dv) (ConsDolanArguments ta tta) (ConsDolanArguments tb ttb) = do
    Refl <- singleArgumentTestEquality @_ @f @polarity sv ta tb
    Refl <- dolanTestEquality dv tta ttb
    return Refl
