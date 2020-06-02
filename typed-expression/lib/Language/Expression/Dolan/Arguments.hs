module Language.Expression.Dolan.Arguments
    ( SingleArgument
    , DolanArguments(..)
    , mapDolanArgumentsType
    , mapDolanArgumentsM
    , mapDolanArguments
    , mapInvertDolanArgumentsM
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

type ArgTypeF :: (Type -> Type -> Type) -> forall (sv :: Variance) ->
                                                   (Polarity -> Type -> Type) -> Polarity -> VarianceKind sv -> Type
data ArgTypeF map sv ft polarity t where
    MkArgTypeF
        :: InVarianceKind sv t'
        => SingleArgument sv ft polarity t'
        -> PolarVarianceMap map polarity sv t t'
        -> ArgTypeF map sv ft polarity t

mapArgTypeF ::
       forall m (map :: Type -> Type -> Type) (fta :: Polarity -> Type -> Type) (ftb :: Polarity -> Type -> Type) sv polarity t.
       (Monad m, Is PolarityType polarity)
    => VarianceType sv
    -> (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> m (PShimWit map ftb polarity' t'))
    -> SingleArgument sv fta polarity t
    -> m (ArgTypeF map sv ftb polarity t)
mapArgTypeF CovarianceType f arg = do
    tf <- f arg
    return $ unShimWit tf MkArgTypeF
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
       forall m (pmap :: PolyMapKind) fta ftb dv polarity (gt :: DolanVarianceKind dv) (gt' :: DolanVarianceKind dv) t.
       (Monad m, DolanVarianceInCategory pmap, Is PolarityType polarity)
    => (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> m (PShimWit (pmap Type) ftb polarity' t'))
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> DolanVarianceMap dv gt'
    -> DolanArguments dv fta gt polarity t
    -> PolarMap (pmap (DolanVarianceKind dv)) polarity gt gt'
    -> m (PShimWit (pmap Type) (DolanArguments dv ftb gt') polarity t)
mapArgsTypeF _ NilListType NilDolanVarianceMap NilDolanVarianceMap NilDolanArguments conv =
    return $ MkShimWit NilDolanArguments conv
mapArgsTypeF f (ConsListType svt dvt) (ConsDolanVarianceMap dvm) (ConsDolanVarianceMap dvm') (ConsDolanArguments sta dta) conv = do
    MkArgTypeF sta' svf <- mapArgTypeF @m @(pmap Type) @fta @ftb @_ @polarity svt f sta
    Dict <- return $ varianceCoercibleKind svt
    Dict <- return $ dolanVarianceInCategory @pmap dvt
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gt) sta
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gt) sta'
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gt') sta'
    MkShimWit dta' conv' <-
        mapArgsTypeF @m @pmap @fta @ftb @_ @polarity f dvt dvm dvm' dta (polarMapTypeApply svt conv svf)
    return $ MkShimWit (ConsDolanArguments sta' dta') conv'

mapDolanArgumentsType ::
       forall (pmap :: PolyMapKind) ft dv polarity (gt :: DolanVarianceKind dv) (gt' :: DolanVarianceKind dv) t.
       (DolanVarianceInCategory pmap, Is PolarityType polarity)
    => DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> DolanVarianceMap dv gt'
    -> DolanArguments dv ft gt polarity t
    -> PolarMap (pmap (DolanVarianceKind dv)) polarity gt gt'
    -> PShimWit (pmap Type) (DolanArguments dv ft gt') polarity t
mapDolanArgumentsType dt dvma dvmb args f = runIdentity $ mapArgsTypeF (pure . mkShimWit) dt dvma dvmb args f

mapDolanArgumentsM ::
       forall m (pmap :: PolyMapKind) fta ftb dv polarity gt t.
       (Monad m, DolanVarianceInCategory pmap, Is PolarityType polarity)
    => (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> m (PShimWit (pmap Type) ftb polarity' t'))
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> DolanArguments dv fta gt polarity t
    -> m (PShimWit (pmap Type) (DolanArguments dv ftb gt) polarity t)
mapDolanArgumentsM f dvt dvm args =
    case dolanVarianceInCategory @pmap dvt of
        Dict ->
            case dolanVarianceMapInKind dvm of
                Dict -> mapArgsTypeF f dvt dvm dvm args cid

mapDolanArguments ::
       forall (pmap :: PolyMapKind) fta ftb dv polarity gt t. (DolanVarianceInCategory pmap, Is PolarityType polarity)
    => (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> PShimWit (pmap Type) ftb polarity' t')
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> DolanArguments dv fta gt polarity t
    -> PShimWit (pmap Type) (DolanArguments dv ftb gt) polarity t
mapDolanArguments f dvt kv args = runIdentity $ mapDolanArgumentsM (\t -> return $ f t) dvt kv args

mapInvertArgTypeF ::
       forall m map fta ftb sv polarity t. (Monad m, Is PolarityType polarity)
    => VarianceType sv
    -> (forall polarity' t'.
            Is PolarityType polarity' => fta polarity' t' -> m (PShimWit map ftb (InvertPolarity polarity') t'))
    -> SingleArgument sv fta polarity t
    -> m (ArgTypeF map sv ftb (InvertPolarity polarity) t)
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
       forall m (pmap :: PolyMapKind) fta ftb dv polarity (gt :: DolanVarianceKind dv) (gt' :: DolanVarianceKind dv) t.
       (Monad m, DolanVarianceInCategory pmap, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' => fta polarity' t' -> m (PShimWit (pmap Type) ftb (InvertPolarity polarity') t'))
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> DolanVarianceMap dv gt'
    -> DolanArguments dv fta gt polarity t
    -> PolarMap (pmap (DolanVarianceKind dv)) (InvertPolarity polarity) gt gt'
    -> m (PShimWit (pmap Type) (DolanArguments dv ftb gt') (InvertPolarity polarity) t)
mapInvertArgsTypeF _ NilListType NilDolanVarianceMap NilDolanVarianceMap NilDolanArguments conv =
    return $ MkShimWit NilDolanArguments conv
mapInvertArgsTypeF f (ConsListType svt dvt) (ConsDolanVarianceMap dvm) (ConsDolanVarianceMap dvm') (ConsDolanArguments sta dta) conv = do
    MkArgTypeF sta' svf <- mapInvertArgTypeF @m @(pmap Type) @fta @ftb @_ @polarity svt f sta
    Dict <- return $ varianceCoercibleKind svt
    Dict <- return $ dolanVarianceInCategory @pmap dvt
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gt) sta
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gt) sta'
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gt') sta'
    MkShimWit dta' conv' <-
        invertPolarity @polarity $
        mapInvertArgsTypeF @m @pmap @fta @ftb @_ @polarity f dvt dvm dvm' dta (polarMapTypeApply svt conv svf)
    return $ MkShimWit (ConsDolanArguments sta' dta') conv'

mapInvertDolanArgumentsM ::
       forall m (pmap :: PolyMapKind) fta ftb dv polarity gt t.
       (Monad m, DolanVarianceInCategory pmap, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' => fta polarity' t' -> m (PShimWit (pmap Type) ftb (InvertPolarity polarity') t'))
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> DolanArguments dv fta gt polarity t
    -> m (PShimWit (pmap Type) (DolanArguments dv ftb gt) (InvertPolarity polarity) t)
mapInvertDolanArgumentsM f dvt dvm args =
    case dolanVarianceInCategory @pmap dvt of
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
       forall map fta ftb ftab sv polarity ta tb. Is PolarityType polarity
    => VarianceType sv
    -> (forall polarity' ta' tb'.
            Is PolarityType polarity' =>
                    fta polarity' ta' -> ftb polarity' tb' -> PShimWit map ftab polarity' (JoinMeetType polarity' ta' tb'))
    -> SingleArgument sv fta polarity ta
    -> SingleArgument sv ftb polarity tb
    -> ArgTypeF map sv ftab polarity (SVJoinMeetType sv polarity ta tb)
mergeArgTypeF CovarianceType f arga argb =
    case f arga argb of
        MkShimWit argab conv -> MkArgTypeF argab conv
mergeArgTypeF ContravarianceType f arga argb =
    invertPolarity @polarity $
    case f arga argb of
        MkShimWit argab conv -> MkArgTypeF argab $ mkContravariantPolarMap conv
mergeArgTypeF RangevarianceType f (MkRangeType tpa tqa) (MkRangeType tpb tqb) =
    invertPolarity @polarity $
    case f tpa tpb of
        MkShimWit tpab convp ->
            case f tqa tqb of
                MkShimWit tqab convq -> MkArgTypeF (MkRangeType tpab tqab) $ mkRangevariantPolarMap convp convq

varPolar1 ::
       forall map polarity sv a b.
       (JoinMeetCategory map, Is PolarityType polarity, InVarianceKind sv a, InVarianceKind sv b)
    => VarianceType sv
    -> PolarVarianceMap map polarity sv a (SVJoinMeetType sv polarity a b)
varPolar1 CovarianceType = polar1
varPolar1 ContravarianceType = invertPolarity @polarity $ mkContravariantPolarMap polar1
varPolar1 RangevarianceType =
    invertPolarity @polarity $
    case (inKind @_ @a, inKind @_ @b) of
        (MkPairWitness, MkPairWitness) -> mkRangevariantPolarMap polar1 polar1

varPolar2 ::
       forall map polarity sv a b.
       (JoinMeetCategory map, Is PolarityType polarity, InVarianceKind sv a, InVarianceKind sv b)
    => VarianceType sv
    -> PolarVarianceMap map polarity sv b (SVJoinMeetType sv polarity a b)
varPolar2 CovarianceType = polar2
varPolar2 ContravarianceType = invertPolarity @polarity $ mkContravariantPolarMap polar2
varPolar2 RangevarianceType =
    invertPolarity @polarity $
    case (inKind @_ @a, inKind @_ @b) of
        (MkPairWitness, MkPairWitness) -> mkRangevariantPolarMap polar2 polar2

mergeArgsTypeF ::
       forall (pmap :: PolyMapKind) (fta :: Polarity -> Type -> Type) (ftb :: Polarity -> Type -> Type) (ftab :: Polarity -> Type -> Type) (dv :: DolanVariance) (polarity :: Polarity) (gta :: DolanVarianceKind dv) (gtb :: DolanVarianceKind dv) (gtab :: DolanVarianceKind dv) (ta :: Type) (tb :: Type).
       (DolanVarianceInCategory pmap, JoinMeetCategory (pmap Type), Is PolarityType polarity)
    => (forall polarity' ta' tb'.
            Is PolarityType polarity' =>
                    fta polarity' ta' -> ftb polarity' tb' -> PShimWit (pmap Type) ftab polarity' (JoinMeetType polarity' ta' tb'))
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gta
    -> DolanVarianceMap dv gtb
    -> DolanVarianceMap dv gtab
    -> DolanArguments dv fta gta polarity ta
    -> DolanArguments dv ftb gtb polarity tb
    -> PolarMap (pmap (DolanVarianceKind dv)) polarity gta gtab
    -> PolarMap (pmap (DolanVarianceKind dv)) polarity gtb gtab
    -> PShimWit (pmap Type) (DolanArguments dv ftab gtab) polarity (JoinMeetType polarity ta tb)
mergeArgsTypeF _ NilListType NilDolanVarianceMap NilDolanVarianceMap NilDolanVarianceMap NilDolanArguments NilDolanArguments conva convb =
    MkShimWit NilDolanArguments $ polarF conva convb
mergeArgsTypeF f (ConsListType svt dvt) (ConsDolanVarianceMap dvma) (ConsDolanVarianceMap dvmb) (ConsDolanVarianceMap dvmab) (ConsDolanArguments (sta :: _ ta0) dta) (ConsDolanArguments (stb :: _ tb0) dtb) conva convb =
    case varianceCoercibleKind svt of
        Dict ->
            case mergeArgTypeF @(pmap Type) @fta @ftb @ftab @_ @polarity svt f sta stb of
                MkArgTypeF stab svf ->
                    case dolanVarianceInCategory @pmap dvt of
                        Dict ->
                            case applyFunctionKindWitness (inKind @_ @gta) sta of
                                Dict ->
                                    case applyFunctionKindWitness (inKind @_ @gtb) stb of
                                        Dict ->
                                            case applyFunctionKindWitness (inKind @_ @gta) stab of
                                                Dict ->
                                                    case applyFunctionKindWitness (inKind @_ @gtb) stab of
                                                        Dict ->
                                                            case applyFunctionKindWitness (inKind @_ @gtab) stab of
                                                                Dict ->
                                                                    case varianceInCategory
                                                                             @(PolarMap (pmap Type) polarity)
                                                                             svt of
                                                                        Dict ->
                                                                            case svJoinMeetTypeInKind
                                                                                     @_
                                                                                     @polarity
                                                                                     @ta0
                                                                                     @tb0
                                                                                     svt of
                                                                                Dict ->
                                                                                    case mergeArgsTypeF
                                                                                             @pmap
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
                                                                                             (polarMapTypeApply
                                                                                                  svt
                                                                                                  conva $
                                                                                              svf <.>
                                                                                              varPolar1
                                                                                                  @(pmap Type)
                                                                                                  @polarity
                                                                                                  @_
                                                                                                  @ta0
                                                                                                  @tb0
                                                                                                  svt)
                                                                                             (polarMapTypeApply
                                                                                                  svt
                                                                                                  convb $
                                                                                              svf <.>
                                                                                              varPolar2
                                                                                                  @(pmap Type)
                                                                                                  @polarity
                                                                                                  @_
                                                                                                  @ta0
                                                                                                  @tb0
                                                                                                  svt) of
                                                                                        MkShimWit dtab convab ->
                                                                                            MkShimWit
                                                                                                (ConsDolanArguments
                                                                                                     stab
                                                                                                     dtab)
                                                                                                convab

mergeDolanArguments ::
       forall (pmap :: PolyMapKind) fta ftb ftab dv polarity gt ta tb.
       (DolanVarianceInCategory pmap, JoinMeetCategory (pmap Type), Is PolarityType polarity)
    => (forall polarity' ta' tb'.
            Is PolarityType polarity' =>
                    fta polarity' ta' -> ftb polarity' tb' -> PShimWit (pmap Type) ftab polarity' (JoinMeetType polarity' ta' tb'))
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> DolanArguments dv fta gt polarity ta
    -> DolanArguments dv ftb gt polarity tb
    -> PShimWit (pmap Type) (DolanArguments dv ftab gt) polarity (JoinMeetType polarity ta tb)
mergeDolanArguments f dvt dvm argsa argsb =
    case dolanVarianceInCategory @pmap dvt of
        Dict ->
            case dolanVarianceMapInKind dvm of
                Dict -> mergeArgsTypeF @pmap f dvt dvm dvm dvm argsa argsb cid cid

dolanArgumentsToArgumentsM' ::
       forall m (pmap :: PolyMapKind) wa wb dv polarity (fa :: DolanVarianceKind dv) (fb :: DolanVarianceKind dv) t.
       (Monad m, DolanVarianceInCategory pmap, Is PolarityType polarity)
    => (forall t'. wa polarity t' -> m (ShimWit (pmap Type) wb polarity t'))
    -> CovaryType dv
    -> DolanVarianceMap dv fa
    -> DolanVarianceMap dv fb
    -> PolarMap (pmap (DolanVarianceKind dv)) polarity fa fb
    -> DolanArguments dv wa fa polarity t
    -> m (ShimWit (pmap Type) (Arguments wb fb) polarity t)
dolanArgumentsToArgumentsM' _ NilListType NilDolanVarianceMap NilDolanVarianceMap conv NilDolanArguments =
    return $ MkShimWit NilArguments conv
dolanArgumentsToArgumentsM' f (ConsListType Refl lc) (ConsDolanVarianceMap dvma) (ConsDolanVarianceMap dvmb) conv (ConsDolanArguments sta dta) = do
    Dict <- return $ covaryKMCategory @pmap lc
    Dict <- return $ applyFunctionKindWitness (inKind @_ @fa) sta
    MkShimWit ta conva <- f sta
    Dict <- return $ applyFunctionKindWitness (inKind @_ @fa) ta
    Dict <- return $ applyFunctionKindWitness (inKind @_ @fb) ta
    MkShimWit tfa convfa <- dolanArgumentsToArgumentsM' f lc dvma dvmb (polarMapTypeApply CovarianceType conv conva) dta
    return $ MkShimWit (ConsArguments ta tfa) convfa

dolanArgumentsToArgumentsM ::
       forall m (pmap :: PolyMapKind) wa wb dv polarity f t.
       (Monad m, DolanVarianceInCategory pmap, Is PolarityType polarity)
    => (forall t'. wa polarity t' -> m (ShimWit (pmap Type) wb polarity t'))
    -> CovaryType dv
    -> CovaryMap f
    -> DolanArguments dv wa f polarity t
    -> m (ShimWit (pmap Type) (Arguments wb f) polarity t)
dolanArgumentsToArgumentsM f lc covary args =
    case covaryMapInKind covary of
        Dict -> let
            dvm = covaryToDolanVarianceMap lc covary
            conv =
                case covaryKMCategory @pmap lc of
                    Dict -> cid
            in dolanArgumentsToArgumentsM' f lc dvm dvm conv args

dolanArgumentsToArguments ::
       forall (pmap :: PolyMapKind) wa wb dv polarity f t. (DolanVarianceInCategory pmap, Is PolarityType polarity)
    => (forall t'. wa polarity t' -> ShimWit (pmap Type) wb polarity t')
    -> CovaryType dv
    -> CovaryMap f
    -> DolanArguments dv wa f polarity t
    -> ShimWit (pmap Type) (Arguments wb f) polarity t
dolanArgumentsToArguments f lc covary args =
    runIdentity $ dolanArgumentsToArgumentsM (\wt -> Identity $ f wt) lc covary args

argumentsToDolanArgumentsM' ::
       forall m (pmap :: PolyMapKind) wa wb dv polarity (fa :: DolanVarianceKind dv) (fb :: DolanVarianceKind dv) t.
       (Monad m, DolanVarianceInCategory pmap, Is PolarityType polarity)
    => (forall t'. wa t' -> m (PShimWit (pmap Type) wb polarity t'))
    -> CovaryType dv
    -> CovaryMap fa
    -> CovaryMap fb
    -> PolarMap (pmap (DolanVarianceKind dv)) polarity fa fb
    -> Arguments wa fa t
    -> m (PShimWit (pmap Type) (DolanArguments dv wb fb) polarity t)
argumentsToDolanArgumentsM' _ NilListType NilCovaryMap NilCovaryMap conv NilArguments =
    return $ MkShimWit NilDolanArguments conv
argumentsToDolanArgumentsM' f (ConsListType Refl ct) (ConsCovaryMap mma) (ConsCovaryMap mmb) conv (ConsArguments arg args) = do
    Dict <- return $ covaryKMCategory @pmap ct
    Dict <- return $ applyFunctionKindWitness (inKind @_ @fa) arg
    MkShimWit ta conva <- f arg
    Dict <- return $ applyFunctionKindWitness (inKind @_ @fa) ta
    Dict <- return $ applyFunctionKindWitness (inKind @_ @fb) ta
    MkShimWit tfa convfa <- argumentsToDolanArgumentsM' f ct mma mmb (polarMapTypeApply CovarianceType conv conva) args
    return $ MkShimWit (ConsDolanArguments ta tfa) convfa

argumentsToDolanArgumentsM ::
       forall m (pmap :: PolyMapKind) wa wb dv polarity f t.
       (Monad m, DolanVarianceInCategory pmap, Is PolarityType polarity)
    => (forall t'. wa t' -> m (PShimWit (pmap Type) wb polarity t'))
    -> CovaryType dv
    -> CovaryMap f
    -> Arguments wa f t
    -> m (PShimWit (pmap Type) (DolanArguments dv wb f) polarity t)
argumentsToDolanArgumentsM f ct cm args =
    argumentsToDolanArgumentsM'
        f
        ct
        cm
        cm
        (case covaryKMCategory @pmap ct of
             Dict ->
                 case covaryMapInKind cm of
                     Dict -> cid)
        args

argumentsToDolanArguments ::
       forall (pmap :: PolyMapKind) wa wb dv polarity f t. (DolanVarianceInCategory pmap, Is PolarityType polarity)
    => (forall t'. wa t' -> PShimWit (pmap Type) wb polarity t')
    -> CovaryType dv
    -> CovaryMap f
    -> Arguments wa f t
    -> PShimWit (pmap Type) (DolanArguments dv wb f) polarity t
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
