module Language.Expression.Dolan.Arguments
    ( SingleArgument
    , DolanArguments(..)
    , mapDolanArgumentsM
    , mapDolanArguments
    , mapInvertDolanArgumentsM
    , mergeDolanArguments
    , Arguments(..)
    , dolanArgumentsToArgumentsM
    , dolanArgumentsToArguments
    , argumentsToDolanArgumentsM
    , argumentsToDolanArguments
    ) where

import Data.Shim
import Language.Expression.Arguments
import Language.Expression.Dolan.Covariance
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Variance
import Shapes

type family SingleArgument (sv :: Variance) (ft :: Polarity -> Type -> Type) (polarity :: Polarity) :: VarianceKind sv -> Type where
    SingleArgument 'Covariance ft polarity = ft polarity
    SingleArgument 'Contravariance ft polarity = ft (InvertPolarity polarity)
    SingleArgument 'Rangevariance ft polarity = RangeType ft polarity

data DolanArguments (dv :: DolanVariance) (ft :: Polarity -> Type -> Type) (gt :: DolanVarianceKind dv) (polarity :: Polarity) (ta :: Type) where
    NilDolanArguments :: DolanArguments '[] ft t polarity t
    ConsDolanArguments
        :: forall sv dv ft gt polarity a ta. InVarianceKind sv a
        => SingleArgument sv ft polarity a
        -> DolanArguments dv ft (gt a) polarity ta
        -> DolanArguments (sv ': dv) ft gt polarity ta

type family PolarSingleVarianceFunc (cat :: Type -> Type -> Type) (polarity :: Polarity) (sv :: Variance) (a :: VarianceKind sv) (b :: VarianceKind sv) :: Type where
    PolarSingleVarianceFunc cat 'Positive sv a b = VarianceCategory cat sv a b
    PolarSingleVarianceFunc cat 'Negative sv a b = VarianceCategory cat sv b a

data ArgTypeF (cat :: Type -> Type -> Type) (sv :: Variance) (ft :: Polarity -> Type -> Type) (polarity :: Polarity) (t :: VarianceKind sv) :: Type where
    MkArgTypeF
        :: InVarianceKind sv t'
        => SingleArgument sv ft polarity t'
        -> PolarSingleVarianceFunc cat polarity sv t t'
        -> ArgTypeF cat sv ft polarity t

mapArgTypeF ::
       forall m (cat :: Type -> Type -> Type) (fta :: Polarity -> Type -> Type) (ftb :: Polarity -> Type -> Type) sv polarity t.
       (Monad m, Is PolarityType polarity)
    => VarianceType sv
    -> (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> m (PShimWit cat ftb polarity' t'))
    -> SingleArgument sv fta polarity t
    -> m (ArgTypeF cat sv ftb polarity t)
mapArgTypeF CovarianceType f arg = do
    tf <- f arg
    return $
        unShimWit tf $
        case representative @_ @_ @polarity of
            PositiveType -> MkArgTypeF
            NegativeType -> MkArgTypeF
mapArgTypeF ContravarianceType f arg =
    invertPolarity @polarity $ do
        MkShimWit arg' conv <- f arg
        return $
            MkArgTypeF arg' $
            case representative @_ @_ @polarity of
                PositiveType -> MkCatDual conv
                NegativeType -> MkCatDual conv
mapArgTypeF RangevarianceType f (MkRangeType tp tq) =
    invertPolarity @polarity $ do
        MkShimWit tp' convp <- f tp
        MkShimWit tq' convq <- f tq
        return $
            MkArgTypeF (MkRangeType tp' tq') $
            case representative @_ @_ @polarity of
                PositiveType -> MkCatRange convp convq
                NegativeType -> MkCatRange convp convq

mapArgsTypeF ::
       forall m (cat :: forall kc. kc -> kc -> Type) fta ftb dv polarity (gt :: DolanVarianceKind dv) (gt' :: DolanVarianceKind dv) t.
       (Monad m, DolanVarianceInCategory cat, Is PolarityType polarity)
    => (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> m (PShimWit cat ftb polarity' t'))
    -> DolanVarianceType dv
    -> DolanVarianceMap cat dv gt
    -> DolanVarianceMap cat dv gt'
    -> DolanArguments dv fta gt polarity t
    -> PolarMapType cat polarity gt gt'
    -> m (PShimWit cat (DolanArguments dv ftb gt') polarity t)
mapArgsTypeF _ NilListType NilDolanVarianceMap NilDolanVarianceMap NilDolanArguments conv =
    return $ MkShimWit NilDolanArguments conv
mapArgsTypeF f (ConsListType svt dvt) (ConsDolanVarianceMap dvm) (ConsDolanVarianceMap dvm') (ConsDolanArguments sta dta) conv = do
    MkArgTypeF sta' svf <- mapArgTypeF @m @cat @fta @ftb @_ @polarity svt f sta
    Dict <- return $ varianceCoercibleKind svt
    Dict <- return $ dolanVarianceInCategory @cat dvt
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gt) sta
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gt) sta'
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gt') sta'
    case representative @_ @_ @polarity of
        PositiveType -> do
            MkShimWit dta' conv' <-
                mapArgsTypeF @m @cat @fta @ftb @_ @polarity f dvt dvm dvm' dta (consShimFunc svt conv svf)
            return $ MkShimWit (ConsDolanArguments sta' dta') conv'
        NegativeType -> do
            MkShimWit dta' conv' <-
                mapArgsTypeF @m @cat @fta @ftb @_ @polarity f dvt dvm dvm' dta (consShimFunc svt conv svf)
            return $ MkShimWit (ConsDolanArguments sta' dta') conv'

mapDolanArgumentsM ::
       forall m (cat :: forall kc. kc -> kc -> Type) fta ftb dv polarity gt t.
       (Monad m, DolanVarianceInCategory cat, Is PolarityType polarity)
    => (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> m (PShimWit cat ftb polarity' t'))
    -> DolanVarianceType dv
    -> DolanVarianceMap cat dv gt
    -> DolanArguments dv fta gt polarity t
    -> m (PShimWit cat (DolanArguments dv ftb gt) polarity t)
mapDolanArgumentsM f dvt dvm args =
    case dolanVarianceInCategory @cat dvt of
        Dict ->
            case dolanVarianceMapInKind dvm of
                Dict ->
                    case representative @_ @_ @polarity of
                        PositiveType -> mapArgsTypeF f dvt dvm dvm args cid
                        NegativeType -> mapArgsTypeF f dvt dvm dvm args cid

mapDolanArguments ::
       forall (cat :: forall kc. kc -> kc -> Type) fta ftb dv polarity gt t.
       (DolanVarianceInCategory cat, Is PolarityType polarity)
    => (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> PShimWit cat ftb polarity' t')
    -> DolanVarianceType dv
    -> DolanVarianceMap cat dv gt
    -> DolanArguments dv fta gt polarity t
    -> PShimWit cat (DolanArguments dv ftb gt) polarity t
mapDolanArguments f dvt kv args = runIdentity $ mapDolanArgumentsM (\t -> return $ f t) dvt kv args

mapInvertArgTypeF ::
       forall m cat fta ftb sv polarity t. (Monad m, Is PolarityType polarity)
    => VarianceType sv
    -> (forall polarity' t'.
            Is PolarityType polarity' => fta polarity' t' -> m (PShimWit cat ftb (InvertPolarity polarity') t'))
    -> SingleArgument sv fta polarity t
    -> m (ArgTypeF cat sv ftb (InvertPolarity polarity) t)
mapInvertArgTypeF CovarianceType f arg = do
    MkShimWit arg' conv <- f arg
    return $
        MkArgTypeF arg' $
        case representative @_ @_ @polarity of
            PositiveType -> conv
            NegativeType -> conv
mapInvertArgTypeF ContravarianceType f arg =
    invertPolarity @polarity $ do
        MkShimWit arg' conv <- f arg
        return $
            MkArgTypeF arg' $
            case representative @_ @_ @polarity of
                PositiveType -> MkCatDual conv
                NegativeType -> MkCatDual conv
mapInvertArgTypeF RangevarianceType f (MkRangeType tp tq) =
    invertPolarity @polarity $ do
        MkShimWit tp' convp <- f tp
        MkShimWit tq' convq <- f tq
        return $
            MkArgTypeF (MkRangeType tp' tq') $
            case representative @_ @_ @polarity of
                PositiveType -> MkCatRange convp convq
                NegativeType -> MkCatRange convp convq

mapInvertArgsTypeF ::
       forall m (cat :: forall kc. kc -> kc -> Type) fta ftb dv polarity gt gt' t.
       (Monad m, DolanVarianceInCategory cat, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' => fta polarity' t' -> m (PShimWit cat ftb (InvertPolarity polarity') t'))
    -> DolanVarianceType dv
    -> DolanVarianceMap cat dv gt
    -> DolanVarianceMap cat dv gt'
    -> DolanArguments dv fta gt polarity t
    -> PolarMapType cat (InvertPolarity polarity) gt gt'
    -> m (PShimWit cat (DolanArguments dv ftb gt') (InvertPolarity polarity) t)
mapInvertArgsTypeF _ NilListType NilDolanVarianceMap NilDolanVarianceMap NilDolanArguments conv =
    return $ MkShimWit NilDolanArguments conv
mapInvertArgsTypeF f (ConsListType svt dvt) (ConsDolanVarianceMap dvm) (ConsDolanVarianceMap dvm') (ConsDolanArguments sta dta) conv = do
    MkArgTypeF sta' svf <- mapInvertArgTypeF @m @cat @fta @ftb @_ @polarity svt f sta
    Dict <- return $ varianceCoercibleKind svt
    Dict <- return $ dolanVarianceInCategory @cat dvt
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gt) sta
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gt) sta'
    Dict <- return $ applyFunctionKindWitness (inKind @_ @gt') sta'
    case representative @_ @_ @polarity of
        PositiveType -> do
            MkShimWit dta' conv' <-
                mapInvertArgsTypeF @m @cat @fta @ftb @_ @polarity f dvt dvm dvm' dta (consShimFunc svt conv svf)
            return $ MkShimWit (ConsDolanArguments sta' dta') conv'
        NegativeType -> do
            MkShimWit dta' conv' <-
                mapInvertArgsTypeF @m @cat @fta @ftb @_ @polarity f dvt dvm dvm' dta (consShimFunc svt conv svf)
            return $ MkShimWit (ConsDolanArguments sta' dta') conv'

mapInvertDolanArgumentsM ::
       forall m (cat :: forall kc. kc -> kc -> Type) fta ftb dv polarity gt t.
       (Monad m, DolanVarianceInCategory cat, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' => fta polarity' t' -> m (PShimWit cat ftb (InvertPolarity polarity') t'))
    -> DolanVarianceType dv
    -> DolanVarianceMap cat dv gt
    -> DolanArguments dv fta gt polarity t
    -> m (PShimWit cat (DolanArguments dv ftb gt) (InvertPolarity polarity) t)
mapInvertDolanArgumentsM f dvt dvm args =
    case dolanVarianceInCategory @cat dvt of
        Dict ->
            case dolanVarianceMapInKind dvm of
                Dict ->
                    case representative @_ @_ @polarity of
                        PositiveType -> mapInvertArgsTypeF f dvt dvm dvm args cid
                        NegativeType -> mapInvertArgsTypeF f dvt dvm dvm args cid

type family PositiveSVJoinMeetType (sv :: Variance) (a :: VarianceKind sv) (b :: VarianceKind sv) = (r :: VarianceKind sv) | r -> sv a b where
    PositiveSVJoinMeetType 'Covariance a b = JoinType a b
    PositiveSVJoinMeetType 'Contravariance a b = MeetType a b
    PositiveSVJoinMeetType 'Rangevariance ('( pa, qa)) ('( pb, qb)) = '( MeetType pa pb, JoinType qa qb)

type family NegativeSVJoinMeetType (sv :: Variance) (a :: VarianceKind sv) (b :: VarianceKind sv) = (r :: VarianceKind sv) | r -> sv a b where
    NegativeSVJoinMeetType 'Covariance a b = MeetType a b
    NegativeSVJoinMeetType 'Contravariance a b = JoinType a b
    NegativeSVJoinMeetType 'Rangevariance ('( pa, qa)) ('( pb, qb)) = '( JoinType pa pb, MeetType qa qb)

type family SVJoinMeetType (sv :: Variance) (polarity :: Polarity) (a :: VarianceKind sv) (b :: VarianceKind sv) = (r :: VarianceKind sv) where
    SVJoinMeetType sv 'Positive a b = PositiveSVJoinMeetType sv a b
    SVJoinMeetType sv 'Negative a b = NegativeSVJoinMeetType sv a b

mergeArgTypeF ::
       forall fta ftb ftab sv polarity ta tb. Is PolarityType polarity
    => VarianceType sv
    -> (forall polarity' ta' tb'.
            Is PolarityType polarity' =>
                    fta polarity' ta' -> ftb polarity' tb' -> PJMShimWit ftab polarity' (JoinMeetType polarity' ta' tb'))
    -> SingleArgument sv fta polarity ta
    -> SingleArgument sv ftb polarity tb
    -> ArgTypeF JMShim sv ftab polarity (SVJoinMeetType sv polarity ta tb)
mergeArgTypeF CovarianceType f arga argb =
    case f arga argb of
        MkShimWit argab conv ->
            case representative @_ @_ @polarity of
                PositiveType -> MkArgTypeF argab conv
                NegativeType -> MkArgTypeF argab conv
mergeArgTypeF ContravarianceType f arga argb =
    invertPolarity @polarity $
    case f arga argb of
        MkShimWit argab conv ->
            case representative @_ @_ @polarity of
                PositiveType -> MkArgTypeF argab $ MkCatDual conv
                NegativeType -> MkArgTypeF argab $ MkCatDual conv
mergeArgTypeF RangevarianceType f (MkRangeType tpa tqa) (MkRangeType tpb tqb) =
    invertPolarity @polarity $
    case f tpa tpb of
        MkShimWit tpab convp ->
            case f tqa tqb of
                MkShimWit tqab convq ->
                    case representative @_ @_ @polarity of
                        PositiveType -> MkArgTypeF (MkRangeType tpab tqab) $ MkCatRange convp convq
                        NegativeType -> MkArgTypeF (MkRangeType tpab tqab) $ MkCatRange convp convq

psvf1 ::
       forall polarity sv a b c. (Is PolarityType polarity, InVarianceKind sv a, InVarianceKind sv b)
    => VarianceType sv
    -> PolarSingleVarianceFunc JMShim polarity sv (SVJoinMeetType sv polarity a b) c
    -> PolarSingleVarianceFunc JMShim polarity sv a c
psvf1 =
    case representative @_ @_ @polarity of
        PositiveType ->
            \case
                CovarianceType -> \conv -> conv . join1
                ContravarianceType -> \(MkCatDual conv) -> MkCatDual $ meet1 . conv
                RangevarianceType ->
                    \(MkCatRange convp convq) ->
                        case (inKind @_ @a, inKind @_ @b) of
                            (MkPairWitness, MkPairWitness) -> MkCatRange (meet1 . convp) (convq . join1)
        NegativeType ->
            \case
                CovarianceType -> \conv -> meet1 . conv
                ContravarianceType -> \(MkCatDual conv) -> MkCatDual $ conv . join1
                RangevarianceType ->
                    \(MkCatRange convp convq) ->
                        case (inKind @_ @a, inKind @_ @b) of
                            (MkPairWitness, MkPairWitness) -> MkCatRange (convp . join1) (meet1 . convq)

psvf2 ::
       forall polarity sv a b c. (Is PolarityType polarity, InVarianceKind sv a, InVarianceKind sv b)
    => VarianceType sv
    -> PolarSingleVarianceFunc JMShim polarity sv (SVJoinMeetType sv polarity a b) c
    -> PolarSingleVarianceFunc JMShim polarity sv b c
psvf2 =
    case representative @_ @_ @polarity of
        PositiveType ->
            \case
                CovarianceType -> \conv -> conv . join2
                ContravarianceType -> \(MkCatDual conv) -> MkCatDual $ meet2 . conv
                RangevarianceType ->
                    \(MkCatRange convp convq) ->
                        case (inKind @_ @a, inKind @_ @b) of
                            (MkPairWitness, MkPairWitness) -> MkCatRange (meet2 . convp) (convq . join2)
        NegativeType ->
            \case
                CovarianceType -> \conv -> meet2 . conv
                ContravarianceType -> \(MkCatDual conv) -> MkCatDual $ conv . join2
                RangevarianceType ->
                    \(MkCatRange convp convq) ->
                        case (inKind @_ @a, inKind @_ @b) of
                            (MkPairWitness, MkPairWitness) -> MkCatRange (convp . join2) (meet2 . convq)

mergeArgsTypeF ::
       forall (fta :: Polarity -> Type -> Type) (ftb :: Polarity -> Type -> Type) (ftab :: Polarity -> Type -> Type) (dv :: DolanVariance) (polarity :: Polarity) (gta :: DolanVarianceKind dv) (gtb :: DolanVarianceKind dv) (gtab :: DolanVarianceKind dv) (ta :: Type) (tb :: Type).
       Is PolarityType polarity
    => (forall polarity' ta' tb'.
            Is PolarityType polarity' =>
                    fta polarity' ta' -> ftb polarity' tb' -> PJMShimWit ftab polarity' (JoinMeetType polarity' ta' tb'))
    -> DolanVarianceType dv
    -> DolanVarianceMap JMShim dv gta
    -> DolanVarianceMap JMShim dv gtb
    -> DolanVarianceMap JMShim dv gtab
    -> DolanArguments dv fta gta polarity ta
    -> DolanArguments dv ftb gtb polarity tb
    -> PolarMapType JMShim polarity gta gtab
    -> PolarMapType JMShim polarity gtb gtab
    -> PJMShimWit (DolanArguments dv ftab gtab) polarity (JoinMeetType polarity ta tb)
mergeArgsTypeF _ NilListType NilDolanVarianceMap NilDolanVarianceMap NilDolanVarianceMap NilDolanArguments NilDolanArguments conva convb =
    MkShimWit NilDolanArguments $
    case representative @_ @_ @polarity of
        PositiveType -> joinf conva convb
        NegativeType -> meetf conva convb
mergeArgsTypeF f (ConsListType svt dvt) (ConsDolanVarianceMap dvma) (ConsDolanVarianceMap dvmb) (ConsDolanVarianceMap dvmab) (ConsDolanArguments sta dta) (ConsDolanArguments stb dtb) conva convb =
    case varianceCoercibleKind svt of
        Dict ->
            case mergeArgTypeF @fta @ftb @ftab @_ @polarity svt f sta stb of
                MkArgTypeF stab svf ->
                    case dolanVarianceInCategory @JMShim dvt of
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
                                                                    case representative @_ @_ @polarity of
                                                                        PositiveType ->
                                                                            case mergeArgsTypeF
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
                                                                                     (consShimFunc svt conva $
                                                                                      psvf1 @polarity svt svf)
                                                                                     (consShimFunc svt convb $
                                                                                      psvf2 @polarity svt svf) of
                                                                                MkShimWit dtab convab ->
                                                                                    MkShimWit
                                                                                        (ConsDolanArguments stab dtab)
                                                                                        convab
                                                                        NegativeType ->
                                                                            case mergeArgsTypeF
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
                                                                                     (consShimFunc svt conva $
                                                                                      psvf1 @polarity svt svf)
                                                                                     (consShimFunc svt convb $
                                                                                      psvf2 @polarity svt svf) of
                                                                                MkShimWit dtab convab ->
                                                                                    MkShimWit
                                                                                        (ConsDolanArguments stab dtab)
                                                                                        convab

mergeDolanArguments ::
       forall fta ftb ftab dv polarity gt ta tb. Is PolarityType polarity
    => (forall polarity' ta' tb'.
            Is PolarityType polarity' =>
                    fta polarity' ta' -> ftb polarity' tb' -> PJMShimWit ftab polarity' (JoinMeetType polarity' ta' tb'))
    -> DolanVarianceType dv
    -> DolanVarianceMap JMShim dv gt
    -> DolanArguments dv fta gt polarity ta
    -> DolanArguments dv ftb gt polarity tb
    -> PJMShimWit (DolanArguments dv ftab gt) polarity (JoinMeetType polarity ta tb)
mergeDolanArguments f dvt dvm argsa argsb =
    case dolanVarianceInCategory @JMShim dvt of
        Dict ->
            case dolanVarianceMapInKind dvm of
                Dict ->
                    case representative @_ @_ @polarity of
                        PositiveType -> mergeArgsTypeF f dvt dvm dvm dvm argsa argsb cid cid
                        NegativeType -> mergeArgsTypeF f dvt dvm dvm dvm argsa argsb cid cid

dolanArgumentsToArgumentsM' ::
       forall m (cat :: forall kc. kc -> kc -> Type) wa wb dv polarity fa fb t.
       (Monad m, DolanVarianceInCategory cat, Is PolarityType polarity)
    => (forall t'. wa polarity t' -> m (ShimWit cat wb polarity t'))
    -> CovaryType dv
    -> DolanVarianceMap cat dv fa
    -> DolanVarianceMap cat dv fb
    -> PolarMapType cat polarity fa fb
    -> DolanArguments dv wa fa polarity t
    -> m (ShimWit cat (Arguments wb fb) polarity t)
dolanArgumentsToArgumentsM' _ NilListType NilDolanVarianceMap NilDolanVarianceMap conv NilDolanArguments =
    return $ MkShimWit NilArguments conv
dolanArgumentsToArgumentsM' f (ConsListType Refl lc) (ConsDolanVarianceMap dvma) (ConsDolanVarianceMap dvmb) conv (ConsDolanArguments sta dta) = do
    Dict <- return $ covaryKMCategory @cat lc
    Dict <- return $ applyFunctionKindWitness (inKind @_ @fa) sta
    MkShimWit ta conva <- f sta
    Dict <- return $ applyFunctionKindWitness (inKind @_ @fa) ta
    Dict <- return $ applyFunctionKindWitness (inKind @_ @fb) ta
    MkShimWit tfa convfa <-
        dolanArgumentsToArgumentsM'
            f
            lc
            dvma
            dvmb
            (case representative @_ @_ @polarity of
                 PositiveType -> consShimFunc CovarianceType conv conva
                 NegativeType -> consShimFunc CovarianceType conv conva)
            dta
    return $ MkShimWit (ConsArguments ta tfa) convfa

dolanArgumentsToArgumentsM ::
       forall m (cat :: forall kc. kc -> kc -> Type) wa wb dv polarity f t.
       (Monad m, DolanVarianceInCategory cat, Is PolarityType polarity)
    => (forall t'. wa polarity t' -> m (ShimWit cat wb polarity t'))
    -> CovaryType dv
    -> CovaryMap cat f
    -> DolanArguments dv wa f polarity t
    -> m (ShimWit cat (Arguments wb f) polarity t)
dolanArgumentsToArgumentsM f lc covary args =
    case covaryMapInKind covary of
        Dict -> let
            dvm = covaryToDolanVarianceMap lc covary
            conv =
                case covaryKMCategory @cat lc of
                    Dict ->
                        case representative @_ @_ @polarity of
                            PositiveType -> cid
                            NegativeType -> cid
            in dolanArgumentsToArgumentsM' f lc dvm dvm conv args

dolanArgumentsToArguments ::
       forall (cat :: forall kc. kc -> kc -> Type) wa wb dv polarity f t.
       (DolanVarianceInCategory cat, Is PolarityType polarity)
    => (forall t'. wa polarity t' -> ShimWit cat wb polarity t')
    -> CovaryType dv
    -> CovaryMap cat f
    -> DolanArguments dv wa f polarity t
    -> ShimWit cat (Arguments wb f) polarity t
dolanArgumentsToArguments f lc covary args =
    runIdentity $ dolanArgumentsToArgumentsM (\wt -> Identity $ f wt) lc covary args

argumentsToDolanArgumentsM' ::
       forall m (cat :: forall kc. kc -> kc -> Type) wa wb dv polarity fa fb t.
       (Monad m, DolanVarianceInCategory cat, Is PolarityType polarity)
    => (forall t'. wa t' -> m (PShimWit cat wb polarity t'))
    -> CovaryType dv
    -> CovaryMap cat fa
    -> CovaryMap cat fb
    -> PolarMapType cat polarity fa fb
    -> Arguments wa fa t
    -> m (PShimWit cat (DolanArguments dv wb fb) polarity t)
argumentsToDolanArgumentsM' _ NilListType NilCovaryMap NilCovaryMap conv NilArguments =
    return $ MkShimWit NilDolanArguments conv
argumentsToDolanArgumentsM' f (ConsListType Refl ct) (ConsCovaryMap mma) (ConsCovaryMap mmb) conv (ConsArguments arg args) = do
    Dict <- return $ covaryKMCategory @cat ct
    Dict <- return $ applyFunctionKindWitness (inKind @_ @fa) arg
    MkShimWit ta conva <- f arg
    Dict <- return $ applyFunctionKindWitness (inKind @_ @fa) ta
    Dict <- return $ applyFunctionKindWitness (inKind @_ @fb) ta
    MkShimWit tfa convfa <-
        argumentsToDolanArgumentsM'
            f
            ct
            mma
            mmb
            (case representative @_ @_ @polarity of
                 PositiveType -> consShimFunc CovarianceType conv conva
                 NegativeType -> consShimFunc CovarianceType conv conva)
            args
    return $ MkShimWit (ConsDolanArguments ta tfa) convfa

argumentsToDolanArgumentsM ::
       forall m (cat :: forall kc. kc -> kc -> Type) wa wb dv polarity f t.
       (Monad m, DolanVarianceInCategory cat, Is PolarityType polarity)
    => (forall t'. wa t' -> m (PShimWit cat wb polarity t'))
    -> CovaryType dv
    -> CovaryMap cat f
    -> Arguments wa f t
    -> m (PShimWit cat (DolanArguments dv wb f) polarity t)
argumentsToDolanArgumentsM f ct cm args =
    argumentsToDolanArgumentsM'
        f
        ct
        cm
        cm
        (case covaryKMCategory @cat ct of
             Dict ->
                 case covaryMapInKind @cat cm of
                     Dict ->
                         case representative @_ @_ @polarity of
                             PositiveType -> cid
                             NegativeType -> cid)
        args

argumentsToDolanArguments ::
       forall (cat :: forall kc. kc -> kc -> Type) wa wb dv polarity f t.
       (DolanVarianceInCategory cat, Is PolarityType polarity)
    => (forall t'. wa t' -> PShimWit cat wb polarity t')
    -> CovaryType dv
    -> CovaryMap cat f
    -> Arguments wa f t
    -> PShimWit cat (DolanArguments dv wb f) polarity t
argumentsToDolanArguments f ct cm args = runIdentity $ argumentsToDolanArgumentsM (\wt -> Identity $ f wt) ct cm args
