module Language.Expression.Dolan.Arguments
    ( SingleArgument
    , DolanArguments(..)
    , bijectTypeArguments
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

import Language.Expression.Dolan.JoinMeet
import Language.Expression.Dolan.PType
import Language.Expression.Dolan.Range
import Language.Expression.Dolan.Variance
import Language.Expression.Polarity
import Language.Expression.TypeF
import Shapes

data Arguments (w :: Type -> Type) (f :: k) (t :: Type) where
    NilArguments :: Arguments w t t
    ConsArguments :: w a -> Arguments w (f a) t -> Arguments w f t

instance TestEquality w => TestEquality (Arguments w f) where
    testEquality NilArguments NilArguments = Just Refl
    testEquality (ConsArguments w1 args1) (ConsArguments w2 args2) = do
        Refl <- testEquality w1 w2
        Refl <- testEquality args1 args2
        Just Refl

---
type family SingleArgument (sv :: SingleVariance) (ft :: Polarity -> Type -> Type) (polarity :: Polarity) :: SingleVarianceKind sv -> Type where
    SingleArgument 'Covariance ft polarity = ft polarity
    SingleArgument 'Contravariance ft polarity = ft (InvertPolarity polarity)
    SingleArgument 'Rangevariance ft polarity = RangeType ft polarity

data DolanArguments (dv :: DolanVariance) (ft :: Polarity -> Type -> Type) (gt :: DolanVarianceKind dv) (polarity :: Polarity) (ta :: Type) where
    NilDolanArguments :: DolanArguments '[] ft t polarity t
    ConsDolanArguments
        :: InVarianceKind sv a
        => SingleArgument sv ft polarity a
        -> DolanArguments dv ft (gt a) polarity ta
        -> DolanArguments (sv ': dv) ft gt polarity ta

bijectTypeArguments ::
       forall ft dv polarity ta t t' r.
       KindBijection t t'
    -> DolanArguments dv ft t polarity ta
    -> (forall ta'. DolanArguments dv ft t' polarity ta' -> Bijection ta ta' -> r)
    -> r
bijectTypeArguments bij NilDolanArguments cont = cont NilDolanArguments bij
bijectTypeArguments bij (ConsDolanArguments arg args) cont =
    bijectTypeArguments (unNestedMorphism @_ @_ @_ @t @t' bij) args $ \args' bijargs ->
        cont (ConsDolanArguments arg args') bijargs

type family PolarSingleVarianceFunc (cat :: Type -> Type -> Type) (polarity :: Polarity) (sv :: SingleVariance) (a :: SingleVarianceKind sv) (b :: SingleVarianceKind sv) :: Type where
    PolarSingleVarianceFunc cat 'Positive sv a b = SingleVarianceFunc cat sv a b
    PolarSingleVarianceFunc cat 'Negative sv a b = SingleVarianceFunc cat sv b a

data ArgTypeF (cat :: Type -> Type -> Type) (sv :: SingleVariance) (ft :: Polarity -> Type -> Type) (polarity :: Polarity) (t :: SingleVarianceKind sv) :: Type where
    MkArgTypeF
        :: InVarianceKind sv t'
        => SingleArgument sv ft polarity t'
        -> PolarSingleVarianceFunc cat polarity sv t t'
        -> ArgTypeF cat sv ft polarity t

mapArgTypeF ::
       forall m (cat :: Type -> Type -> Type) (fta :: Polarity -> Type -> Type) (ftb :: Polarity -> Type -> Type) sv polarity t.
       (Monad m, Category cat, Is PolarityType polarity)
    => SingleVarianceType sv
    -> (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> m (GenPTypeF cat ftb polarity' t'))
    -> SingleArgument sv fta polarity t
    -> m (ArgTypeF cat sv ftb polarity t)
mapArgTypeF CovarianceType f arg = do
    tf <- f arg
    return $
        unTypeF tf $
        case representative @_ @_ @polarity of
            PositiveType -> MkArgTypeF
            NegativeType -> MkArgTypeF
mapArgTypeF ContravarianceType f arg =
    invertPolarity @polarity $ do
        MkTypeF arg' conv <- f arg
        return $
            MkArgTypeF arg' $
            case representative @_ @_ @polarity of
                PositiveType -> MkCatDual conv
                NegativeType -> MkCatDual conv
mapArgTypeF RangevarianceType f (MkRangeType tp tq) =
    invertPolarity @polarity $ do
        MkTypeF tp' convp <- f tp
        MkTypeF tq' convq <- f tq
        return $
            MkArgTypeF (MkRangeType tp' tq') $
            case representative @_ @_ @polarity of
                PositiveType -> MkWithRange convp convq
                NegativeType -> MkWithRange convp convq

mapArgsTypeF ::
       forall m cat fta ftb dv polarity (gt :: DolanVarianceKind dv) (gt' :: DolanVarianceKind dv) t.
       (Monad m, Category cat, Is PolarityType polarity)
    => (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> m (GenPTypeF cat ftb polarity' t'))
    -> DolanVarianceType dv
    -> DolanVarianceMap cat dv gt
    -> DolanArguments dv fta gt polarity t
    -> PolarMapType (KindMorphism cat) polarity gt gt'
    -> m (GenPTypeF cat (DolanArguments dv ftb gt') polarity t)
mapArgsTypeF _ NilListType NilDolanVarianceMap NilDolanArguments conv = return $ MkTypeF NilDolanArguments conv
mapArgsTypeF f (ConsListType svt dvt) (ConsDolanVarianceMap svm dvm) (ConsDolanArguments sta dta) conv = do
    MkArgTypeF sta' svf <- mapArgTypeF @m @cat @fta @ftb @_ @polarity svt f sta
    case dolanVarianceKMCategory @cat dvt of
        Dict ->
            case representative @_ @_ @polarity of
                PositiveType ->
                    case conv of
                        MkNestedMorphism mconv -> do
                            MkTypeF dta' conv' <-
                                mapArgsTypeF @m @cat @fta @ftb @_ @polarity f dvt dvm dta (mconv . svm svf)
                            return $ MkTypeF (ConsDolanArguments sta' dta') conv'
                NegativeType ->
                    case conv of
                        MkNestedMorphism mconv -> do
                            MkTypeF dta' conv' <-
                                mapArgsTypeF @m @cat @fta @ftb @_ @polarity f dvt dvm dta (svm svf . mconv)
                            return $ MkTypeF (ConsDolanArguments sta' dta') conv'

mapDolanArgumentsM ::
       forall m cat fta ftb dv polarity gt t. (Monad m, Category cat, Is PolarityType polarity)
    => (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> m (GenPTypeF cat ftb polarity' t'))
    -> DolanVarianceType dv
    -> DolanVarianceMap cat dv gt
    -> DolanArguments dv fta gt polarity t
    -> m (GenPTypeF cat (DolanArguments dv ftb gt) polarity t)
mapDolanArgumentsM f dvt dvm args =
    case dolanVarianceKMCategory @cat dvt of
        Dict ->
            case representative @_ @_ @polarity of
                PositiveType -> mapArgsTypeF f dvt dvm args id
                NegativeType -> mapArgsTypeF f dvt dvm args id

mapDolanArguments ::
       forall cat fta ftb dv polarity gt t. (Category cat, Is PolarityType polarity)
    => (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> GenPTypeF cat ftb polarity' t')
    -> DolanVarianceType dv
    -> DolanVarianceMap cat dv gt
    -> DolanArguments dv fta gt polarity t
    -> GenPTypeF cat (DolanArguments dv ftb gt) polarity t
mapDolanArguments f dvt kv args = runIdentity $ mapDolanArgumentsM (\t -> return $ f t) dvt kv args

mapInvertArgTypeF ::
       forall m cat fta ftb sv polarity t. (Monad m, Is PolarityType polarity)
    => SingleVarianceType sv
    -> (forall polarity' t'.
            Is PolarityType polarity' => fta polarity' t' -> m (GenPTypeF cat ftb (InvertPolarity polarity') t'))
    -> SingleArgument sv fta polarity t
    -> m (ArgTypeF cat sv ftb (InvertPolarity polarity) t)
mapInvertArgTypeF CovarianceType f arg = do
    MkTypeF arg' conv <- f arg
    return $
        MkArgTypeF arg' $
        case representative @_ @_ @polarity of
            PositiveType -> conv
            NegativeType -> conv
mapInvertArgTypeF ContravarianceType f arg =
    invertPolarity @polarity $ do
        MkTypeF arg' conv <- f arg
        return $
            MkArgTypeF arg' $
            case representative @_ @_ @polarity of
                PositiveType -> MkCatDual conv
                NegativeType -> MkCatDual conv
mapInvertArgTypeF RangevarianceType f (MkRangeType tp tq) =
    invertPolarity @polarity $ do
        MkTypeF tp' convp <- f tp
        MkTypeF tq' convq <- f tq
        return $
            MkArgTypeF (MkRangeType tp' tq') $
            case representative @_ @_ @polarity of
                PositiveType -> MkWithRange convp convq
                NegativeType -> MkWithRange convp convq

mapInvertArgsTypeF ::
       forall m cat fta ftb dv polarity gt gt' t. (Monad m, Category cat, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' => fta polarity' t' -> m (GenPTypeF cat ftb (InvertPolarity polarity') t'))
    -> DolanVarianceType dv
    -> DolanVarianceMap cat dv gt
    -> DolanArguments dv fta gt polarity t
    -> PolarMapType (KindMorphism cat) (InvertPolarity polarity) gt gt'
    -> m (GenPTypeF cat (DolanArguments dv ftb gt') (InvertPolarity polarity) t)
mapInvertArgsTypeF _ NilListType NilDolanVarianceMap NilDolanArguments conv = return $ MkTypeF NilDolanArguments conv
mapInvertArgsTypeF f (ConsListType svt dvt) (ConsDolanVarianceMap svm dvm) (ConsDolanArguments sta dta) conv = do
    MkArgTypeF sta' svf <- mapInvertArgTypeF @m @cat @fta @ftb @_ @polarity svt f sta
    case dolanVarianceKMCategory @cat dvt of
        Dict ->
            case representative @_ @_ @polarity of
                PositiveType ->
                    case conv of
                        MkNestedMorphism mconv -> do
                            MkTypeF dta' conv' <-
                                mapInvertArgsTypeF @m @cat @fta @ftb @_ @polarity f dvt dvm dta (svm svf . mconv)
                            return $ MkTypeF (ConsDolanArguments sta' dta') conv'
                NegativeType ->
                    case conv of
                        MkNestedMorphism mconv -> do
                            MkTypeF dta' conv' <-
                                mapInvertArgsTypeF @m @cat @fta @ftb @_ @polarity f dvt dvm dta (mconv . svm svf)
                            return $ MkTypeF (ConsDolanArguments sta' dta') conv'

mapInvertDolanArgumentsM ::
       forall m cat fta ftb dv polarity gt t. (Monad m, Category cat, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' => fta polarity' t' -> m (GenPTypeF cat ftb (InvertPolarity polarity') t'))
    -> DolanVarianceType dv
    -> DolanVarianceMap cat dv gt
    -> DolanArguments dv fta gt polarity t
    -> m (GenPTypeF cat (DolanArguments dv ftb gt) (InvertPolarity polarity) t)
mapInvertDolanArgumentsM f dvt dvm args =
    case dolanVarianceKMCategory @cat dvt of
        Dict ->
            case representative @_ @_ @polarity of
                PositiveType -> mapInvertArgsTypeF f dvt dvm args id
                NegativeType -> mapInvertArgsTypeF f dvt dvm args id

type family PositiveSVJoinMeetType (sv :: SingleVariance) (a :: SingleVarianceKind sv) (b :: SingleVarianceKind sv) = (r :: SingleVarianceKind sv) | r -> sv a b where
    PositiveSVJoinMeetType 'Covariance a b = JoinType a b
    PositiveSVJoinMeetType 'Contravariance a b = MeetType a b
    PositiveSVJoinMeetType 'Rangevariance ('( pa, qa)) ('( pb, qb)) = '( MeetType pa pb, JoinType qa qb)

type family NegativeSVJoinMeetType (sv :: SingleVariance) (a :: SingleVarianceKind sv) (b :: SingleVarianceKind sv) = (r :: SingleVarianceKind sv) | r -> sv a b where
    NegativeSVJoinMeetType 'Covariance a b = MeetType a b
    NegativeSVJoinMeetType 'Contravariance a b = JoinType a b
    NegativeSVJoinMeetType 'Rangevariance ('( pa, qa)) ('( pb, qb)) = '( JoinType pa pb, MeetType qa qb)

type family SVJoinMeetType (sv :: SingleVariance) (polarity :: Polarity) (a :: SingleVarianceKind sv) (b :: SingleVarianceKind sv) = (r :: SingleVarianceKind sv) where
    SVJoinMeetType sv 'Positive a b = PositiveSVJoinMeetType sv a b
    SVJoinMeetType sv 'Negative a b = NegativeSVJoinMeetType sv a b

mergeArgTypeF ::
       forall fta ftb ftab sv polarity ta tb. Is PolarityType polarity
    => SingleVarianceType sv
    -> (forall polarity' ta' tb'.
            Is PolarityType polarity' =>
                    fta polarity' ta' -> ftb polarity' tb' -> PTypeF ftab polarity' (JoinMeetType polarity' ta' tb'))
    -> SingleArgument sv fta polarity ta
    -> SingleArgument sv ftb polarity tb
    -> ArgTypeF (->) sv ftab polarity (SVJoinMeetType sv polarity ta tb)
mergeArgTypeF CovarianceType f arga argb =
    case f arga argb of
        MkTypeF argab conv ->
            case representative @_ @_ @polarity of
                PositiveType -> MkArgTypeF argab conv
                NegativeType -> MkArgTypeF argab conv
mergeArgTypeF ContravarianceType f arga argb =
    invertPolarity @polarity $
    case f arga argb of
        MkTypeF argab conv ->
            case representative @_ @_ @polarity of
                PositiveType -> MkArgTypeF argab $ MkCatDual conv
                NegativeType -> MkArgTypeF argab $ MkCatDual conv
mergeArgTypeF RangevarianceType f (MkRangeType tpa tqa) (MkRangeType tpb tqb) =
    invertPolarity @polarity $
    case f tpa tpb of
        MkTypeF tpab convp ->
            case f tqa tqb of
                MkTypeF tqab convq ->
                    case representative @_ @_ @polarity of
                        PositiveType -> MkArgTypeF (MkRangeType tpab tqab) $ MkWithRange convp convq
                        NegativeType -> MkArgTypeF (MkRangeType tpab tqab) $ MkWithRange convp convq

psvf1 ::
       forall polarity sv a b c. (Is PolarityType polarity, InVarianceKind sv a, InVarianceKind sv b)
    => SingleVarianceType sv
    -> PolarSingleVarianceFunc (->) polarity sv (SVJoinMeetType sv polarity a b) c
    -> PolarSingleVarianceFunc (->) polarity sv a c
psvf1 =
    case representative @_ @_ @polarity of
        PositiveType ->
            \case
                CovarianceType -> \conv -> conv . join1
                ContravarianceType -> \(MkCatDual conv) -> MkCatDual $ meet1 . conv
                RangevarianceType ->
                    \(MkWithRange convp convq) ->
                        case (inKind @_ @a, inKind @_ @b) of
                            (MkPairWitness, MkPairWitness) -> MkWithRange (meet1 . convp) (convq . join1)
        NegativeType ->
            \case
                CovarianceType -> \conv -> meet1 . conv
                ContravarianceType -> \(MkCatDual conv) -> MkCatDual $ conv . join1
                RangevarianceType ->
                    \(MkWithRange convp convq) ->
                        case (inKind @_ @a, inKind @_ @b) of
                            (MkPairWitness, MkPairWitness) -> MkWithRange (convp . join1) (meet1 . convq)

psvf2 ::
       forall polarity sv a b c. (Is PolarityType polarity, InVarianceKind sv a, InVarianceKind sv b)
    => SingleVarianceType sv
    -> PolarSingleVarianceFunc (->) polarity sv (SVJoinMeetType sv polarity a b) c
    -> PolarSingleVarianceFunc (->) polarity sv b c
psvf2 =
    case representative @_ @_ @polarity of
        PositiveType ->
            \case
                CovarianceType -> \conv -> conv . join2
                ContravarianceType -> \(MkCatDual conv) -> MkCatDual $ meet2 . conv
                RangevarianceType ->
                    \(MkWithRange convp convq) ->
                        case (inKind @_ @a, inKind @_ @b) of
                            (MkPairWitness, MkPairWitness) -> MkWithRange (meet2 . convp) (convq . join2)
        NegativeType ->
            \case
                CovarianceType -> \conv -> meet2 . conv
                ContravarianceType -> \(MkCatDual conv) -> MkCatDual $ conv . join2
                RangevarianceType ->
                    \(MkWithRange convp convq) ->
                        case (inKind @_ @a, inKind @_ @b) of
                            (MkPairWitness, MkPairWitness) -> MkWithRange (convp . join2) (meet2 . convq)

mergeArgsTypeF ::
       forall fta ftb ftab dv polarity gta gtb gtab ta tb. Is PolarityType polarity
    => (forall polarity' ta' tb'.
            Is PolarityType polarity' =>
                    fta polarity' ta' -> ftb polarity' tb' -> PTypeF ftab polarity' (JoinMeetType polarity' ta' tb'))
    -> DolanVarianceType dv
    -> DolanVarianceMap (->) dv gta
    -> DolanVarianceMap (->) dv gtb
    -> DolanArguments dv fta gta polarity ta
    -> DolanArguments dv ftb gtb polarity tb
    -> PolarMapType (KindMorphism (->)) polarity gta gtab
    -> PolarMapType (KindMorphism (->)) polarity gtb gtab
    -> PTypeF (DolanArguments dv ftab gtab) polarity (JoinMeetType polarity ta tb)
mergeArgsTypeF _ NilListType NilDolanVarianceMap NilDolanVarianceMap NilDolanArguments NilDolanArguments conva convb =
    MkTypeF NilDolanArguments $
    case representative @_ @_ @polarity of
        PositiveType -> joinf conva convb
        NegativeType -> meetf conva convb
mergeArgsTypeF f (ConsListType svt dvt) (ConsDolanVarianceMap svma dvma) (ConsDolanVarianceMap svmb dvmb) (ConsDolanArguments sta dta) (ConsDolanArguments stb dtb) conva convb =
    case mergeArgTypeF @fta @ftb @ftab @_ @polarity svt f sta stb of
        MkArgTypeF stab svf ->
            case dolanVarianceKMCategory @(->) dvt of
                Dict ->
                    case representative @_ @_ @polarity of
                        PositiveType ->
                            case (conva, convb) of
                                (MkNestedMorphism mconva, MkNestedMorphism mconvb) ->
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
                                             dta
                                             dtb
                                             (mconva . svma (psvf1 @polarity svt svf))
                                             (mconvb . svmb (psvf2 @polarity svt svf)) of
                                        MkTypeF dtab convab -> MkTypeF (ConsDolanArguments stab dtab) convab
                        NegativeType ->
                            case (conva, convb) of
                                (MkNestedMorphism mconva, MkNestedMorphism mconvb) ->
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
                                             dta
                                             dtb
                                             (svma (psvf1 @polarity svt svf) . mconva)
                                             (svmb (psvf2 @polarity svt svf) . mconvb) of
                                        MkTypeF dtab convab -> MkTypeF (ConsDolanArguments stab dtab) convab

mergeDolanArguments ::
       forall fta ftb ftab dv polarity gt ta tb. Is PolarityType polarity
    => (forall polarity' ta' tb'.
            Is PolarityType polarity' =>
                    fta polarity' ta' -> ftb polarity' tb' -> PTypeF ftab polarity' (JoinMeetType polarity' ta' tb'))
    -> DolanVarianceType dv
    -> DolanVarianceMap (->) dv gt
    -> DolanArguments dv fta gt polarity ta
    -> DolanArguments dv ftb gt polarity tb
    -> PTypeF (DolanArguments dv ftab gt) polarity (JoinMeetType polarity ta tb)
mergeDolanArguments f dvt dv argsa argsb =
    case dolanVarianceKMCategory @(->) dvt of
        Dict ->
            case representative @_ @_ @polarity of
                PositiveType -> mergeArgsTypeF f dvt dv dv argsa argsb id id
                NegativeType -> mergeArgsTypeF f dvt dv dv argsa argsb id id

dolanArgumentsToArgumentsM' ::
       forall m cat wa wb dv polarity fa fb t. (Monad m, Category cat, Is PolarityType polarity)
    => (forall t'. wa polarity t' -> m (GenTypeF cat wb polarity t'))
    -> CovaryType dv
    -> DolanVarianceMap cat dv fa
    -> PolarMapType (KindMorphism cat) polarity fa fb
    -> DolanArguments dv wa fa polarity t
    -> m (GenTypeF cat (Arguments wb fb) polarity t)
dolanArgumentsToArgumentsM' _ NilListType NilDolanVarianceMap conv NilDolanArguments =
    return $ MkTypeF NilArguments conv
dolanArgumentsToArgumentsM' f (ConsListType Refl lc) (ConsDolanVarianceMap svm dvm) conv (ConsDolanArguments sta dta) =
    case covaryKMCategory @cat lc of
        Dict -> do
            MkTypeF ta conva <- f sta
            MkTypeF tfa convfa <-
                dolanArgumentsToArgumentsM'
                    f
                    lc
                    dvm
                    (case representative @_ @_ @polarity of
                         PositiveType ->
                             case conv of
                                 (MkNestedMorphism ff) -> ff . svm conva
                         NegativeType ->
                             case conv of
                                 (MkNestedMorphism ff) -> svm conva . ff)
                    dta
            return $ MkTypeF (ConsArguments ta tfa) convfa

dolanArgumentsToArgumentsM ::
       forall m cat wa wb dv polarity f t. (Monad m, Category cat, Is PolarityType polarity)
    => (forall t'. wa polarity t' -> m (GenTypeF cat wb polarity t'))
    -> CovaryType dv
    -> CovaryMap cat f
    -> DolanArguments dv wa f polarity t
    -> m (GenTypeF cat (Arguments wb f) polarity t)
dolanArgumentsToArgumentsM f lc covary args =
    dolanArgumentsToArgumentsM'
        f
        lc
        (covaryToDolanVarianceMap lc covary)
        (case covaryKMCategory @cat lc of
             Dict ->
                 case representative @_ @_ @polarity of
                     PositiveType -> id
                     NegativeType -> id)
        args

dolanArgumentsToArguments ::
       forall cat wa wb dv polarity f t. (Category cat, Is PolarityType polarity)
    => (forall t'. wa polarity t' -> GenTypeF cat wb polarity t')
    -> CovaryType dv
    -> CovaryMap cat f
    -> DolanArguments dv wa f polarity t
    -> GenTypeF cat (Arguments wb f) polarity t
dolanArgumentsToArguments f lc covary args =
    runIdentity $ dolanArgumentsToArgumentsM (\wt -> Identity $ f wt) lc covary args

argumentsToDolanArgumentsM' ::
       forall m cat wa wb dv polarity fa fb t. (Monad m, Category cat, Is PolarityType polarity)
    => (forall t'. wa t' -> m (GenPTypeF cat wb polarity t'))
    -> CovaryType dv
    -> CovaryMap cat fa
    -> PolarMapType (KindMorphism cat) polarity fa fb
    -> Arguments wa fa t
    -> m (GenPTypeF cat (DolanArguments dv wb fb) polarity t)
argumentsToDolanArgumentsM' _ NilListType NilCovaryMap conv NilArguments = return $ MkTypeF NilDolanArguments conv
argumentsToDolanArgumentsM' f (ConsListType Refl ct) (ConsCovaryMap m mm) conv (ConsArguments arg args) = do
    Dict <- return $ covaryKMCategory @cat ct
    MkTypeF ta conva <- f arg
    MkTypeF tfa convfa <-
        argumentsToDolanArgumentsM'
            f
            ct
            mm
            (case representative @_ @_ @polarity of
                 PositiveType ->
                     case conv of
                         (MkNestedMorphism ff) -> ff . m conva
                 NegativeType ->
                     case conv of
                         (MkNestedMorphism ff) -> m conva . ff)
            args
    return $ MkTypeF (ConsDolanArguments ta tfa) convfa

argumentsToDolanArgumentsM ::
       forall m cat wa wb dv polarity f t. (Monad m, Category cat, Is PolarityType polarity)
    => (forall t'. wa t' -> m (GenPTypeF cat wb polarity t'))
    -> CovaryType dv
    -> CovaryMap cat f
    -> Arguments wa f t
    -> m (GenPTypeF cat (DolanArguments dv wb f) polarity t)
argumentsToDolanArgumentsM f ct cm args =
    argumentsToDolanArgumentsM'
        f
        ct
        cm
        (case covaryKMCategory @cat ct of
             Dict ->
                 case representative @_ @_ @polarity of
                     PositiveType -> id
                     NegativeType -> id)
        args

argumentsToDolanArguments ::
       forall cat wa wb dv polarity f t. (Category cat, Is PolarityType polarity)
    => (forall t'. wa t' -> GenPTypeF cat wb polarity t')
    -> CovaryType dv
    -> CovaryMap cat f
    -> Arguments wa f t
    -> GenPTypeF cat (DolanArguments dv wb f) polarity t
argumentsToDolanArguments f ct cm args = runIdentity $ argumentsToDolanArgumentsM (\wt -> Identity $ f wt) ct cm args
