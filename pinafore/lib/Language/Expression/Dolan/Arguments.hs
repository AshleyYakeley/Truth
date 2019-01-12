module Language.Expression.Dolan.Arguments
    ( SingleArgument
    , DolanArguments(..)
    , bijectTypeArguments
    , mapDolanArgumentsM
    , mapDolanArguments
    , mapInvertDolanArgumentsM
    , mergeDolanArguments
    ) where

import Language.Expression.Dolan.JoinMeet
import Language.Expression.Dolan.PType
import Language.Expression.Dolan.Range
import Language.Expression.Dolan.Variance
import Language.Expression.Polarity
import Language.Expression.TypeF
import Shapes

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
       forall ft dk polarity ta t t' r.
       KindBijection (DolanVarianceKind dk) t t'
    -> DolanArguments dk ft t polarity ta
    -> (forall ta'. DolanArguments dk ft t' polarity ta' -> Bijection ta ta' -> r)
    -> r
bijectTypeArguments bij NilDolanArguments cont = cont NilDolanArguments bij
bijectTypeArguments bij (ConsDolanArguments arg args) cont =
    bijectTypeArguments (unNestedMorphism @_ @_ @_ @t @t' bij) args $ \args' bijargs ->
        cont (ConsDolanArguments arg args') bijargs

type family PolarSingleVarianceFunc (polarity :: Polarity) (sv :: SingleVariance) (a :: SingleVarianceKind sv) (b :: SingleVarianceKind sv) :: Type where
    PolarSingleVarianceFunc 'Positive sv a b = SingleVarianceFunc sv a b
    PolarSingleVarianceFunc 'Negative sv a b = SingleVarianceFunc sv b a

data ArgTypeF (sv :: SingleVariance) (ft :: Polarity -> Type -> Type) (polarity :: Polarity) (t :: SingleVarianceKind sv) :: Type where
    MkArgTypeF
        :: InVarianceKind sv t'
        => SingleArgument sv ft polarity t'
        -> PolarSingleVarianceFunc polarity sv t t'
        -> ArgTypeF sv ft polarity t

mapArgTypeF ::
       forall m ft sv polarity t. (Monad m, Is PolarityType polarity)
    => SingleVarianceType sv
    -> (forall polarity' t'. Is PolarityType polarity' => ft polarity' t' -> m (PTypeF ft polarity' t'))
    -> SingleArgument sv ft polarity t
    -> m (ArgTypeF sv ft polarity t)
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
       forall m ft dv polarity gt gt' t. (Monad m, Is PolarityType polarity)
    => (forall polarity' t'. Is PolarityType polarity' => ft polarity' t' -> m (PTypeF ft polarity' t'))
    -> DolanVarianceType dv
    -> DolanKindVary dv gt
    -> DolanArguments dv ft gt polarity t
    -> ConvertType polarity gt gt'
    -> m (PTypeF (DolanArguments dv ft gt') polarity t)
mapArgsTypeF _ NilListType NilDolanKindVary NilDolanArguments conv = return $ MkTypeF NilDolanArguments conv
mapArgsTypeF f (ConsListType svt dvt) (ConsDolanKindVary svm dvm) (ConsDolanArguments sta dta) conv = do
    MkArgTypeF sta' svf <- mapArgTypeF @m @ft @_ @polarity svt f sta
    case dolanVarianceKMCategory @(->) dvt of
        Dict ->
            case representative @_ @_ @polarity of
                PositiveType ->
                    case conv of
                        MkNestedMorphism mconv -> do
                            MkTypeF dta' conv' <- mapArgsTypeF @m @ft @_ @polarity f dvt dvm dta (mconv . svm svf)
                            return $ MkTypeF (ConsDolanArguments sta' dta') conv'
                NegativeType ->
                    case conv of
                        MkNestedMorphism mconv -> do
                            MkTypeF dta' conv' <- mapArgsTypeF @m @ft @_ @polarity f dvt dvm dta (svm svf . mconv)
                            return $ MkTypeF (ConsDolanArguments sta' dta') conv'

mapDolanArgumentsM ::
       forall m ft dv polarity gt t. (Monad m, Is PolarityType polarity)
    => (forall polarity' t'. Is PolarityType polarity' => ft polarity' t' -> m (PTypeF ft polarity' t'))
    -> DolanVarianceType dv
    -> DolanKindVary dv gt
    -> DolanArguments dv ft gt polarity t
    -> m (PTypeF (DolanArguments dv ft gt) polarity t)
mapDolanArgumentsM f dvt dvm args =
    case dolanVarianceKMCategory @(->) dvt of
        Dict ->
            case representative @_ @_ @polarity of
                PositiveType -> mapArgsTypeF f dvt dvm args id
                NegativeType -> mapArgsTypeF f dvt dvm args id

mapDolanArguments ::
       forall ft dv polarity gt t. Is PolarityType polarity
    => (forall polarity' t'. Is PolarityType polarity' => ft polarity' t' -> PTypeF ft polarity' t')
    -> DolanVarianceType dv
    -> DolanKindVary dv gt
    -> DolanArguments dv ft gt polarity t
    -> PTypeF (DolanArguments dv ft gt) polarity t
mapDolanArguments f dvt kv args = runIdentity $ mapDolanArgumentsM (\t -> return $ f t) dvt kv args

mapInvertArgTypeF ::
       forall m ft sv polarity t. (Monad m, Is PolarityType polarity)
    => SingleVarianceType sv
    -> (forall polarity' t'. Is PolarityType polarity' => ft polarity' t' -> m (PTypeF ft (InvertPolarity polarity') t'))
    -> SingleArgument sv ft polarity t
    -> m (ArgTypeF sv ft (InvertPolarity polarity) t)
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
       forall m ft dv polarity gt gt' t. (Monad m, Is PolarityType polarity)
    => (forall polarity' t'. Is PolarityType polarity' => ft polarity' t' -> m (PTypeF ft (InvertPolarity polarity') t'))
    -> DolanVarianceType dv
    -> DolanKindVary dv gt
    -> DolanArguments dv ft gt polarity t
    -> ConvertType (InvertPolarity polarity) gt gt'
    -> m (PTypeF (DolanArguments dv ft gt') (InvertPolarity polarity) t)
mapInvertArgsTypeF _ NilListType NilDolanKindVary NilDolanArguments conv = return $ MkTypeF NilDolanArguments conv
mapInvertArgsTypeF f (ConsListType svt dvt) (ConsDolanKindVary svm dvm) (ConsDolanArguments sta dta) conv = do
    MkArgTypeF sta' svf <- mapInvertArgTypeF @m @ft @_ @polarity svt f sta
    case dolanVarianceKMCategory @(->) dvt of
        Dict ->
            case representative @_ @_ @polarity of
                PositiveType ->
                    case conv of
                        MkNestedMorphism mconv -> do
                            MkTypeF dta' conv' <- mapInvertArgsTypeF @m @ft @_ @polarity f dvt dvm dta (svm svf . mconv)
                            return $ MkTypeF (ConsDolanArguments sta' dta') conv'
                NegativeType ->
                    case conv of
                        MkNestedMorphism mconv -> do
                            MkTypeF dta' conv' <- mapInvertArgsTypeF @m @ft @_ @polarity f dvt dvm dta (mconv . svm svf)
                            return $ MkTypeF (ConsDolanArguments sta' dta') conv'

mapInvertDolanArgumentsM ::
       forall m ft dv polarity gt t. (Monad m, Is PolarityType polarity)
    => (forall polarity' t'. Is PolarityType polarity' => ft polarity' t' -> m (PTypeF ft (InvertPolarity polarity') t'))
    -> DolanVarianceType dv
    -> DolanKindVary dv gt
    -> DolanArguments dv ft gt polarity t
    -> m (PTypeF (DolanArguments dv ft gt) (InvertPolarity polarity) t)
mapInvertDolanArgumentsM f dvt dvm args =
    case dolanVarianceKMCategory @(->) dvt of
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
       forall ft sv polarity ta tb. Is PolarityType polarity
    => SingleVarianceType sv
    -> (forall polarity' ta' tb'.
            Is PolarityType polarity' =>
                    ft polarity' ta' -> ft polarity' tb' -> PTypeF ft polarity' (JoinMeetType polarity' ta' tb'))
    -> SingleArgument sv ft polarity ta
    -> SingleArgument sv ft polarity tb
    -> ArgTypeF sv ft polarity (SVJoinMeetType sv polarity ta tb)
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
    -> PolarSingleVarianceFunc polarity sv (SVJoinMeetType sv polarity a b) c
    -> PolarSingleVarianceFunc polarity sv a c
psvf1 =
    case representative @_ @_ @polarity of
        PositiveType ->
            \case
                CovarianceType -> \conv -> conv . join1
                ContravarianceType -> \(MkCatDual conv) -> MkCatDual $ meet1 . conv
                RangevarianceType ->
                    \(MkWithRange convp convq) ->
                        case (inKind @_ @a, inKind @_ @b) of
                            (MkPairWitness _ _, MkPairWitness _ _) -> MkWithRange (meet1 . convp) (convq . join1)
        NegativeType ->
            \case
                CovarianceType -> \conv -> meet1 . conv
                ContravarianceType -> \(MkCatDual conv) -> MkCatDual $ conv . join1
                RangevarianceType ->
                    \(MkWithRange convp convq) ->
                        case (inKind @_ @a, inKind @_ @b) of
                            (MkPairWitness _ _, MkPairWitness _ _) -> MkWithRange (convp . join1) (meet1 . convq)

psvf2 ::
       forall polarity sv a b c. (Is PolarityType polarity, InVarianceKind sv a, InVarianceKind sv b)
    => SingleVarianceType sv
    -> PolarSingleVarianceFunc polarity sv (SVJoinMeetType sv polarity a b) c
    -> PolarSingleVarianceFunc polarity sv b c
psvf2 =
    case representative @_ @_ @polarity of
        PositiveType ->
            \case
                CovarianceType -> \conv -> conv . join2
                ContravarianceType -> \(MkCatDual conv) -> MkCatDual $ meet2 . conv
                RangevarianceType ->
                    \(MkWithRange convp convq) ->
                        case (inKind @_ @a, inKind @_ @b) of
                            (MkPairWitness _ _, MkPairWitness _ _) -> MkWithRange (meet2 . convp) (convq . join2)
        NegativeType ->
            \case
                CovarianceType -> \conv -> meet2 . conv
                ContravarianceType -> \(MkCatDual conv) -> MkCatDual $ conv . join2
                RangevarianceType ->
                    \(MkWithRange convp convq) ->
                        case (inKind @_ @a, inKind @_ @b) of
                            (MkPairWitness _ _, MkPairWitness _ _) -> MkWithRange (convp . join2) (meet2 . convq)

mergeArgsTypeF ::
       forall ft dv polarity gta gtb gtab ta tb. Is PolarityType polarity
    => (forall polarity' ta' tb'.
            Is PolarityType polarity' =>
                    ft polarity' ta' -> ft polarity' tb' -> PTypeF ft polarity' (JoinMeetType polarity' ta' tb'))
    -> DolanVarianceType dv
    -> DolanKindVary dv gta
    -> DolanKindVary dv gtb
    -> DolanArguments dv ft gta polarity ta
    -> DolanArguments dv ft gtb polarity tb
    -> ConvertType polarity gta gtab
    -> ConvertType polarity gtb gtab
    -> PTypeF (DolanArguments dv ft gtab) polarity (JoinMeetType polarity ta tb)
mergeArgsTypeF _ NilListType NilDolanKindVary NilDolanKindVary NilDolanArguments NilDolanArguments conva convb =
    MkTypeF NilDolanArguments $
    case representative @_ @_ @polarity of
        PositiveType -> joinf conva convb
        NegativeType -> meetf conva convb
mergeArgsTypeF f (ConsListType svt dvt) (ConsDolanKindVary svma dvma) (ConsDolanKindVary svmb dvmb) (ConsDolanArguments sta dta) (ConsDolanArguments stb dtb) conva convb =
    case mergeArgTypeF @ft @_ @polarity svt f sta stb of
        MkArgTypeF stab svf ->
            case dolanVarianceKMCategory @(->) dvt of
                Dict ->
                    case representative @_ @_ @polarity of
                        PositiveType ->
                            case (conva, convb) of
                                (MkNestedMorphism mconva, MkNestedMorphism mconvb) ->
                                    case mergeArgsTypeF
                                             @ft
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
                                             @ft
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
       forall ft dv polarity gt ta tb. Is PolarityType polarity
    => (forall polarity' ta' tb'.
            Is PolarityType polarity' =>
                    ft polarity' ta' -> ft polarity' tb' -> PTypeF ft polarity' (JoinMeetType polarity' ta' tb'))
    -> DolanVarianceType dv
    -> DolanKindVary dv gt
    -> DolanArguments dv ft gt polarity ta
    -> DolanArguments dv ft gt polarity tb
    -> PTypeF (DolanArguments dv ft gt) polarity (JoinMeetType polarity ta tb)
mergeDolanArguments f dvt dv argsa argsb =
    case dolanVarianceKMCategory @(->) dvt of
        Dict ->
            case representative @_ @_ @polarity of
                PositiveType -> mergeArgsTypeF f dvt dv dv argsa argsb id id
                NegativeType -> mergeArgsTypeF f dvt dv dv argsa argsb id id
