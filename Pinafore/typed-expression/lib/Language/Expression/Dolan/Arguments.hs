module Language.Expression.Dolan.Arguments
    ( CCRArguments(..)
    , ccrArgumentsEndo
    , ccrArgumentsType
    , mapCCRArgumentsM
    , mapCCRArguments
    , nilAnyCCRArguments
    , consAnyCCRArguments
    , DolanArguments
    , DolanArgumentsShimWit
    , nilDolanArgumentsShimWit
    , consDolanArgumentsShimWit
    , forDolanArguments
    , saturateArgsConstraint
    , mapDolanArgumentsF
    , mapDolanArgumentsM
    , mapDolanArguments
    , mapInvertDolanArgumentsM
    , mergeDolanArgumentsM
    , mergeDolanArguments
    , Arguments(..)
    , mapDolanArgumentsFM
    , ccrArgumentsToShimArgumentsM
    , ccrArgumentsToArgumentsM
    , dolanArgumentsToArgumentsM
    , dolanArgumentsToArguments
    , argumentsToDolanArgumentsM
    , argumentsToDolanArguments
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Argument
import Language.Expression.Dolan.Covariance
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Variance
import Shapes

type CCRArguments :: CCRArgumentKind -> forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type -> Type
data CCRArguments w dv gt t where
    NilCCRArguments :: forall (w :: CCRArgumentKind) (t :: Type). CCRArguments w '[] t t
    ConsCCRArguments
        :: forall (w :: CCRArgumentKind) (sv :: CCRVariance) (dv :: DolanVariance) (gt :: CCRVarianceKind sv -> DolanVarianceKind dv) (a :: CCRVarianceKind sv) (t :: Type).
           w sv a
        -> CCRArguments w dv (gt a) t
        -> CCRArguments w (sv ': dv) gt t

ccrArgumentsType ::
       forall (w :: CCRArgumentKind) dv gt t. IsCCRArg w
    => CCRArguments w dv gt t
    -> DolanVarianceType dv
ccrArgumentsType NilCCRArguments = NilListType
ccrArgumentsType (ConsCCRArguments arg args) = ConsListType (ccrArgumentType arg) (ccrArgumentsType args)

ccrArgumentsEndo ::
       forall (w :: CCRArgumentKind) (dv :: DolanVariance) (f :: DolanVarianceKind dv) (t :: Type).
       CCRArguments w dv f t
    -> KindFunction f f
    -> t
    -> t
ccrArgumentsEndo NilCCRArguments ff = ff
ccrArgumentsEndo (ConsCCRArguments _ args) (MkNestedMorphism ff) = ccrArgumentsEndo args ff

mapCCRArgumentsFM ::
       forall m (pshim :: PolyShimKind) (wa :: CCRArgumentKind) (wb :: CCRArgumentKind) dv polarity (gta :: DolanVarianceKind dv) (gtb :: DolanVarianceKind dv) t.
       (Monad m, DolanVarianceCategory pshim, Is PolarityType polarity, IsCCRArg wa, IsCCRArg wb)
    => (forall sv' (t' :: CCRVarianceKind sv'). wa sv' t' -> m (CCRArgumentShimWit (pshim Type) wb polarity sv' t'))
    -> DolanVarianceMap dv gta
    -> DolanVarianceMap dv gtb
    -> CCRArguments wa dv gta t
    -> PolarMap (pshim (DolanVarianceKind dv)) polarity gta gtb
    -> m (PolarShimWit (pshim Type) (CCRArguments wb dv gtb) polarity t)
mapCCRArgumentsFM _ NilDolanVarianceMap NilDolanVarianceMap NilCCRArguments conv =
    return $ MkShimWit NilCCRArguments conv
mapCCRArgumentsFM f (ConsDolanVarianceMap ccrva dvma) (ConsDolanVarianceMap ccrvb dvmb) (ConsCCRArguments sta dta) conv = do
    MkShimWit stb svf <- f sta
    let svt = ccrArgumentType sta
    MkShimWit dtb convb <-
        mapCCRArgumentsFM @m @pshim @wa @wb @_ @polarity f dvma dvmb dta (polarMapTypeApply svt ccrva ccrvb conv svf)
    return $ MkShimWit (ConsCCRArguments stb dtb) convb

mapCCRArgumentsF ::
       forall (pshim :: PolyShimKind) (wa :: CCRArgumentKind) (wb :: CCRArgumentKind) dv polarity (gta :: DolanVarianceKind dv) (gtb :: DolanVarianceKind dv) t.
       (DolanVarianceCategory pshim, Is PolarityType polarity, IsCCRArg wa, IsCCRArg wb)
    => (forall sv' (t' :: CCRVarianceKind sv'). wa sv' t' -> CCRArgumentShimWit (pshim Type) wb polarity sv' t')
    -> DolanVarianceMap dv gta
    -> DolanVarianceMap dv gtb
    -> CCRArguments wa dv gta t
    -> PolarMap (pshim (DolanVarianceKind dv)) polarity gta gtb
    -> PolarShimWit (pshim Type) (CCRArguments wb dv gtb) polarity t
mapCCRArgumentsF f dvma dvmb args conv = runIdentity $ mapCCRArgumentsFM (\wa -> Identity $ f wa) dvma dvmb args conv

mapCCRArgumentsM ::
       forall m (pshim :: PolyShimKind) (wa :: CCRArgumentKind) (wb :: CCRArgumentKind) dv polarity (gt :: DolanVarianceKind dv) t.
       (Monad m, DolanVarianceCategory pshim, Is PolarityType polarity, IsCCRArg wa, IsCCRArg wb)
    => (forall sv' (t' :: CCRVarianceKind sv'). wa sv' t' -> m (CCRArgumentShimWit (pshim Type) wb polarity sv' t'))
    -> DolanVarianceMap dv gt
    -> CCRArguments wa dv gt t
    -> m (PolarShimWit (pshim Type) (CCRArguments wb dv gt) polarity t)
mapCCRArgumentsM f dvm args = let
    dvt = ccrArgumentsType args
    in case dolanVarianceCategory @pshim dvt of
           Dict -> mapCCRArgumentsFM f dvm dvm args id

mapCCRArguments ::
       forall (pshim :: PolyShimKind) (wa :: CCRArgumentKind) (wb :: CCRArgumentKind) dv polarity (gt :: DolanVarianceKind dv) t.
       (DolanVarianceCategory pshim, Is PolarityType polarity, IsCCRArg wa, IsCCRArg wb)
    => (forall sv' (t' :: CCRVarianceKind sv'). wa sv' t' -> CCRArgumentShimWit (pshim Type) wb polarity sv' t')
    -> DolanVarianceMap dv gt
    -> CCRArguments wa dv gt t
    -> PolarShimWit (pshim Type) (CCRArguments wb dv gt) polarity t
mapCCRArguments f dvm args = runIdentity $ mapCCRArgumentsM (\wa -> Identity $ f wa) dvm args

nilAnyCCRArguments :: forall (w :: CCRArgumentKind) gt. AnyW (CCRArguments w '[] gt)
nilAnyCCRArguments = MkAnyW NilCCRArguments

consAnyCCRArguments ::
       forall (w :: CCRArgumentKind) sv (t :: CCRVarianceKind sv) (dv :: DolanVariance).
       w sv t
    -> (forall (gt :: DolanVarianceKind dv). AnyW (CCRArguments w dv gt))
    -> (forall (gt :: CCRVarianceKind sv -> DolanVarianceKind dv). AnyW (CCRArguments w (sv ': dv) gt))
consAnyCCRArguments p atp = let
    atp' :: forall (gt :: CCRVarianceKind sv -> DolanVarianceKind dv). AnyW (CCRArguments w (sv ': dv) gt)
    atp' =
        case atp @(gt t) of
            MkAnyW pp -> MkAnyW $ ConsCCRArguments p pp
    in atp'

type DolanArguments :: forall (dv :: DolanVariance) ->
                               (Polarity -> Type -> Type) -> DolanVarianceKind dv -> Polarity -> Type -> Type
type DolanArguments dv ft gt polarity = CCRArguments (CCRPolarArgument ft polarity) dv gt

instance forall (w :: CCRArgumentKind) dv gt. IsCCRArg w => TestEquality (CCRArguments w dv gt) where
    testEquality NilCCRArguments NilCCRArguments = Just Refl
    testEquality (ConsCCRArguments ta tta) (ConsCCRArguments tb ttb) = do
        Refl <- ccrArgumentTestEquality @w ta tb
        Refl <- testEquality tta ttb
        return Refl

type DolanArgumentsShimWit :: PolyShimKind -> forall (dv :: DolanVariance) ->
                                                      (Polarity -> Type -> Type) -> DolanVarianceKind dv -> Polarity -> Type -> Type
type DolanArgumentsShimWit pshim dv ft gt polarity
     = PolarShimWit (pshim Type) (DolanArguments dv ft gt polarity) polarity

nilDolanArgumentsShimWit ::
       forall (pshim :: PolyShimKind) ft t polarity. (Is PolarityType polarity, Category (pshim Type))
    => DolanArgumentsShimWit pshim '[] ft t polarity t
nilDolanArgumentsShimWit = mkShimWit NilCCRArguments

consDolanArgumentsShimWit ::
       forall (pshim :: PolyShimKind) ft sv dv gt a t polarity.
       ( Is PolarityType polarity
       , DolanVarianceCategory pshim
       , TestEquality (ft 'Positive)
       , TestEquality (ft 'Negative)
       , HasDolanVariance (sv ': dv) gt
       )
    => CCRPolarArgumentShimWit (pshim Type) ft polarity sv a
    -> DolanArgumentsShimWit pshim dv ft (gt a) polarity t
    -> DolanArgumentsShimWit pshim (sv ': dv) ft gt polarity t
consDolanArgumentsShimWit (MkShimWit arg conv1) (MkShimWit args convr) = let
    svt = ccrArgumentType arg
    dvt = ccrArgumentsType args
    plain ::
           forall sv' x.
           CCRPolarArgument ft polarity sv' x
        -> CCRArgumentShimWit (pshim Type) (CCRPolarArgument ft polarity) polarity sv' x
    plain arg' =
        case ccrVarianceCategory @((PolarMap (pshim Type) polarity)) $ ccrArgumentType arg' of
            Dict -> mkShimWit arg'
    in case dolanVarianceMap @(sv ': dv) @gt of
           ConsDolanVarianceMap ccrv dvm ->
               case dolanVarianceCategory @pshim dvt of
                   Dict ->
                       case mapCCRArgumentsF plain dvm dvm args (polarMapTypeApply svt ccrv ccrv id conv1) of
                           MkShimWit args' conv' -> MkShimWit (ConsCCRArguments arg args') $ conv' . convr

forDolanArguments ::
       forall polarity dv ft gt t r. (Is PolarityType polarity, Monoid r)
    => (forall polarity' t'. Is PolarityType polarity' => ft polarity' t' -> r)
    -> DolanArguments dv ft gt polarity t
    -> r
forDolanArguments _call NilCCRArguments = mempty
forDolanArguments call (ConsCCRArguments arg args) =
    forCCRPolarArgument @polarity call arg <> forDolanArguments call args

saturateArgsConstraint ::
       forall (w :: Type -> Type) dv ft gt polarity (t :: Type).
       SaturatedWitness w gt
    -> DolanArguments dv ft gt polarity t
    -> w t
saturateArgsConstraint (NilSaturatedWitness wt) NilCCRArguments = wt
saturateArgsConstraint (ConsSaturatedWitness sw) (ConsCCRArguments _ args) = saturateArgsConstraint sw args

mapDolanArgumentsFM ::
       forall m (pshim :: PolyShimKind) fta ftb dv polarity (gt :: DolanVarianceKind dv) (gt' :: DolanVarianceKind dv) t.
       ( Monad m
       , DolanVarianceCategory pshim
       , Is PolarityType polarity
       , TestEquality (fta 'Positive)
       , TestEquality (fta 'Negative)
       , TestEquality (ftb 'Positive)
       , TestEquality (ftb 'Negative)
       )
    => (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> m (PShimWit (pshim Type) ftb polarity' t'))
    -> DolanVarianceMap dv gt
    -> DolanVarianceMap dv gt'
    -> DolanArguments dv fta gt polarity t
    -> PolarMap (pshim (DolanVarianceKind dv)) polarity gt gt'
    -> m (DolanArgumentsShimWit pshim dv ftb gt' polarity t)
mapDolanArgumentsFM f = mapCCRArgumentsFM $ mapCCRPolarArgumentShimWit f

mapDolanArgumentsF ::
       forall (pshim :: PolyShimKind) ft dv polarity (gt :: DolanVarianceKind dv) (gt' :: DolanVarianceKind dv) t.
       (DolanVarianceCategory pshim, Is PolarityType polarity, TestEquality (ft 'Positive), TestEquality (ft 'Negative))
    => DolanVarianceMap dv gt
    -> DolanVarianceMap dv gt'
    -> DolanArguments dv ft gt polarity t
    -> PolarMap (pshim (DolanVarianceKind dv)) polarity gt gt'
    -> DolanArgumentsShimWit pshim dv ft gt' polarity t
mapDolanArgumentsF dvma dvmb args f = runIdentity $ mapDolanArgumentsFM (pure . mkPolarShimWit) dvma dvmb args f

mapDolanArgumentsM ::
       forall m (pshim :: PolyShimKind) fta ftb dv polarity gt t.
       ( Monad m
       , DolanVarianceCategory pshim
       , Is PolarityType polarity
       , TestEquality (fta 'Positive)
       , TestEquality (fta 'Negative)
       , TestEquality (ftb 'Positive)
       , TestEquality (ftb 'Negative)
       )
    => (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> m (PShimWit (pshim Type) ftb polarity' t'))
    -> DolanVarianceMap dv gt
    -> DolanArguments dv fta gt polarity t
    -> m (DolanArgumentsShimWit pshim dv ftb gt polarity t)
mapDolanArgumentsM f dvm args = let
    dvt = ccrArgumentsType args
    in case dolanVarianceCategory @pshim dvt of
           Dict -> mapDolanArgumentsFM f dvm dvm args id

mapDolanArguments ::
       forall (pshim :: PolyShimKind) fta ftb dv polarity gt t.
       ( DolanVarianceCategory pshim
       , Is PolarityType polarity
       , TestEquality (fta 'Positive)
       , TestEquality (fta 'Negative)
       , TestEquality (ftb 'Positive)
       , TestEquality (ftb 'Negative)
       )
    => (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> PShimWit (pshim Type) ftb polarity' t')
    -> DolanVarianceMap dv gt
    -> DolanArguments dv fta gt polarity t
    -> DolanArgumentsShimWit pshim dv ftb gt polarity t
mapDolanArguments f dvm args = runIdentity $ mapDolanArgumentsM (\t -> return $ f t) dvm args

mapInvertArgsTypeF ::
       forall m (pshim :: PolyShimKind) fta ftb dv polarity (gt :: DolanVarianceKind dv) (gt' :: DolanVarianceKind dv) t.
       ( Monad m
       , DolanVarianceCategory pshim
       , Is PolarityType polarity
       , TestEquality (fta 'Positive)
       , TestEquality (fta 'Negative)
       , TestEquality (ftb 'Positive)
       , TestEquality (ftb 'Negative)
       )
    => (forall polarity' t'.
            Is PolarityType polarity' => fta polarity' t' -> m (PShimWit (pshim Type) ftb (InvertPolarity polarity') t'))
    -> DolanVarianceMap dv gt
    -> DolanVarianceMap dv gt'
    -> DolanArguments dv fta gt polarity t
    -> PolarMap (pshim (DolanVarianceKind dv)) (InvertPolarity polarity) gt gt'
    -> m (DolanArgumentsShimWit pshim dv ftb gt' (InvertPolarity polarity) t)
mapInvertArgsTypeF f dvma dvmb args conv =
    invertPolarity @polarity $ mapCCRArgumentsFM (mapInvertCCRPolarArgumentShimWit f) dvma dvmb args conv

mapInvertDolanArgumentsM ::
       forall m (pshim :: PolyShimKind) fta ftb dv polarity gt t.
       ( Monad m
       , DolanVarianceCategory pshim
       , Is PolarityType polarity
       , TestEquality (fta 'Positive)
       , TestEquality (fta 'Negative)
       , TestEquality (ftb 'Positive)
       , TestEquality (ftb 'Negative)
       )
    => (forall polarity' t'.
            Is PolarityType polarity' => fta polarity' t' -> m (PShimWit (pshim Type) ftb (InvertPolarity polarity') t'))
    -> DolanVarianceMap dv gt
    -> DolanArguments dv fta gt polarity t
    -> m (DolanArgumentsShimWit pshim dv ftb gt (InvertPolarity polarity) t)
mapInvertDolanArgumentsM f dvm args = let
    dvt = ccrArgumentsType args
    in case dolanVarianceCategory @pshim dvt of
           Dict -> invertPolarity @polarity $ mapInvertArgsTypeF f dvm dvm args id

mergeArgsTypeF ::
       forall (m :: Type -> Type) (pshim :: PolyShimKind) (fta :: Polarity -> Type -> Type) (ftb :: Polarity -> Type -> Type) (ftab :: Polarity -> Type -> Type) (dv :: DolanVariance) (polarity :: Polarity) (gta :: DolanVarianceKind dv) (gtb :: DolanVarianceKind dv) (gtab :: DolanVarianceKind dv) (ta :: Type) (tb :: Type).
       ( Monad m
       , DolanVarianceCategory pshim
       , JoinMeetCategory (pshim Type)
       , Is PolarityType polarity
       , TestEquality (fta 'Positive)
       , TestEquality (fta 'Negative)
       , TestEquality (ftb 'Positive)
       , TestEquality (ftb 'Negative)
       )
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
    -> m (DolanArgumentsShimWit pshim dv ftab gtab polarity (JoinMeetType polarity ta tb))
mergeArgsTypeF _ NilListType NilDolanVarianceMap NilDolanVarianceMap NilDolanVarianceMap NilCCRArguments NilCCRArguments conva convb =
    return $ MkShimWit NilCCRArguments $ polarF conva convb
mergeArgsTypeF f (ConsListType svt dvt) (ConsDolanVarianceMap ccrva dvma) (ConsDolanVarianceMap ccrvb dvmb) (ConsDolanVarianceMap ccrvab dvmab) (ConsCCRArguments (sta :: _ ta0) dta) (ConsCCRArguments (stb :: _ tb0) dtb) conva convb = do
    Dict <- return $ ccrVarianceCoercibleKind svt
    MkShimWit stab svf <- mergeCCRPolarArgumentShimWit @m @(pshim Type) @fta @ftb @ftab @_ @polarity f sta stb
    Dict <- return $ dolanVarianceCategory @pshim dvt
    Dict <- return $ ccrVarianceCategory @(PolarMap (pshim Type) polarity) svt
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
            (polarMapTypeApply svt ccrva ccrvab conva $ svf . ccrPolar1 @(pshim Type) @polarity @_ @ta0 @tb0 svt)
            (polarMapTypeApply svt ccrvb ccrvab convb $ svf . ccrPolar2 @(pshim Type) @polarity @_ @ta0 @tb0 svt)
    return $ MkShimWit (ConsCCRArguments stab dtab) convab

mergeDolanArgumentsM ::
       forall m (pshim :: PolyShimKind) fta ftb ftab dv polarity gt ta tb.
       ( Monad m
       , DolanVarianceCategory pshim
       , JoinMeetCategory (pshim Type)
       , Is PolarityType polarity
       , TestEquality (fta 'Positive)
       , TestEquality (fta 'Negative)
       , TestEquality (ftb 'Positive)
       , TestEquality (ftb 'Negative)
       )
    => (forall polarity' ta' tb'.
            Is PolarityType polarity' =>
                    fta polarity' ta' -> ftb polarity' tb' -> m (PShimWit (pshim Type) ftab polarity' (JoinMeetType polarity' ta' tb')))
    -> DolanVarianceMap dv gt
    -> DolanArguments dv fta gt polarity ta
    -> DolanArguments dv ftb gt polarity tb
    -> m (DolanArgumentsShimWit pshim dv ftab gt polarity (JoinMeetType polarity ta tb))
mergeDolanArgumentsM f dvm argsa argsb = let
    dvt = ccrArgumentsType argsa
    in case dolanVarianceCategory @pshim dvt of
           Dict -> mergeArgsTypeF @m @pshim f dvt dvm dvm dvm argsa argsb id id

mergeDolanArguments ::
       forall (pshim :: PolyShimKind) fta ftb ftab dv polarity gt ta tb.
       ( DolanVarianceCategory pshim
       , JoinMeetCategory (pshim Type)
       , Is PolarityType polarity
       , TestEquality (fta 'Positive)
       , TestEquality (fta 'Negative)
       , TestEquality (ftb 'Positive)
       , TestEquality (ftb 'Negative)
       )
    => (forall polarity' ta' tb'.
            Is PolarityType polarity' =>
                    fta polarity' ta' -> ftb polarity' tb' -> PShimWit (pshim Type) ftab polarity' (JoinMeetType polarity' ta' tb'))
    -> DolanVarianceMap dv gt
    -> DolanArguments dv fta gt polarity ta
    -> DolanArguments dv ftb gt polarity tb
    -> DolanArgumentsShimWit pshim dv ftab gt polarity (JoinMeetType polarity ta tb)
mergeDolanArguments f dvm argsa argsb = runIdentity $ mergeDolanArgumentsM (\a b -> Identity $ f a b) dvm argsa argsb

ccrArgumentsToShimArgumentsM' ::
       forall m (pshim :: PolyShimKind) (wa :: Polarity -> CCRArgumentKind) wb dv polarity (gta :: DolanVarianceKind dv) (gtb :: DolanVarianceKind dv) t.
       (Monad m, DolanVarianceCategory pshim, Is PolarityType polarity)
    => (forall (t' :: Type). wa polarity CoCCRVariance t' -> m (PolarShimWit (pshim Type) wb polarity t'))
    -> CovaryType dv
    -> DolanVarianceMap dv gta
    -> DolanVarianceMap dv gtb
    -> PolarMap (pshim (DolanVarianceKind dv)) polarity gta gtb
    -> CCRArguments (wa polarity) dv gta t
    -> m (PolarShimWit (pshim Type) (Arguments wb gtb) polarity t)
ccrArgumentsToShimArgumentsM' _ NilListType NilDolanVarianceMap NilDolanVarianceMap conv NilCCRArguments =
    return $ MkShimWit NilArguments conv
ccrArgumentsToShimArgumentsM' f (ConsListType Refl lc) (ConsDolanVarianceMap ccrva dvma) (ConsDolanVarianceMap ccrvb dvmb) conv (ConsCCRArguments sta dta) = do
    Dict <- return $ covaryKMCategory @pshim lc
    MkShimWit ta conva <- f sta
    MkShimWit tfa convfa <-
        ccrArgumentsToShimArgumentsM' f lc dvma dvmb (polarMapTypeApply CoCCRVarianceType ccrva ccrvb conv conva) dta
    return $ MkShimWit (ConsArguments ta tfa) convfa

ccrArgumentsToShimArgumentsM ::
       forall m (pshim :: PolyShimKind) (wa :: Polarity -> CCRArgumentKind) wb dv polarity f t.
       (Monad m, DolanVarianceCategory pshim, Is PolarityType polarity)
    => (forall t'. wa polarity CoCCRVariance t' -> m (PolarShimWit (pshim Type) wb polarity t'))
    -> CovaryType dv
    -> CovaryMap f
    -> CCRArguments (wa polarity) dv f t
    -> m (PolarShimWit (pshim Type) (Arguments wb f) polarity t)
ccrArgumentsToShimArgumentsM f lc covary args = let
    dvm = covaryToDolanVarianceMap lc covary
    conv =
        case covaryKMCategory @pshim lc of
            Dict -> id
    in ccrArgumentsToShimArgumentsM' f lc dvm dvm conv args

ccrArgumentsToArgumentsM ::
       forall m (wa :: CCRArgumentKind) wb dv f t. Applicative m
    => (forall t'. wa CoCCRVariance t' -> m (wb t'))
    -> CovaryType dv
    -> CCRArguments wa dv f t
    -> m (Arguments wb f t)
ccrArgumentsToArgumentsM _ NilListType NilCCRArguments = pure NilArguments
ccrArgumentsToArgumentsM f (ConsListType Refl lc) (ConsCCRArguments sta dta) =
    ConsArguments <$> f sta <*> ccrArgumentsToArgumentsM f lc dta

dolanArgumentsToArgumentsM ::
       forall m (pshim :: PolyShimKind) wa wb dv polarity f t.
       (Monad m, DolanVarianceCategory pshim, Is PolarityType polarity)
    => (forall t'. wa polarity t' -> m (PolarShimWit (pshim Type) wb polarity t'))
    -> CovaryType dv
    -> CovaryMap f
    -> DolanArguments dv wa f polarity t
    -> m (PolarShimWit (pshim Type) (Arguments wb f) polarity t)
dolanArgumentsToArgumentsM f = ccrArgumentsToShimArgumentsM $ \(CoCCRPolarArgument arg) -> f arg

dolanArgumentsToArguments ::
       forall (pshim :: PolyShimKind) wa wb dv polarity f t. (DolanVarianceCategory pshim, Is PolarityType polarity)
    => (forall t'. wa polarity t' -> PolarShimWit (pshim Type) wb polarity t')
    -> CovaryType dv
    -> CovaryMap f
    -> DolanArguments dv wa f polarity t
    -> PolarShimWit (pshim Type) (Arguments wb f) polarity t
dolanArgumentsToArguments f lc covary args =
    runIdentity $ dolanArgumentsToArgumentsM (\wt -> Identity $ f wt) lc covary args

argumentsToDolanArgumentsM' ::
       forall m (pshim :: PolyShimKind) wa wb dv polarity (fa :: DolanVarianceKind dv) (fb :: DolanVarianceKind dv) t.
       (Monad m, DolanVarianceCategory pshim, Is PolarityType polarity)
    => (forall t'. wa t' -> m (PShimWit (pshim Type) wb polarity t'))
    -> CovaryType dv
    -> CovaryMap fa
    -> CovaryMap fb
    -> PolarMap (pshim (DolanVarianceKind dv)) polarity fa fb
    -> Arguments wa fa t
    -> m (DolanArgumentsShimWit pshim dv wb fb polarity t)
argumentsToDolanArgumentsM' _ NilListType NilCovaryMap NilCovaryMap conv NilArguments =
    return $ MkShimWit NilCCRArguments conv
argumentsToDolanArgumentsM' f (ConsListType Refl ct) (ConsCovaryMap ccrva mma) (ConsCovaryMap ccrvb mmb) conv (ConsArguments arg args) = do
    Dict <- return $ covaryKMCategory @pshim ct
    MkShimWit ta conva <- f arg
    MkShimWit tfa convfa <-
        argumentsToDolanArgumentsM' f ct mma mmb (polarMapTypeApply CoCCRVarianceType ccrva ccrvb conv conva) args
    return $ MkShimWit (ConsCCRArguments (CoCCRPolarArgument ta) tfa) convfa

argumentsToDolanArgumentsM ::
       forall m (pshim :: PolyShimKind) wa wb dv polarity f t.
       (Monad m, DolanVarianceCategory pshim, Is PolarityType polarity)
    => (forall t'. wa t' -> m (PShimWit (pshim Type) wb polarity t'))
    -> CovaryType dv
    -> CovaryMap f
    -> Arguments wa f t
    -> m (DolanArgumentsShimWit pshim dv wb f polarity t)
argumentsToDolanArgumentsM f ct cm args =
    argumentsToDolanArgumentsM'
        f
        ct
        cm
        cm
        (case covaryKMCategory @pshim ct of
             Dict -> id)
        args

argumentsToDolanArguments ::
       forall (pshim :: PolyShimKind) wa wb dv polarity f t. (DolanVarianceCategory pshim, Is PolarityType polarity)
    => (forall t'. wa t' -> PShimWit (pshim Type) wb polarity t')
    -> CovaryType dv
    -> CovaryMap f
    -> Arguments wa f t
    -> DolanArgumentsShimWit pshim dv wb f polarity t
argumentsToDolanArguments f ct cm args = runIdentity $ argumentsToDolanArgumentsM (\wt -> Identity $ f wt) ct cm args
