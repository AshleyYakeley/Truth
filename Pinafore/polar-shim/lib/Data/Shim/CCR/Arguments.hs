module Data.Shim.CCR.Arguments
    ( CCRArguments (..)
    , ccrArgumentsEndo
    , ccrArgumentsType
    , ccrArgumentsToList
    , matchCCRArguments
    , mapSameCCRArguments
    , mapCCRArgumentsFM
    , mapPolarCCRArgumentsM
    , mapPolarCCRArguments
    , nilAnyCCRArguments
    , consAnyCCRArguments
    , CCRPolarArguments
    , CCRPolarArgumentsShimWit
    , nilCCRPolarArgumentsShimWit
    , consCCRPolarArgumentsShimWit
    , forDolanArguments
    , mapDolanArgumentsF
    , mapDolanArgumentsM
    , mapDolanArguments
    , mapInvertDolanArgumentsM
    , mergeDolanArgumentsM
    , mergeDolanArguments
    , mapDolanArgumentsFM
    , ccrArgumentsToShimArgumentsM
    , ccrArgumentsToArgumentsM
    , dolanArgumentsToArgumentsM
    , dolanArgumentsToArguments
    , argumentsToDolanArgumentsM
    , argumentsToDolanArguments
    )
where

import Shapes

import Data.Shim.CCR.Apply
import Data.Shim.CCR.Argument
import Data.Shim.CCR.Covariance
import Data.Shim.CCR.PolarVariance
import Data.Shim.CCR.Variance
import Data.Shim.CCR.Variances
import Data.Shim.Mono
import Data.Shim.Polar
import Data.Shim.Poly

type CCRArguments :: CCRArgumentKind -> forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Type -> Type
data CCRArguments w dv gt t where
    NilCCRArguments :: forall (w :: CCRArgumentKind) (t :: Type). CCRArguments w '[] t t
    ConsCCRArguments ::
        forall (w :: CCRArgumentKind) (sv :: CCRVariance) (dv :: CCRVariances) (gt :: CCRVarianceKind sv -> CCRVariancesKind dv) (a :: CCRVarianceKind sv) (t :: Type).
        w sv a ->
        CCRArguments w dv (gt a) t ->
        CCRArguments w (sv ': dv) gt t

instance
    forall (w :: CCRArgumentKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv) (t :: Type).
    (IsCCRArg w, AllConstraint Show (w CoCCRVariance), AllConstraint Show (w ContraCCRVariance), AllConstraint Show (w 'RangeCCRVariance)) =>
    Show (CCRArguments w dv gt t)
    where
    show = let
        showArg :: forall (sv :: CCRVariance) (a :: CCRVarianceKind sv). w sv a -> String
        showArg arg = case ccrArgumentType arg of
            CoCCRVarianceType -> allShow arg
            ContraCCRVarianceType -> allShow arg
            RangeCCRVarianceType -> allShow arg

        showArgs :: forall (dv' :: CCRVariances) (gt' :: CCRVariancesKind dv'). CCRArguments w dv' gt' t -> [String]
        showArgs NilCCRArguments = []
        showArgs (ConsCCRArguments arg args) = showArg arg : showArgs args
        in \args -> "{" <> intercalate "," (showArgs args) <> "}"

instance
    forall (w :: CCRArgumentKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
    (IsCCRArg w, AllConstraint Show (w CoCCRVariance), AllConstraint Show (w ContraCCRVariance), AllConstraint Show (w 'RangeCCRVariance)) =>
    AllConstraint Show (CCRArguments w dv gt)
    where
    allConstraint = Dict

ccrArgumentsType ::
    forall (w :: CCRArgumentKind) dv gt t.
    IsCCRArg w =>
    CCRArguments w dv gt t ->
    CCRVariancesType dv
ccrArgumentsType NilCCRArguments = NilListType
ccrArgumentsType (ConsCCRArguments arg args) = ConsListType (ccrArgumentType arg) (ccrArgumentsType args)

ccrArgumentsEndo ::
    forall (w :: CCRArgumentKind) (dv :: CCRVariances) (f :: CCRVariancesKind dv) (t :: Type).
    CCRArguments w dv f t ->
    KindFunction f f ->
    t ->
    t
ccrArgumentsEndo NilCCRArguments ff = ff
ccrArgumentsEndo (ConsCCRArguments _ args) (MkNestedMorphism ff) = ccrArgumentsEndo args ff

ccrArgumentsToList ::
    forall (w :: CCRArgumentKind) (dv :: CCRVariances) (f :: CCRVariancesKind dv) (t :: Type) r.
    (forall (sv :: CCRVariance) (a :: CCRVarianceKind sv). w sv a -> r) -> CCRArguments w dv f t -> [r]
ccrArgumentsToList _ NilCCRArguments = []
ccrArgumentsToList f (ConsCCRArguments arg args) = f arg : ccrArgumentsToList f args

matchCCRArguments ::
    forall (wa :: CCRArgumentKind) (wb :: CCRArgumentKind) (dv :: CCRVariances) (gta :: CCRVariancesKind dv) (gtb :: CCRVariancesKind dv) (t :: Type).
    CCRArguments wa dv gta t -> CCRArguments wb dv gtb t -> gta :~: gtb
matchCCRArguments NilCCRArguments NilCCRArguments = Refl
matchCCRArguments (ConsCCRArguments _ argsa) (ConsCCRArguments _ argsb) =
    case matchCCRArguments argsa argsb of
        Refl -> Refl

mapSameCCRArguments ::
    forall (wa :: CCRArgumentKind) (wb :: CCRArgumentKind) dv (gt :: CCRVariancesKind dv) t.
    (forall sv' (t' :: CCRVarianceKind sv'). wa sv' t' -> wb sv' t') ->
    CCRArguments wa dv gt t ->
    CCRArguments wb dv gt t
mapSameCCRArguments _ NilCCRArguments = NilCCRArguments
mapSameCCRArguments f (ConsCCRArguments st dt) = ConsCCRArguments (f st) $ mapSameCCRArguments f dt

mapCCRArgumentsFM ::
    forall m (pshim :: PolyShimKind) (wa :: CCRArgumentKind) (wb :: CCRArgumentKind) dv (gta :: CCRVariancesKind dv) (gtb :: CCRVariancesKind dv) t.
    (Monad m, CCRVariancesShim pshim, IsCCRArg wa, IsCCRArg wb) =>
    (forall sv' (t' :: CCRVarianceKind sv'). wa sv' t' -> m (CCRShimWit (pshim Type) wb sv' t')) ->
    CCRVariancesMap dv gta ->
    CCRVariancesMap dv gtb ->
    CCRArguments wa dv gta t ->
    pshim (CCRVariancesKind dv) gta gtb ->
    m (ShimWit (pshim Type) (CCRArguments wb dv gtb) t)
mapCCRArgumentsFM _ NilCCRVariancesMap NilCCRVariancesMap NilCCRArguments conv = return $ MkShimWit NilCCRArguments conv
mapCCRArgumentsFM f (ConsCCRVariancesMap ccrva dvma) (ConsCCRVariancesMap ccrvb dvmb) (ConsCCRArguments sta dta) conv = do
    MkShimWit stb svf <- f sta
    let svt = ccrArgumentType sta
    MkShimWit dtb convb <- mapCCRArgumentsFM @m @pshim @wa @wb f dvma dvmb dta (applyPolyShim svt ccrva ccrvb conv svf)
    return $ MkShimWit (ConsCCRArguments stb dtb) convb

mapPolarCCRArgumentsFM ::
    forall m (pshim :: PolyShimKind) (wa :: CCRArgumentKind) (wb :: CCRArgumentKind) dv polarity (gta :: CCRVariancesKind dv) (gtb :: CCRVariancesKind dv) t.
    (Monad m, CCRVariancesShim pshim, Is PolarityType polarity, IsCCRArg wa, IsCCRArg wb) =>
    (forall sv' (t' :: CCRVarianceKind sv'). wa sv' t' -> m (CCRArgumentShimWit (pshim Type) wb polarity sv' t')) ->
    CCRVariancesMap dv gta ->
    CCRVariancesMap dv gtb ->
    CCRArguments wa dv gta t ->
    PolarShim (pshim (CCRVariancesKind dv)) polarity gta gtb ->
    m (PolarShimWit (pshim Type) (CCRArguments wb dv gtb) polarity t)
mapPolarCCRArgumentsFM _ NilCCRVariancesMap NilCCRVariancesMap NilCCRArguments conv =
    return $ MkShimWit NilCCRArguments conv
mapPolarCCRArgumentsFM f (ConsCCRVariancesMap ccrva dvma) (ConsCCRVariancesMap ccrvb dvmb) (ConsCCRArguments sta dta) conv = do
    MkShimWit stb svf <- f sta
    let svt = ccrArgumentType sta
    MkShimWit dtb convb <-
        mapPolarCCRArgumentsFM
            @m
            @pshim
            @wa
            @wb
            @_
            @polarity
            f
            dvma
            dvmb
            dta
            (polarShimTypeApply svt ccrva ccrvb conv svf)
    return $ MkShimWit (ConsCCRArguments stb dtb) convb

mapPolarCCRArgumentsF ::
    forall (pshim :: PolyShimKind) (wa :: CCRArgumentKind) (wb :: CCRArgumentKind) dv polarity (gta :: CCRVariancesKind dv) (gtb :: CCRVariancesKind dv) t.
    (CCRVariancesShim pshim, Is PolarityType polarity, IsCCRArg wa, IsCCRArg wb) =>
    (forall sv' (t' :: CCRVarianceKind sv'). wa sv' t' -> CCRArgumentShimWit (pshim Type) wb polarity sv' t') ->
    CCRVariancesMap dv gta ->
    CCRVariancesMap dv gtb ->
    CCRArguments wa dv gta t ->
    PolarShim (pshim (CCRVariancesKind dv)) polarity gta gtb ->
    PolarShimWit (pshim Type) (CCRArguments wb dv gtb) polarity t
mapPolarCCRArgumentsF f dvma dvmb args conv =
    runIdentity $ mapPolarCCRArgumentsFM (\wa -> Identity $ f wa) dvma dvmb args conv

mapPolarCCRArgumentsM ::
    forall m (pshim :: PolyShimKind) (wa :: CCRArgumentKind) (wb :: CCRArgumentKind) dv polarity (gt :: CCRVariancesKind dv) t.
    (Monad m, CCRVariancesShim pshim, Is PolarityType polarity, IsCCRArg wa, IsCCRArg wb) =>
    (forall sv' (t' :: CCRVarianceKind sv'). wa sv' t' -> m (CCRArgumentShimWit (pshim Type) wb polarity sv' t')) ->
    CCRVariancesMap dv gt ->
    CCRArguments wa dv gt t ->
    m (PolarShimWit (pshim Type) (CCRArguments wb dv gt) polarity t)
mapPolarCCRArgumentsM f dvm args = let
    dvt = ccrArgumentsType args
    in case ccrVariancesCategory @pshim dvt of
        Dict -> mapPolarCCRArgumentsFM f dvm dvm args id

mapPolarCCRArguments ::
    forall (pshim :: PolyShimKind) (wa :: CCRArgumentKind) (wb :: CCRArgumentKind) dv polarity (gt :: CCRVariancesKind dv) t.
    (CCRVariancesShim pshim, Is PolarityType polarity, IsCCRArg wa, IsCCRArg wb) =>
    (forall sv' (t' :: CCRVarianceKind sv'). wa sv' t' -> CCRArgumentShimWit (pshim Type) wb polarity sv' t') ->
    CCRVariancesMap dv gt ->
    CCRArguments wa dv gt t ->
    PolarShimWit (pshim Type) (CCRArguments wb dv gt) polarity t
mapPolarCCRArguments f dvm args = runIdentity $ mapPolarCCRArgumentsM (\wa -> Identity $ f wa) dvm args

nilAnyCCRArguments :: forall (w :: CCRArgumentKind) gt. Some (CCRArguments w '[] gt)
nilAnyCCRArguments = MkSome NilCCRArguments

consAnyCCRArguments ::
    forall (w :: CCRArgumentKind) sv (t :: CCRVarianceKind sv) (dv :: CCRVariances).
    w sv t ->
    (forall (gt :: CCRVariancesKind dv). Some (CCRArguments w dv gt)) ->
    (forall (gt :: CCRVarianceKind sv -> CCRVariancesKind dv). Some (CCRArguments w (sv ': dv) gt))
consAnyCCRArguments p atp = let
    atp' :: forall (gt :: CCRVarianceKind sv -> CCRVariancesKind dv). Some (CCRArguments w (sv ': dv) gt)
    atp' =
        case atp @(gt t) of
            MkSome pp -> MkSome $ ConsCCRArguments p pp
    in atp'

type CCRPolarArguments ::
    forall (dv :: CCRVariances) -> (Polarity -> Type -> Type) -> CCRVariancesKind dv -> Polarity -> Type -> Type
type CCRPolarArguments dv ft gt polarity = CCRArguments (CCRPolarArgument ft polarity) dv gt

instance forall (w :: CCRArgumentKind) dv gt. IsCCRArg w => TestEquality (CCRArguments w dv gt) where
    testEquality NilCCRArguments NilCCRArguments = Just Refl
    testEquality (ConsCCRArguments ta tta) (ConsCCRArguments tb ttb) = do
        Refl <- ccrArgumentTestEquality @w ta tb
        Refl <- testEquality tta ttb
        return Refl

type CCRPolarArgumentsShimWit ::
    PolyShimKind ->
    forall (dv :: CCRVariances) ->
    (Polarity -> Type -> Type) -> CCRVariancesKind dv -> Polarity -> Type -> Type
type CCRPolarArgumentsShimWit pshim dv ft gt polarity =
    PolarShimWit (pshim Type) (CCRPolarArguments dv ft gt polarity) polarity

nilCCRPolarArgumentsShimWit ::
    forall (pshim :: PolyShimKind) ft t polarity.
    (Is PolarityType polarity, Category (pshim Type)) =>
    CCRPolarArgumentsShimWit pshim '[] ft t polarity t
nilCCRPolarArgumentsShimWit = mkShimWit NilCCRArguments

consCCRPolarArgumentsShimWit ::
    forall (pshim :: PolyShimKind) ft sv dv gt a t polarity.
    (Is PolarityType polarity, CCRVariancesShim pshim, TestEquality (ft 'Positive), TestEquality (ft 'Negative)) =>
    CCRVariancesMap (sv ': dv) gt ->
    CCRPolarArgumentShimWit (pshim Type) ft polarity sv a ->
    CCRPolarArgumentsShimWit pshim dv ft (gt a) polarity t ->
    CCRPolarArgumentsShimWit pshim (sv ': dv) ft gt polarity t
consCCRPolarArgumentsShimWit (ConsCCRVariancesMap ccrv dvm) (MkShimWit arg conv1) (MkShimWit args convr) = let
    svt = ccrArgumentType arg
    dvt = ccrArgumentsType args
    plain ::
        forall sv' x.
        CCRPolarArgument ft polarity sv' x ->
        CCRArgumentShimWit (pshim Type) (CCRPolarArgument ft polarity) polarity sv' x
    plain arg' =
        case ccrVarianceCategory @((PolarShim (pshim Type) polarity)) $ ccrArgumentType arg' of
            Dict -> mkShimWit arg'
    in case ccrVariancesCategory @pshim dvt of
        Dict ->
            case mapPolarCCRArgumentsF plain dvm dvm args (polarShimTypeApply svt ccrv ccrv id conv1) of
                MkShimWit args' conv' -> MkShimWit (ConsCCRArguments arg args') $ conv' . convr

forDolanArguments ::
    forall polarity dv ft gt t r.
    (Is PolarityType polarity, Monoid r) =>
    (forall polarity' t'. Is PolarityType polarity' => ft polarity' t' -> r) ->
    CCRPolarArguments dv ft gt polarity t ->
    r
forDolanArguments _call NilCCRArguments = mempty
forDolanArguments call (ConsCCRArguments arg args) =
    forCCRPolarArgument @polarity call arg <> forDolanArguments call args

mapDolanArgumentsFM ::
    forall m (pshim :: PolyShimKind) fta ftb dv polarity (gt :: CCRVariancesKind dv) (gt' :: CCRVariancesKind dv) t.
    ( Monad m
    , CCRVariancesShim pshim
    , Is PolarityType polarity
    , TestEquality (fta 'Positive)
    , TestEquality (fta 'Negative)
    , TestEquality (ftb 'Positive)
    , TestEquality (ftb 'Negative)
    ) =>
    (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> m (PShimWit (pshim Type) ftb polarity' t')) ->
    CCRVariancesMap dv gt ->
    CCRVariancesMap dv gt' ->
    CCRPolarArguments dv fta gt polarity t ->
    PolarShim (pshim (CCRVariancesKind dv)) polarity gt gt' ->
    m (CCRPolarArgumentsShimWit pshim dv ftb gt' polarity t)
mapDolanArgumentsFM f = mapPolarCCRArgumentsFM $ mapCCRPolarArgumentShimWit f

mapDolanArgumentsF ::
    forall (pshim :: PolyShimKind) ft dv polarity (gt :: CCRVariancesKind dv) (gt' :: CCRVariancesKind dv) t.
    (CCRVariancesShim pshim, Is PolarityType polarity, TestEquality (ft 'Positive), TestEquality (ft 'Negative)) =>
    CCRVariancesMap dv gt ->
    CCRVariancesMap dv gt' ->
    CCRPolarArguments dv ft gt polarity t ->
    PolarShim (pshim (CCRVariancesKind dv)) polarity gt gt' ->
    CCRPolarArgumentsShimWit pshim dv ft gt' polarity t
mapDolanArgumentsF dvma dvmb args f = runIdentity $ mapDolanArgumentsFM (pure . mkPolarShimWit) dvma dvmb args f

mapDolanArgumentsM ::
    forall m (pshim :: PolyShimKind) fta ftb dv polarity gt t.
    ( Monad m
    , CCRVariancesShim pshim
    , Is PolarityType polarity
    , TestEquality (fta 'Positive)
    , TestEquality (fta 'Negative)
    , TestEquality (ftb 'Positive)
    , TestEquality (ftb 'Negative)
    ) =>
    (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> m (PShimWit (pshim Type) ftb polarity' t')) ->
    CCRVariancesMap dv gt ->
    CCRPolarArguments dv fta gt polarity t ->
    m (CCRPolarArgumentsShimWit pshim dv ftb gt polarity t)
mapDolanArgumentsM f dvm args = let
    dvt = ccrArgumentsType args
    in case ccrVariancesCategory @pshim dvt of
        Dict -> mapDolanArgumentsFM f dvm dvm args id

mapDolanArguments ::
    forall (pshim :: PolyShimKind) fta ftb dv polarity gt t.
    ( CCRVariancesShim pshim
    , Is PolarityType polarity
    , TestEquality (fta 'Positive)
    , TestEquality (fta 'Negative)
    , TestEquality (ftb 'Positive)
    , TestEquality (ftb 'Negative)
    ) =>
    (forall polarity' t'. Is PolarityType polarity' => fta polarity' t' -> PShimWit (pshim Type) ftb polarity' t') ->
    CCRVariancesMap dv gt ->
    CCRPolarArguments dv fta gt polarity t ->
    CCRPolarArgumentsShimWit pshim dv ftb gt polarity t
mapDolanArguments f dvm args = runIdentity $ mapDolanArgumentsM (\t -> return $ f t) dvm args

mapInvertArgsTypeF ::
    forall m (pshim :: PolyShimKind) fta ftb dv polarity (gt :: CCRVariancesKind dv) (gt' :: CCRVariancesKind dv) t.
    ( Monad m
    , CCRVariancesShim pshim
    , Is PolarityType polarity
    , TestEquality (fta 'Positive)
    , TestEquality (fta 'Negative)
    , TestEquality (ftb 'Positive)
    , TestEquality (ftb 'Negative)
    ) =>
    ( forall polarity' t'.
      Is PolarityType polarity' => fta (InvertPolarity polarity') t' -> m (PShimWit (pshim Type) ftb polarity' t')
    ) ->
    CCRVariancesMap dv gt ->
    CCRVariancesMap dv gt' ->
    CCRPolarArguments dv fta gt (InvertPolarity polarity) t ->
    PolarShim (pshim (CCRVariancesKind dv)) polarity gt gt' ->
    m (CCRPolarArgumentsShimWit pshim dv ftb gt' polarity t)
mapInvertArgsTypeF f dvma dvmb args conv =
    withInvertPolarity @polarity $ mapPolarCCRArgumentsFM (mapInvertCCRPolarArgumentShimWit f) dvma dvmb args conv

mapInvertDolanArgumentsM ::
    forall m (pshim :: PolyShimKind) fta ftb dv polarity gt t.
    ( Monad m
    , CCRVariancesShim pshim
    , Is PolarityType polarity
    , TestEquality (fta 'Positive)
    , TestEquality (fta 'Negative)
    , TestEquality (ftb 'Positive)
    , TestEquality (ftb 'Negative)
    ) =>
    ( forall polarity' t'.
      Is PolarityType polarity' => fta (InvertPolarity polarity') t' -> m (PShimWit (pshim Type) ftb polarity' t')
    ) ->
    CCRVariancesMap dv gt ->
    CCRPolarArguments dv fta gt (InvertPolarity polarity) t ->
    m (CCRPolarArgumentsShimWit pshim dv ftb gt polarity t)
mapInvertDolanArgumentsM f dvm args = let
    dvt = withInvertPolarity @polarity $ ccrArgumentsType args
    in case ccrVariancesCategory @pshim dvt of
        Dict -> withInvertPolarity @polarity $ mapInvertArgsTypeF f dvm dvm args id

mergeArgsTypeF ::
    forall (m :: Type -> Type) (pshim :: PolyShimKind) (fta :: Polarity -> Type -> Type) (ftb :: Polarity -> Type -> Type) (ftab :: Polarity -> Type -> Type) (dv :: CCRVariances) (polarity :: Polarity) (gta :: CCRVariancesKind dv) (gtb :: CCRVariancesKind dv) (gtab :: CCRVariancesKind dv) (ta :: Type) (tb :: Type).
    ( Monad m
    , CCRVariancesShim pshim
    , JoinMeetShim (pshim Type)
    , Is PolarityType polarity
    , TestEquality (fta 'Positive)
    , TestEquality (fta 'Negative)
    , TestEquality (ftb 'Positive)
    , TestEquality (ftb 'Negative)
    ) =>
    ( forall polarity' ta' tb'.
      Is PolarityType polarity' =>
      fta polarity' ta' -> ftb polarity' tb' -> m (PShimWit (pshim Type) ftab polarity' (JoinMeetType polarity' ta' tb'))
    ) ->
    CCRVariancesType dv ->
    CCRVariancesMap dv gta ->
    CCRVariancesMap dv gtb ->
    CCRVariancesMap dv gtab ->
    CCRPolarArguments dv fta gta polarity ta ->
    CCRPolarArguments dv ftb gtb polarity tb ->
    PolarShim (pshim (CCRVariancesKind dv)) polarity gta gtab ->
    PolarShim (pshim (CCRVariancesKind dv)) polarity gtb gtab ->
    m (CCRPolarArgumentsShimWit pshim dv ftab gtab polarity (JoinMeetType polarity ta tb))
mergeArgsTypeF _ NilListType NilCCRVariancesMap NilCCRVariancesMap NilCCRVariancesMap NilCCRArguments NilCCRArguments conva convb =
    return $ MkShimWit NilCCRArguments $ polarF conva convb
mergeArgsTypeF f (ConsListType svt dvt) (ConsCCRVariancesMap ccrva dvma) (ConsCCRVariancesMap ccrvb dvmb) (ConsCCRVariancesMap ccrvab dvmab) (ConsCCRArguments (sta :: _ ta0) dta) (ConsCCRArguments (stb :: _ tb0) dtb) conva convb = do
    Dict <- return $ ccrVarianceCoercibleKind svt
    MkShimWit stab svf <- mergeCCRPolarArgumentShimWit @m @(pshim Type) @fta @ftb @ftab @_ @polarity f sta stb
    Dict <- return $ ccrVariancesCategory @pshim dvt
    Dict <- return $ ccrVarianceCategory @(PolarShim (pshim Type) polarity) svt
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
            (polarShimTypeApply svt ccrva ccrvab conva $ svf . ccrPolar1 @(pshim Type) @polarity @_ @ta0 @tb0 svt)
            (polarShimTypeApply svt ccrvb ccrvab convb $ svf . ccrPolar2 @(pshim Type) @polarity @_ @ta0 @tb0 svt)
    return $ MkShimWit (ConsCCRArguments stab dtab) convab

mergeDolanArgumentsM ::
    forall m (pshim :: PolyShimKind) fta ftb ftab dv polarity gt ta tb.
    ( Monad m
    , CCRVariancesShim pshim
    , JoinMeetShim (pshim Type)
    , Is PolarityType polarity
    , TestEquality (fta 'Positive)
    , TestEquality (fta 'Negative)
    , TestEquality (ftb 'Positive)
    , TestEquality (ftb 'Negative)
    ) =>
    ( forall polarity' ta' tb'.
      Is PolarityType polarity' =>
      fta polarity' ta' -> ftb polarity' tb' -> m (PShimWit (pshim Type) ftab polarity' (JoinMeetType polarity' ta' tb'))
    ) ->
    CCRVariancesMap dv gt ->
    CCRPolarArguments dv fta gt polarity ta ->
    CCRPolarArguments dv ftb gt polarity tb ->
    m (CCRPolarArgumentsShimWit pshim dv ftab gt polarity (JoinMeetType polarity ta tb))
mergeDolanArgumentsM f dvm argsa argsb = let
    dvt = ccrArgumentsType argsa
    in case ccrVariancesCategory @pshim dvt of
        Dict -> mergeArgsTypeF @m @pshim f dvt dvm dvm dvm argsa argsb id id

mergeDolanArguments ::
    forall (pshim :: PolyShimKind) fta ftb ftab dv polarity gt ta tb.
    ( CCRVariancesShim pshim
    , JoinMeetShim (pshim Type)
    , Is PolarityType polarity
    , TestEquality (fta 'Positive)
    , TestEquality (fta 'Negative)
    , TestEquality (ftb 'Positive)
    , TestEquality (ftb 'Negative)
    ) =>
    ( forall polarity' ta' tb'.
      Is PolarityType polarity' =>
      fta polarity' ta' -> ftb polarity' tb' -> PShimWit (pshim Type) ftab polarity' (JoinMeetType polarity' ta' tb')
    ) ->
    CCRVariancesMap dv gt ->
    CCRPolarArguments dv fta gt polarity ta ->
    CCRPolarArguments dv ftb gt polarity tb ->
    CCRPolarArgumentsShimWit pshim dv ftab gt polarity (JoinMeetType polarity ta tb)
mergeDolanArguments f dvm argsa argsb = runIdentity $ mergeDolanArgumentsM (\a b -> Identity $ f a b) dvm argsa argsb

ccrArgumentsToShimArgumentsM' ::
    forall m (pshim :: PolyShimKind) (wa :: Polarity -> CCRArgumentKind) wb dv polarity (gta :: CCRVariancesKind dv) (gtb :: CCRVariancesKind dv) t.
    (Monad m, CCRVariancesShim pshim, Is PolarityType polarity) =>
    (forall (t' :: Type). wa polarity CoCCRVariance t' -> m (PolarShimWit (pshim Type) wb polarity t')) ->
    CovaryType dv ->
    CCRVariancesMap dv gta ->
    CCRVariancesMap dv gtb ->
    PolarShim (pshim (CCRVariancesKind dv)) polarity gta gtb ->
    CCRArguments (wa polarity) dv gta t ->
    m (PolarShimWit (pshim Type) (Arguments wb gtb) polarity t)
ccrArgumentsToShimArgumentsM' _ NilListType NilCCRVariancesMap NilCCRVariancesMap conv NilCCRArguments =
    return $ MkShimWit NilArguments conv
ccrArgumentsToShimArgumentsM' f (ConsListType Refl lc) (ConsCCRVariancesMap ccrva dvma) (ConsCCRVariancesMap ccrvb dvmb) conv (ConsCCRArguments sta dta) = do
    Dict <- return $ covaryKMCategory @pshim lc
    MkShimWit ta conva <- f sta
    MkShimWit tfa convfa <-
        ccrArgumentsToShimArgumentsM' f lc dvma dvmb (polarShimTypeApply CoCCRVarianceType ccrva ccrvb conv conva) dta
    return $ MkShimWit (ConsArguments ta tfa) convfa

ccrArgumentsToShimArgumentsM ::
    forall m (pshim :: PolyShimKind) (wa :: Polarity -> CCRArgumentKind) wb dv polarity f t.
    (Monad m, CCRVariancesShim pshim, Is PolarityType polarity) =>
    (forall t'. wa polarity CoCCRVariance t' -> m (PolarShimWit (pshim Type) wb polarity t')) ->
    CovaryType dv ->
    CovaryMap f ->
    CCRArguments (wa polarity) dv f t ->
    m (PolarShimWit (pshim Type) (Arguments wb f) polarity t)
ccrArgumentsToShimArgumentsM f lc covary args = let
    dvm = covaryToCCRVariancesMap lc covary
    conv =
        case covaryKMCategory @pshim lc of
            Dict -> id
    in ccrArgumentsToShimArgumentsM' f lc dvm dvm conv args

ccrArgumentsToArgumentsM ::
    forall m (wa :: CCRArgumentKind) wb dv f t.
    Applicative m =>
    (forall t'. wa CoCCRVariance t' -> m (wb t')) ->
    CovaryType dv ->
    CCRArguments wa dv f t ->
    m (Arguments wb f t)
ccrArgumentsToArgumentsM _ NilListType NilCCRArguments = pure NilArguments
ccrArgumentsToArgumentsM f (ConsListType Refl lc) (ConsCCRArguments sta dta) =
    ConsArguments <$> f sta <*> ccrArgumentsToArgumentsM f lc dta

dolanArgumentsToArgumentsM ::
    forall m (pshim :: PolyShimKind) wa wb dv polarity f t.
    (Monad m, CCRVariancesShim pshim, Is PolarityType polarity) =>
    (forall t'. wa polarity t' -> m (PolarShimWit (pshim Type) wb polarity t')) ->
    CovaryType dv ->
    CovaryMap f ->
    CCRPolarArguments dv wa f polarity t ->
    m (PolarShimWit (pshim Type) (Arguments wb f) polarity t)
dolanArgumentsToArgumentsM f = ccrArgumentsToShimArgumentsM $ \(CoCCRPolarArgument arg) -> f arg

dolanArgumentsToArguments ::
    forall (pshim :: PolyShimKind) wa wb dv polarity f t.
    (CCRVariancesShim pshim, Is PolarityType polarity) =>
    (forall t'. wa polarity t' -> PolarShimWit (pshim Type) wb polarity t') ->
    CovaryType dv ->
    CovaryMap f ->
    CCRPolarArguments dv wa f polarity t ->
    PolarShimWit (pshim Type) (Arguments wb f) polarity t
dolanArgumentsToArguments f lc covary args =
    runIdentity $ dolanArgumentsToArgumentsM (\wt -> Identity $ f wt) lc covary args

argumentsToDolanArgumentsM' ::
    forall m (pshim :: PolyShimKind) wa wb dv polarity (fa :: CCRVariancesKind dv) (fb :: CCRVariancesKind dv) t.
    (Monad m, CCRVariancesShim pshim, Is PolarityType polarity) =>
    (forall t'. wa t' -> m (PShimWit (pshim Type) wb polarity t')) ->
    CovaryType dv ->
    CovaryMap fa ->
    CovaryMap fb ->
    PolarShim (pshim (CCRVariancesKind dv)) polarity fa fb ->
    Arguments wa fa t ->
    m (CCRPolarArgumentsShimWit pshim dv wb fb polarity t)
argumentsToDolanArgumentsM' _ NilListType NilCovaryMap NilCovaryMap conv NilArguments =
    return $ MkShimWit NilCCRArguments conv
argumentsToDolanArgumentsM' f (ConsListType Refl ct) (ConsCovaryMap ccrva mma) (ConsCovaryMap ccrvb mmb) conv (ConsArguments arg args) = do
    Dict <- return $ covaryKMCategory @pshim ct
    MkShimWit ta conva <- f arg
    MkShimWit tfa convfa <-
        argumentsToDolanArgumentsM' f ct mma mmb (polarShimTypeApply CoCCRVarianceType ccrva ccrvb conv conva) args
    return $ MkShimWit (ConsCCRArguments (CoCCRPolarArgument ta) tfa) convfa

argumentsToDolanArgumentsM ::
    forall m (pshim :: PolyShimKind) wa wb dv polarity f t.
    (Monad m, CCRVariancesShim pshim, Is PolarityType polarity) =>
    (forall t'. wa t' -> m (PShimWit (pshim Type) wb polarity t')) ->
    CovaryType dv ->
    CovaryMap f ->
    Arguments wa f t ->
    m (CCRPolarArgumentsShimWit pshim dv wb f polarity t)
argumentsToDolanArgumentsM f ct cm args =
    argumentsToDolanArgumentsM'
        f
        ct
        cm
        cm
        ( case covaryKMCategory @pshim ct of
            Dict -> id
        )
        args

argumentsToDolanArguments ::
    forall (pshim :: PolyShimKind) wa wb dv polarity f t.
    (CCRVariancesShim pshim, Is PolarityType polarity) =>
    (forall t'. wa t' -> PShimWit (pshim Type) wb polarity t') ->
    CovaryType dv ->
    CovaryMap f ->
    Arguments wa f t ->
    CCRPolarArgumentsShimWit pshim dv wb f polarity t
argumentsToDolanArguments f ct cm args = runIdentity $ argumentsToDolanArgumentsM (\wt -> Identity $ f wt) ct cm args
