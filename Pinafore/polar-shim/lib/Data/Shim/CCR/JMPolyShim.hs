module Data.Shim.CCR.JMPolyShim
    ( JMPolyShim
    , showJMPolyShim
    , goodJMPolyShim
    )
where

import Shapes

import Data.Shim.CCR.Apply
import Data.Shim.CCR.Variance
import Data.Shim.CCR.Variances
import Data.Shim.Mono
import Data.Shim.Polar
import Data.Shim.Poly
import Data.Shim.Range

type JMPolyShim :: PolyShimKind
data JMPolyShim k a b where
    FuncJMPolyShim :: forall k (a :: k) (b :: k). String -> KindFunction a b -> JMPolyShim k a b
    IdentityJMPolyShim :: forall k (t :: k). JMPolyShim k t t
    CoerceJMPolyShim :: forall k (a :: k) (b :: k). String -> Coercion a b -> JMPolyShim k a b
    ComposeJMPolyShim ::
        forall k (a :: k) (b :: k) (c :: k).
        JMPolyShim k b c ->
        JMPolyShim k a b ->
        JMPolyShim k a c -- first arg is never itself a ComposeJMPolyShim
    InitFJMPolyShim :: JMPolyShim Type BottomType t
    TermFJMPolyShim :: JMPolyShim Type t TopType
    Join1JMPolyShim :: JMPolyShim Type t a -> JMPolyShim Type t (JoinType a b)
    Join2JMPolyShim :: JMPolyShim Type t b -> JMPolyShim Type t (JoinType a b)
    JoinFJMPolyShim :: JMPolyShim Type a t -> JMPolyShim Type b t -> JMPolyShim Type (JoinType a b) t
    Meet1JMPolyShim :: JMPolyShim Type a t -> JMPolyShim Type (MeetType a b) t
    Meet2JMPolyShim :: JMPolyShim Type b t -> JMPolyShim Type (MeetType a b) t
    MeetFJMPolyShim :: JMPolyShim Type t a -> JMPolyShim Type t b -> JMPolyShim Type t (MeetType a b)
    ConsJMPolyShim ::
        forall (v :: CCRVariance) k (f :: CCRVarianceKind v -> k) (g :: CCRVarianceKind v -> k) (a :: CCRVarianceKind v) (b :: CCRVarianceKind v).
        CCRVarianceType v ->
        CCRVariation v f ->
        CCRVariation v g ->
        JMPolyShim (CCRVarianceKind v -> k) f g ->
        CCRVarianceCategory (JMPolyShim Type) v a b ->
        JMPolyShim k (f a) (g b)
    LazyJMPolyShim :: forall k (a :: k) (b :: k). JMPolyShim k a b -> JMPolyShim k a b

goodJMPolyShim :: Int -> JMPolyShim k a b -> Bool
goodJMPolyShim 0 _ = False
goodJMPolyShim i (ComposeJMPolyShim s1 s2) = goodJMPolyShim i s1 && goodJMPolyShim i s2
goodJMPolyShim i (Join1JMPolyShim s) = goodJMPolyShim i s
goodJMPolyShim i (Join2JMPolyShim s) = goodJMPolyShim i s
goodJMPolyShim i (JoinFJMPolyShim s1 s2) = goodJMPolyShim i s1 && goodJMPolyShim i s2
goodJMPolyShim i (Meet1JMPolyShim s) = goodJMPolyShim i s
goodJMPolyShim i (Meet2JMPolyShim s) = goodJMPolyShim i s
goodJMPolyShim i (MeetFJMPolyShim s1 s2) = goodJMPolyShim i s1 && goodJMPolyShim i s2
goodJMPolyShim i (LazyJMPolyShim f) = goodJMPolyShim (pred i) f
goodJMPolyShim _ _ = True

ccrVarianceCategoryShowJMPolyShim ::
    forall (sv :: CCRVariance) a b. CCRVarianceType sv -> Int -> CCRVarianceCategory (JMPolyShim Type) sv a b -> String
ccrVarianceCategoryShowJMPolyShim CoCCRVarianceType i f = showJMPolyShim i f
ccrVarianceCategoryShowJMPolyShim ContraCCRVarianceType i (MkCatDual f) = showJMPolyShim i f
ccrVarianceCategoryShowJMPolyShim RangeCCRVarianceType i (MkCatRange pp qq) =
    "(" <> showJMPolyShim i pp <> "," <> showJMPolyShim i qq <> ")"

showJMPolyShim :: Int -> JMPolyShim k a b -> String
showJMPolyShim 0 _ = "..."
showJMPolyShim _ (FuncJMPolyShim t _) = "[func " <> t <> "]"
showJMPolyShim _ IdentityJMPolyShim = "id"
showJMPolyShim _ (CoerceJMPolyShim t _) = "[coerce " <> t <> "]"
showJMPolyShim i (ComposeJMPolyShim s1 s2) = "(compose " <> showJMPolyShim i s1 <> " " <> showJMPolyShim i s2 <> ")"
showJMPolyShim _ InitFJMPolyShim = "initf"
showJMPolyShim _ TermFJMPolyShim = "termf"
showJMPolyShim i (Join1JMPolyShim s) = "(join1 " <> showJMPolyShim i s <> ")"
showJMPolyShim i (Join2JMPolyShim s) = "(join2 " <> showJMPolyShim i s <> ")"
showJMPolyShim i (JoinFJMPolyShim s1 s2) = "(joinf " <> showJMPolyShim i s1 <> " " <> showJMPolyShim i s2 <> ")"
showJMPolyShim i (Meet1JMPolyShim s) = "(meet1 " <> showJMPolyShim i s <> ")"
showJMPolyShim i (Meet2JMPolyShim s) = "(meet2 " <> showJMPolyShim i s <> ")"
showJMPolyShim i (MeetFJMPolyShim s1 s2) = "(meetf " <> showJMPolyShim i s1 <> " " <> showJMPolyShim i s2 <> ")"
showJMPolyShim i (ConsJMPolyShim vt _ _ s1 s2) =
    "(" <> show vt <> " " <> showJMPolyShim i s1 <> " " <> ccrVarianceCategoryShowJMPolyShim vt i s2 <> ")"
showJMPolyShim i (LazyJMPolyShim f) = "(lazy " <> showJMPolyShim (pred i) f <> ")"

instance Show (JMPolyShim k a b) where
    show = showJMPolyShim 1

instance CoercibleKind k => Category (JMPolyShim k) where
    id = IdentityJMPolyShim
    (.) :: forall (a :: k) (b :: k) (c :: k). JMPolyShim k b c -> JMPolyShim k a b -> JMPolyShim k a c
    p . IdentityJMPolyShim = p
    IdentityJMPolyShim . q = q
    _ . InitFJMPolyShim = initf
    TermFJMPolyShim . _ = termf
    ConsJMPolyShim pvt _ ccrvg pf pa . ConsJMPolyShim qvt ccrvf _ qf qa
        | Just Refl <- testEquality pvt qvt =
            case ccrVarianceCategory @(JMPolyShim Type) pvt of
                Dict -> applyPolyShim pvt ccrvf ccrvg (pf . qf) (pa . qa)
    p . q
        | Just pc <- shimToCoercion p
        , Just qc <- shimToCoercion q =
            CoerceJMPolyShim (show p <> " . " <> show q) $ pc . qc
    Join1JMPolyShim p . q = Join1JMPolyShim $ p . q
    Join2JMPolyShim p . q = Join2JMPolyShim $ p . q
    p . JoinFJMPolyShim ta tb = joinf (p . ta) (p . tb)
    JoinFJMPolyShim ap _ . Join1JMPolyShim qa = ap . qa
    JoinFJMPolyShim _ bp . Join2JMPolyShim qb = bp . qb
    p . Meet1JMPolyShim q = Meet1JMPolyShim $ p . q
    p . Meet2JMPolyShim q = Meet2JMPolyShim $ p . q
    MeetFJMPolyShim ta tb . q = meetf (ta . q) (tb . q)
    Meet1JMPolyShim aq . MeetFJMPolyShim pa _ = aq . pa
    Meet2JMPolyShim bq . MeetFJMPolyShim _ pb = bq . pb
    FuncJMPolyShim tp p . FuncJMPolyShim tq q = FuncJMPolyShim (tp <> "." <> tq) $ p . q
    f . ComposeJMPolyShim p q = (f . p) . q
    ComposeJMPolyShim p q . f =
        case q . f of
            qf@(ComposeJMPolyShim _ _) -> ComposeJMPolyShim p qf
            qf -> p . qf
    p . q = ComposeJMPolyShim p q

instance ApplyPolyShim JMPolyShim where
    applyPolyShim CoCCRVarianceType _ _ IdentityJMPolyShim IdentityJMPolyShim = IdentityJMPolyShim
    applyPolyShim ContraCCRVarianceType _ _ IdentityJMPolyShim (MkCatDual IdentityJMPolyShim) = IdentityJMPolyShim
    applyPolyShim RangeCCRVarianceType _ _ IdentityJMPolyShim (MkCatRange IdentityJMPolyShim IdentityJMPolyShim) = IdentityJMPolyShim
    applyPolyShim vt ccrvf ccrvg jmf jma = ConsJMPolyShim vt ccrvf ccrvg jmf jma

instance forall k (f :: Type -> k). HasCCRVariance CoCCRVariance f => CatFunctor (JMPolyShim Type) (JMPolyShim k) f where
    cfmap f = applyCoPolyShim ccrVariation ccrVariation IdentityJMPolyShim f

instance
    forall k (f :: Type -> k).
    HasCCRVariance ContraCCRVariance f =>
    CatFunctor (CatDual (JMPolyShim Type)) (JMPolyShim k) f
    where
    cfmap jmfa = applyPolyShim ContraCCRVarianceType ccrVariation ccrVariation IdentityJMPolyShim jmfa

instance
    forall k (f :: (Type, Type) -> k).
    HasCCRVariance 'RangeCCRVariance f =>
    CatFunctor (CatRange (JMPolyShim Type)) (JMPolyShim k) f
    where
    cfmap :: forall a b. CatRange (JMPolyShim Type) a b -> JMPolyShim k (f a) (f b)
    cfmap (MkCatRange IdentityJMPolyShim IdentityJMPolyShim) = IdentityJMPolyShim
    cfmap jmf = applyPolyShim RangeCCRVarianceType ccrVariation ccrVariation IdentityJMPolyShim jmf

instance JoinMeetIsoShim (JMPolyShim Type)

instance JoinMeetShim (JMPolyShim Type) where
    initf = InitFJMPolyShim
    termf = TermFJMPolyShim
    join1 = Join1JMPolyShim id
    join2 = Join2JMPolyShim id
    joinf (Join1JMPolyShim IdentityJMPolyShim) (Join2JMPolyShim IdentityJMPolyShim) = IdentityJMPolyShim
    joinf a b = JoinFJMPolyShim a b
    meet1 = Meet1JMPolyShim id
    meet2 = Meet2JMPolyShim id
    meetf (Meet1JMPolyShim IdentityJMPolyShim) (Meet2JMPolyShim IdentityJMPolyShim) = IdentityJMPolyShim
    meetf a b = MeetFJMPolyShim a b

instance CoercibleKind k => IsoMapShim (JMPolyShim k)

instance CoercibleKind k => CoerceShim (JMPolyShim k) where
    coercionToShim = CoerceJMPolyShim
    shimToCoercion IdentityJMPolyShim = Just id
    shimToCoercion (CoerceJMPolyShim _ c) = Just c
    shimToCoercion (ConsJMPolyShim CoCCRVarianceType ccrvf _ f a)
        | Just Dict <- ccrvMaybeRepresentational ccrvf = do
            cf <- shimToCoercion f
            ca <- shimToCoercion a
            return $ applyCoercion1 cf ca
    shimToCoercion (ConsJMPolyShim CoCCRVarianceType _ ccrvg f a)
        | Just Dict <- ccrvMaybeRepresentational ccrvg = do
            cf <- shimToCoercion f
            ca <- shimToCoercion a
            return $ applyCoercion2 cf ca
    shimToCoercion (ConsJMPolyShim ContraCCRVarianceType ccrvf _ f (MkCatDual a))
        | Just Dict <- ccrvMaybeRepresentational ccrvf = do
            cf <- shimToCoercion f
            ca <- shimToCoercion a
            return $ applyCoercion1 cf $ invert ca
    shimToCoercion (ConsJMPolyShim ContraCCRVarianceType _ ccrvg f (MkCatDual a))
        | Just Dict <- ccrvMaybeRepresentational ccrvg = do
            cf <- shimToCoercion f
            ca <- shimToCoercion a
            return $ applyCoercion2 cf $ invert ca
    shimToCoercion _ = Nothing

instance CoercibleKind k => FunctionShim (JMPolyShim k) where
    functionToShim = FuncJMPolyShim

instance CoercibleKind k => RecoverShim (JMPolyShim k) where
    shimToFunction :: forall (a :: k) (b :: k). JMPolyShim k a b -> KindFunction a b
    shimToFunction f
        | Just c <- shimToCoercion f = coercionToFunction c
    shimToFunction (FuncJMPolyShim _ f) = f
    shimToFunction IdentityJMPolyShim = id
    shimToFunction (ComposeJMPolyShim a b) = shimToFunction a . shimToFunction b
    shimToFunction (CoerceJMPolyShim _ c) = coercionToFunction c
    shimToFunction InitFJMPolyShim = initf
    shimToFunction TermFJMPolyShim = termf
    shimToFunction (Join1JMPolyShim ta) = join1 . shimToFunction ta
    shimToFunction (Join2JMPolyShim tb) = join2 . shimToFunction tb
    shimToFunction (JoinFJMPolyShim ap bp) = joinf (shimToFunction ap) (shimToFunction bp)
    shimToFunction (Meet1JMPolyShim ta) = shimToFunction ta . meet1
    shimToFunction (Meet2JMPolyShim tb) = shimToFunction tb . meet2
    shimToFunction (MeetFJMPolyShim pa pb) = meetf (shimToFunction pa) (shimToFunction pb)
    shimToFunction (ConsJMPolyShim vt _ ccrvg jmf jma) = let
        fromCon ::
            forall (v :: CCRVariance) (f :: CCRVarianceKind v -> k) (g :: CCRVarianceKind v -> k) (a' :: CCRVarianceKind v) (b' :: CCRVarianceKind v).
            CCRVarianceType v ->
            CCRVariation v g ->
            JMPolyShim (CCRVarianceKind v -> k) f g ->
            CCRVarianceCategory (JMPolyShim Type) v a' b' ->
            KindFunction (f a') (g b')
        fromCon CoCCRVarianceType ccrvg' jmf' jma' =
            case shimToFunction jmf' of
                MkNestedMorphism ff -> ccrvMap ccrvg' (shimToFunction jma') . ff
        fromCon ContraCCRVarianceType ccrvg' jmf' (MkCatDual jma') =
            case shimToFunction jmf' of
                MkNestedMorphism ff -> ccrvMap ccrvg' (MkCatDual (shimToFunction jma')) . ff
        fromCon RangeCCRVarianceType ccrvg' jmf' (MkCatRange jma1 jma2) =
            case shimToFunction jmf' of
                MkNestedMorphism ff -> ccrvMap ccrvg' (MkCatRange (shimToFunction jma1) (shimToFunction jma2)) . ff
        in fromCon vt ccrvg jmf jma
    shimToFunction (LazyJMPolyShim f) = shimToFunction f

instance LazyCategory (JMPolyShim Type) where
    iLazy = LazyJMPolyShim

instance CartesianShim (JMPolyShim Type) where
    funcShim ab pq = applyCoPolyShim ccrVariation ccrVariation (applyContraPolyShim ccrVariation ccrVariation id ab) pq
    pairShim ab pq = applyCoPolyShim ccrVariation ccrVariation (applyCoPolyShim ccrVariation ccrVariation id ab) pq
    eitherShim ab pq = applyCoPolyShim ccrVariation ccrVariation (applyCoPolyShim ccrVariation ccrVariation id ab) pq
    shimExtractFunction :: JMPolyShim Type a (b -> c) -> (forall c'. JMPolyShim Type a (b -> c') -> JMPolyShim Type c' c -> r) -> r
    shimExtractFunction (ConsJMPolyShim CoCCRVarianceType ccrvf ccrvg fg cc) call = let
        abc' = applyCoPolyShim ccrvf ccrvg fg id
        in call abc' cc
    shimExtractFunction abc call = call abc id

instance CCRVariancesPolyShim JMPolyShim where
    ccrVariancesCategory NilListType = Dict
    ccrVariancesCategory (ConsListType _ lt) =
        case ccrVariancesCategory @JMPolyShim lt of
            Dict -> Dict

instance ReduciblePolyShim JMPolyShim where
    type ReducedPolyShim JMPolyShim = JMPolyShim
