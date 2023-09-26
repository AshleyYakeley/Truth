module Data.Shim.CCR.JM
    ( JMShim
    ) where

import Data.Shim.CCR.Apply
import Data.Shim.CCR.Variance
import Data.Shim.CCR.Variances
import Data.Shim.Mono
import Data.Shim.Polar
import Data.Shim.Poly
import Data.Shim.Range
import Shapes

type JMShim :: PolyShimKind
data JMShim k a b where
    FuncJMShim :: forall k (a :: k) (b :: k). String -> KindFunction a b -> JMShim k a b
    IdentityJMShim :: forall k (t :: k). JMShim k t t
    CoerceJMShim :: forall k (a :: k) (b :: k). String -> Coercion a b -> JMShim k a b
    ComposeJMShim
        :: forall k (a :: k) (b :: k) (c :: k).
           JMShim k b c
        -> JMShim k a b
        -> JMShim k a c -- first arg is never itself a ComposeJMShim
    InitFJMShim :: JMShim Type BottomType t
    TermFJMShim :: JMShim Type t TopType
    Join1JMShim :: JMShim Type t a -> JMShim Type t (JoinType a b)
    Join2JMShim :: JMShim Type t b -> JMShim Type t (JoinType a b)
    JoinFJMShim :: JMShim Type a t -> JMShim Type b t -> JMShim Type (JoinType a b) t
    Meet1JMShim :: JMShim Type a t -> JMShim Type (MeetType a b) t
    Meet2JMShim :: JMShim Type b t -> JMShim Type (MeetType a b) t
    MeetFJMShim :: JMShim Type t a -> JMShim Type t b -> JMShim Type t (MeetType a b)
    ConsJMShim
        :: forall (v :: CCRVariance) k (f :: CCRVarianceKind v -> k) (g :: CCRVarianceKind v -> k) (a :: CCRVarianceKind v) (b :: CCRVarianceKind v).
           CCRVarianceType v
        -> CCRVariation v f
        -> CCRVariation v g
        -> JMShim (CCRVarianceKind v -> k) f g
        -> CCRVarianceCategory (JMShim Type) v a b
        -> JMShim k (f a) (g b)

instance Show (JMShim k a b) where
    show (FuncJMShim t _) = "[func " <> t <> "]"
    show IdentityJMShim = "id"
    show (CoerceJMShim t _) = "[coerce " <> t <> "]"
    show (ComposeJMShim s1 s2) = "(compose " <> show s1 <> " " <> show s2 <> ")"
    show InitFJMShim = "initf"
    show TermFJMShim = "termf"
    show (Join1JMShim s) = "(join1 " <> show s <> ")"
    show (Join2JMShim s) = "(join2 " <> show s <> ")"
    show (JoinFJMShim s1 s2) = "(joinf " <> show s1 <> " " <> show s2 <> ")"
    show (Meet1JMShim s) = "(meet1 " <> show s <> ")"
    show (Meet2JMShim s) = "(meet2 " <> show s <> ")"
    show (MeetFJMShim s1 s2) = "(meetf " <> show s1 <> " " <> show s2 <> ")"
    show (ConsJMShim vt _ _ s1 s2) =
        "(" <> show vt <> " " <> show s1 <> " " <> ccrVarianceCategoryShow @(JMShim Type) vt s2 <> ")"

instance CoercibleKind k => Category (JMShim k) where
    id = IdentityJMShim
    (.) :: forall (a :: k) (b :: k) (c :: k). JMShim k b c -> JMShim k a b -> JMShim k a c
    p . IdentityJMShim = p
    IdentityJMShim . q = q
    _ . InitFJMShim = initf
    TermFJMShim . _ = termf
    ConsJMShim pvt _ ccrvg pf pa . ConsJMShim qvt ccrvf _ qf qa
        | Just Refl <- testEquality pvt qvt =
            case ccrVarianceCategory @(JMShim Type) pvt of
                Dict -> applyPolyShim pvt ccrvf ccrvg (pf . qf) (pa . qa)
    p . q
        | Just pc <- shimToCoercion p
        , Just qc <- shimToCoercion q = CoerceJMShim (show p <> " . " <> show q) $ pc . qc
    Join1JMShim p . q = Join1JMShim $ p . q
    Join2JMShim p . q = Join2JMShim $ p . q
    p . JoinFJMShim ta tb = joinf (p . ta) (p . tb)
    JoinFJMShim ap _ . Join1JMShim qa = ap . qa
    JoinFJMShim _ bp . Join2JMShim qb = bp . qb
    p . Meet1JMShim q = Meet1JMShim $ p . q
    p . Meet2JMShim q = Meet2JMShim $ p . q
    MeetFJMShim ta tb . q = meetf (ta . q) (tb . q)
    Meet1JMShim aq . MeetFJMShim pa _ = aq . pa
    Meet2JMShim bq . MeetFJMShim _ pb = bq . pb
    FuncJMShim tp p . FuncJMShim tq q = FuncJMShim (tp <> "." <> tq) $ p . q
    f . ComposeJMShim p q = (f . p) . q
    ComposeJMShim p q . f =
        case q . f of
            qf@(ComposeJMShim _ _) -> ComposeJMShim p qf
            qf -> p . qf
    p . q = ComposeJMShim p q

instance ApplyPolyShim JMShim where
    applyPolyShim CoCCRVarianceType _ _ IdentityJMShim IdentityJMShim = IdentityJMShim
    applyPolyShim ContraCCRVarianceType _ _ IdentityJMShim (MkCatDual IdentityJMShim) = IdentityJMShim
    applyPolyShim RangeCCRVarianceType _ _ IdentityJMShim (MkCatRange IdentityJMShim IdentityJMShim) = IdentityJMShim
    applyPolyShim vt ccrvf ccrvg jmf jma = ConsJMShim vt ccrvf ccrvg jmf jma

instance forall k (f :: Type -> k). HasCCRVariance CoCCRVariance f => CatFunctor (JMShim Type) (JMShim k) f where
    cfmap f = applyCoPolyShim ccrVariation ccrVariation IdentityJMShim f

instance forall k (f :: Type -> k). HasCCRVariance ContraCCRVariance f =>
             CatFunctor (CatDual (JMShim Type)) (JMShim k) f where
    cfmap jmfa = applyPolyShim ContraCCRVarianceType ccrVariation ccrVariation IdentityJMShim jmfa

instance forall k (f :: (Type, Type) -> k). HasCCRVariance 'RangeCCRVariance f =>
             CatFunctor (CatRange (JMShim Type)) (JMShim k) f where
    cfmap :: forall a b. CatRange (JMShim Type) a b -> JMShim k (f a) (f b)
    cfmap (MkCatRange IdentityJMShim IdentityJMShim) = IdentityJMShim
    cfmap jmf = applyPolyShim RangeCCRVarianceType ccrVariation ccrVariation IdentityJMShim jmf

instance JoinMeetIsoShim (JMShim Type)

instance JoinMeetShim (JMShim Type) where
    initf = InitFJMShim
    termf = TermFJMShim
    join1 = Join1JMShim id
    join2 = Join2JMShim id
    joinf (Join1JMShim IdentityJMShim) (Join2JMShim IdentityJMShim) = IdentityJMShim
    joinf a b = JoinFJMShim a b
    meet1 = Meet1JMShim id
    meet2 = Meet2JMShim id
    meetf (Meet1JMShim IdentityJMShim) (Meet2JMShim IdentityJMShim) = IdentityJMShim
    meetf a b = MeetFJMShim a b

instance CoercibleKind k => IsoMapShim (JMShim k)

instance CoercibleKind k => CoerceShim (JMShim k) where
    coercionToShim = CoerceJMShim
    shimToCoercion IdentityJMShim = Just id
    shimToCoercion (CoerceJMShim _ c) = Just c
    shimToCoercion (ConsJMShim CoCCRVarianceType ccrvf _ f a)
        | Just Dict <- ccrvMaybeRepresentational ccrvf = do
            cf <- shimToCoercion f
            ca <- shimToCoercion a
            return $ applyCoercion1 cf ca
    shimToCoercion (ConsJMShim CoCCRVarianceType _ ccrvg f a)
        | Just Dict <- ccrvMaybeRepresentational ccrvg = do
            cf <- shimToCoercion f
            ca <- shimToCoercion a
            return $ applyCoercion2 cf ca
    shimToCoercion (ConsJMShim ContraCCRVarianceType ccrvf _ f (MkCatDual a))
        | Just Dict <- ccrvMaybeRepresentational ccrvf = do
            cf <- shimToCoercion f
            ca <- shimToCoercion a
            return $ applyCoercion1 cf $ invert ca
    shimToCoercion (ConsJMShim ContraCCRVarianceType _ ccrvg f (MkCatDual a))
        | Just Dict <- ccrvMaybeRepresentational ccrvg = do
            cf <- shimToCoercion f
            ca <- shimToCoercion a
            return $ applyCoercion2 cf $ invert ca
    shimToCoercion _ = Nothing

instance CoercibleKind k => FunctionShim (JMShim k) where
    functionToShim = FuncJMShim

instance CoercibleKind k => RecoverShim (JMShim k) where
    shimToFunction :: forall (a :: k) (b :: k). JMShim k a b -> KindFunction a b
    shimToFunction f
        | Just c <- shimToCoercion f = coercionToFunction c
    shimToFunction (FuncJMShim _ f) = f
    shimToFunction IdentityJMShim = id
    shimToFunction (ComposeJMShim a b) = shimToFunction a . shimToFunction b
    shimToFunction (CoerceJMShim _ c) = coercionToFunction c
    shimToFunction InitFJMShim = initf
    shimToFunction TermFJMShim = termf
    shimToFunction (Join1JMShim ta) = join1 . shimToFunction ta
    shimToFunction (Join2JMShim tb) = join2 . shimToFunction tb
    shimToFunction (JoinFJMShim ap bp) = joinf (shimToFunction ap) (shimToFunction bp)
    shimToFunction (Meet1JMShim ta) = shimToFunction ta . meet1
    shimToFunction (Meet2JMShim tb) = shimToFunction tb . meet2
    shimToFunction (MeetFJMShim pa pb) = meetf (shimToFunction pa) (shimToFunction pb)
    shimToFunction (ConsJMShim vt _ ccrvg jmf jma) = let
        fromCon ::
               forall (v :: CCRVariance) (f :: CCRVarianceKind v -> k) (g :: CCRVarianceKind v -> k) (a' :: CCRVarianceKind v) (b' :: CCRVarianceKind v).
               CCRVarianceType v
            -> CCRVariation v g
            -> JMShim (CCRVarianceKind v -> k) f g
            -> CCRVarianceCategory (JMShim Type) v a' b'
            -> KindFunction (f a') (g b')
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

instance LazyCategory (JMShim Type)

instance CartesianShim (JMShim Type) where
    funcShim ab pq = applyCoPolyShim ccrVariation ccrVariation (applyContraPolyShim ccrVariation ccrVariation id ab) pq
    pairShim ab pq = applyCoPolyShim ccrVariation ccrVariation (applyCoPolyShim ccrVariation ccrVariation id ab) pq
    eitherShim ab pq = applyCoPolyShim ccrVariation ccrVariation (applyCoPolyShim ccrVariation ccrVariation id ab) pq
    shimExtractFunction :: JMShim Type a (b -> c) -> (forall c'. JMShim Type a (b -> c') -> JMShim Type c' c -> r) -> r
    shimExtractFunction (ConsJMShim CoCCRVarianceType ccrvf ccrvg fg cc) call = let
        abc' = applyCoPolyShim ccrvf ccrvg fg id
        in call abc' cc
    shimExtractFunction abc call = call abc id

instance CCRVariancesShim JMShim where
    ccrVariancesCategory NilListType = Dict
    ccrVariancesCategory (ConsListType _ lt) =
        case ccrVariancesCategory @JMShim lt of
            Dict -> Dict

instance ReduciblePolyShim JMShim where
    type ReducedPolyShim JMShim = JMShim
