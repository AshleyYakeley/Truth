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

class ShowDepth t where
    showDepth :: Int -> t -> String
    goodDepth :: Int -> t -> Bool

type JMShim :: ShimKind Type
data JMShim a b where
    FuncJMShim :: forall a b. String -> (a -> b) -> JMShim a b
    IdentityJMShim :: forall t. JMShim t t
    CoerceJMShim :: forall a b. String -> Coercion a b -> JMShim a b
    ComposeJMShim ::
        forall a b c.
        JMShim b c ->
        JMShim a b ->
        JMShim a c -- first arg is never itself a ComposeJMShim
    InitFJMShim :: JMShim BottomType t
    TermFJMShim :: JMShim t TopType
    Join1JMShim :: JMShim t a -> JMShim t (JoinType a b)
    Join2JMShim :: JMShim t b -> JMShim t (JoinType a b)
    JoinFJMShim :: JMShim a t -> JMShim b t -> JMShim (JoinType a b) t
    Meet1JMShim :: JMShim a t -> JMShim (MeetType a b) t
    Meet2JMShim :: JMShim b t -> JMShim (MeetType a b) t
    MeetFJMShim :: JMShim t a -> JMShim t b -> JMShim t (MeetType a b)
    LazyJMShim :: forall a b. JMShim a b -> JMShim a b

instance Category JMShim where
    id = IdentityJMShim
    (.) :: forall a b c. JMShim b c -> JMShim a b -> JMShim a c
    p . IdentityJMShim = p
    IdentityJMShim . q = q
    _ . InitFJMShim = initf
    TermFJMShim . _ = termf
    p . q
        | Just pc <- shimToCoercion p
        , Just qc <- shimToCoercion q =
            CoerceJMShim (show p <> " . " <> show q) $ pc . qc
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

instance JoinMeetIsoShim JMShim

instance JoinMeetShim JMShim where
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

instance IsoMapShim JMShim

instance CoerceShim JMShim where
    coercionToShim = CoerceJMShim
    shimToCoercion IdentityJMShim = Just id
    shimToCoercion (CoerceJMShim _ c) = Just c
    shimToCoercion _ = Nothing

instance ShowDepth (JMShim a b) where
    showDepth 0 _ = "..."
    showDepth _ (FuncJMShim t _) = "[func " <> t <> "]"
    showDepth _ IdentityJMShim = "id"
    showDepth _ (CoerceJMShim t _) = "[coerce " <> t <> "]"
    showDepth i (ComposeJMShim s1 s2) = "(compose " <> showDepth i s1 <> " " <> showDepth i s2 <> ")"
    showDepth _ InitFJMShim = "initf"
    showDepth _ TermFJMShim = "termf"
    showDepth i (Join1JMShim s) = "(join1 " <> showDepth i s <> ")"
    showDepth i (Join2JMShim s) = "(join2 " <> showDepth i s <> ")"
    showDepth i (JoinFJMShim s1 s2) = "(joinf " <> showDepth i s1 <> " " <> showDepth i s2 <> ")"
    showDepth i (Meet1JMShim s) = "(meet1 " <> showDepth i s <> ")"
    showDepth i (Meet2JMShim s) = "(meet2 " <> showDepth i s <> ")"
    showDepth i (MeetFJMShim s1 s2) = "(meetf " <> showDepth i s1 <> " " <> showDepth i s2 <> ")"
    showDepth i (LazyJMShim f) = "(lazy " <> showDepth (pred i) f <> ")"
    goodDepth 0 _ = False
    goodDepth i (ComposeJMShim s1 s2) = goodDepth i s1 && goodDepth i s2
    goodDepth i (Join1JMShim s) = goodDepth i s
    goodDepth i (Join2JMShim s) = goodDepth i s
    goodDepth i (JoinFJMShim s1 s2) = goodDepth i s1 && goodDepth i s2
    goodDepth i (Meet1JMShim s) = goodDepth i s
    goodDepth i (Meet2JMShim s) = goodDepth i s
    goodDepth i (MeetFJMShim s1 s2) = goodDepth i s1 && goodDepth i s2
    goodDepth i (LazyJMShim f) = goodDepth (pred i) f
    goodDepth _ _ = True


instance Show (JMShim a b) where
    show = showDepth 1

instance FunctionShim JMShim where
    functionToShim = FuncJMShim

instance RecoverShim JMShim where
    shimToFunction :: forall a b. JMShim a b -> a -> b
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
    shimToFunction (LazyJMShim f) = shimToFunction f

instance LazyCategory JMShim where
    iLazy = LazyJMShim



type GenPolyShim :: ShimKind Type -> PolyShimKind
data GenPolyShim shim k a b where
    BaseGenPolyShim :: forall (shim :: ShimKind Type) (a :: Type) (b :: Type).
        shim a b -> GenPolyShim shim Type a b
    ConsGenPolyShim ::
        forall (shim :: ShimKind Type)
         (v :: CCRVariance) k (f :: CCRVarianceKind v -> k) (g :: CCRVarianceKind v -> k) (a :: CCRVarianceKind v) (b :: CCRVarianceKind v).
        CCRVarianceType v ->
        CCRVariation v f ->
        CCRVariation v g ->
        GenPolyShim shim (CCRVarianceKind v -> k) f g ->
        CCRVarianceCategory shim v a b ->
        GenPolyShim shim k (f a) (g b)








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
