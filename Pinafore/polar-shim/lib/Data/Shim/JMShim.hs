module Data.Shim.JMShim
    ( JMShim
    ) where

import Data.Shim.CCRVariance
import Data.Shim.CatRange
import Data.Shim.JoinMeet
import Data.Shim.MaybeRepresentational
import Data.Shim.PolyShim
import Shapes

type JMShim :: PolyShimKind
data JMShim k a b where
    FuncJMShim :: forall k (a :: k) (b :: k). String -> KindFunction a b -> JMShim k a b
    IdentityJMShim :: forall k (t :: k). JMShim k t t
    CoerceJMShim :: forall k (a :: k) (b :: k). String -> Coercion a b -> JMShim k a b
    ComposeJMShim
        :: forall k (a :: k) (b :: k) (c :: k). InKind b
        => JMShim k b c
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
    ApplFJMShim :: JMShim Type t1 (a -> b) -> JMShim Type t2 a -> JMShim Type (MeetType t1 t2) b
    ConsJMShim
        :: forall (v :: CCRVariance) k (f :: CCRVarianceKind v -> k) (g :: CCRVarianceKind v -> k) (a :: CCRVarianceKind v) (b :: CCRVarianceKind v).
           (InKind a, InKind b, HasCCRVariance v f, HasCCRVariance v g)
        => CCRVarianceType v
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
    show (ApplFJMShim s1 s2) = "(applf " <> show s1 <> " " <> show s2 <> ")"
    show (ConsJMShim vt s1 s2) =
        "(" <> show vt <> " " <> show s1 <> " " <> ccrVarianceCategoryShow @(JMShim Type) vt s2 <> ")"

instance CoercibleKind k => InCategory (JMShim k) where
    cid = IdentityJMShim
    (<.>) ::
           forall (a :: k) (b :: k) (c :: k). (InKind a, InKind b, InKind c)
        => JMShim k b c
        -> JMShim k a b
        -> JMShim k a c
    p <.> IdentityJMShim = p
    IdentityJMShim <.> q = q
    _ <.> InitFJMShim = initf
    TermFJMShim <.> _ = termf
    ConsJMShim pvt pf pa <.> ConsJMShim qvt qf qa
        | Just Refl <- testEquality pvt qvt =
            case ccrVarianceInCategory @(JMShim Type) pvt of
                Dict -> applyPolyShim pvt (pf <.> qf) (pa <.> qa)
    p <.> q
        | Just pc <- enhancedCoercion p
        , Just qc <- enhancedCoercion q = CoerceJMShim (show p <> " . " <> show q) $ pc <.> qc
    Join1JMShim p <.> q = Join1JMShim $ p <.> q
    Join2JMShim p <.> q = Join2JMShim $ p <.> q
    p <.> JoinFJMShim ta tb = joinf (p <.> ta) (p <.> tb)
    JoinFJMShim ap _ <.> Join1JMShim qa = ap <.> qa
    JoinFJMShim _ bp <.> Join2JMShim qb = bp <.> qb
    p <.> Meet1JMShim q = Meet1JMShim $ p <.> q
    p <.> Meet2JMShim q = Meet2JMShim $ p <.> q
    MeetFJMShim ta tb <.> q = meetf (ta <.> q) (tb <.> q)
    Meet1JMShim aq <.> MeetFJMShim pa _ = aq <.> pa
    Meet2JMShim bq <.> MeetFJMShim _ pb = bq <.> pb
    FuncJMShim tp p <.> FuncJMShim tq q = FuncJMShim (tp <> "." <> tq) $ p <.> q
    f <.> ComposeJMShim p q = (f <.> p) <.> q
    ComposeJMShim p q <.> f =
        case q <.> f of
            qf@(ComposeJMShim _ _) -> ComposeJMShim p qf
            qf -> p <.> qf
    p <.> q = ComposeJMShim p q

instance Category (JMShim Type) where
    id = cid
    (.) = (<.>)

instance ApplyPolyShim JMShim where
    applyPolyShim CoCCRVarianceType IdentityJMShim IdentityJMShim = IdentityJMShim
    applyPolyShim ContraCCRVarianceType IdentityJMShim (MkCatDual IdentityJMShim) = IdentityJMShim
    applyPolyShim RangeCCRVarianceType IdentityJMShim (MkCatRange IdentityJMShim IdentityJMShim) = IdentityJMShim
    applyPolyShim vt jmf jma = ConsJMShim vt jmf jma

instance forall k (f :: Type -> k). HasCCRVariance CoCCRVariance f => CatFunctor (JMShim Type) (JMShim k) f where
    cfmap f = applyCoPolyShim IdentityJMShim f

instance forall k (f :: Type -> k). HasCCRVariance ContraCCRVariance f =>
             CatFunctor (CatDual (JMShim Type)) (JMShim k) f where
    cfmap jmfa = applyPolyShim ContraCCRVarianceType IdentityJMShim jmfa

instance forall k (f :: (Type, Type) -> k). HasCCRVariance 'RangeCCRVariance f =>
             CatFunctor (CatRange (JMShim Type)) (JMShim k) f where
    cfmap ::
           forall a b. (InKind a, InKind b)
        => CatRange (JMShim Type) a b
        -> JMShim k (f a) (f b)
    cfmap (MkCatRange IdentityJMShim IdentityJMShim) = IdentityJMShim
    cfmap jmf =
        case (inKind @_ @a, inKind @_ @b) of
            (MkPairWitness, MkPairWitness) -> applyPolyShim RangeCCRVarianceType IdentityJMShim jmf

instance JoinMeetIsoCategory (JMShim Type)

instance JoinMeetCategory (JMShim Type) where
    initf = InitFJMShim
    termf = TermFJMShim
    join1 = Join1JMShim cid
    join2 = Join2JMShim cid
    joinf (Join1JMShim IdentityJMShim) (Join2JMShim IdentityJMShim) = IdentityJMShim
    joinf a b = JoinFJMShim a b
    meet1 = Meet1JMShim cid
    meet2 = Meet2JMShim cid
    meetf (Meet1JMShim IdentityJMShim) (Meet2JMShim IdentityJMShim) = IdentityJMShim
    meetf a b = MeetFJMShim a b
    applf (Meet1JMShim p) q = applf p q <.> iMeetPair meet1 cid
    applf (ConsJMShim CoCCRVarianceType IdentityJMShim p) q = p <.> applf cid q
    applf (ConsJMShim CoCCRVarianceType (ConsJMShim ContraCCRVarianceType IdentityJMShim (MkCatDual pa)) pb) q =
        pb <.> applf cid (pa <.> q)
    applf p q = ApplFJMShim p q

varrep1 ::
       forall shim f g. MaybeRepresentational f
    => shim f g
    -> Maybe (Dict (RepresentationalRole f))
varrep1 _ = maybeRepresentational @_ @_ @f

varrep2 ::
       forall shim f g. MaybeRepresentational g
    => shim f g
    -> Maybe (Dict (RepresentationalRole g))
varrep2 _ = maybeRepresentational @_ @_ @g

instance LazyCategory (JMShim Type)

instance CoercibleKind k => IsoMapShim (JMShim k)

instance CoercibleKind k => CoerceShim (JMShim k) where
    coercionEnhanced = CoerceJMShim
    enhancedCoercion IdentityJMShim = Just cid
    enhancedCoercion (CoerceJMShim _ c) = Just c
    enhancedCoercion (ConsJMShim CoCCRVarianceType f a)
        | Just Dict <- varrep1 f = do
            cf <- enhancedCoercion f
            ca <- enhancedCoercion a
            return $ applyCoercion1 cf ca
    enhancedCoercion (ConsJMShim CoCCRVarianceType f a)
        | Just Dict <- varrep2 f = do
            cf <- enhancedCoercion f
            ca <- enhancedCoercion a
            return $ applyCoercion2 cf ca
    enhancedCoercion (ConsJMShim ContraCCRVarianceType f (MkCatDual a))
        | Just Dict <- varrep1 f = do
            cf <- enhancedCoercion f
            ca <- enhancedCoercion a
            return $ applyCoercion1 cf $ invert ca
    enhancedCoercion (ConsJMShim ContraCCRVarianceType f (MkCatDual a))
        | Just Dict <- varrep2 f = do
            cf <- enhancedCoercion f
            ca <- enhancedCoercion a
            return $ applyCoercion2 cf $ invert ca
    enhancedCoercion _ = Nothing

instance CoercibleKind k => FunctionShim (JMShim k) where
    functionToShim = FuncJMShim
    shimToFunction ::
           forall (a :: k) (b :: k). (InKind a, InKind b)
        => JMShim k a b
        -> KindFunction a b
    shimToFunction f
        | Just c <- enhancedCoercion f = coercionToFunction c
    shimToFunction (FuncJMShim _ f) = f
    shimToFunction IdentityJMShim = cid
    shimToFunction (ComposeJMShim a b) = shimToFunction a <.> shimToFunction b
    shimToFunction (CoerceJMShim _ c) = coercionToFunction c
    shimToFunction InitFJMShim = initf
    shimToFunction TermFJMShim = termf
    shimToFunction (Join1JMShim ta) = join1 <.> shimToFunction ta
    shimToFunction (Join2JMShim tb) = join2 <.> shimToFunction tb
    shimToFunction (JoinFJMShim ap bp) = joinf (shimToFunction ap) (shimToFunction bp)
    shimToFunction (Meet1JMShim ta) = shimToFunction ta <.> meet1
    shimToFunction (Meet2JMShim tb) = shimToFunction tb <.> meet2
    shimToFunction (MeetFJMShim pa pb) = meetf (shimToFunction pa) (shimToFunction pb)
    shimToFunction (ApplFJMShim pa pb) = applf (shimToFunction pa) (shimToFunction pb)
    shimToFunction (ConsJMShim vt jmf jma) = let
        fromCon ::
               forall (v :: CCRVariance) (f :: CCRVarianceKind v -> k) (g :: CCRVarianceKind v -> k) (a' :: CCRVarianceKind v) (b' :: CCRVarianceKind v).
               ( InKind f
               , InKind g
               , InKind a'
               , InKind b'
               , CatFunctor (CCRVarianceCategory KindFunction v) KindFunction f
               , CatFunctor (CCRVarianceCategory KindFunction v) KindFunction g
               )
            => CCRVarianceType v
            -> JMShim (CCRVarianceKind v -> k) f g
            -> CCRVarianceCategory (JMShim Type) v a' b'
            -> KindFunction (f a') (g b')
        fromCon CoCCRVarianceType jmf' jma' =
            case inKind @_ @f of
                MkFunctionKindWitness ->
                    case inKind @_ @g of
                        MkFunctionKindWitness ->
                            case shimToFunction jmf' of
                                MkNestedMorphism ff -> cfmap (shimToFunction jma') <.> ff
        fromCon ContraCCRVarianceType jmf' (MkCatDual jma') =
            case inKind @_ @f of
                MkFunctionKindWitness ->
                    case inKind @_ @g of
                        MkFunctionKindWitness ->
                            case shimToFunction jmf' of
                                MkNestedMorphism ff -> cfmap (MkCatDual (shimToFunction jma')) <.> ff
        fromCon RangeCCRVarianceType jmf' (MkCatRange jma1 jma2) =
            case inKind @_ @f of
                MkFunctionKindWitness ->
                    case inKind @_ @g of
                        MkFunctionKindWitness ->
                            case shimToFunction jmf' of
                                MkNestedMorphism ff ->
                                    cfmap (MkCatRange (shimToFunction jma1) (shimToFunction jma2)) <.> ff
        in fromCon vt jmf jma

instance CartesianShim (JMShim Type) where
    funcShim ab pq = applyCoPolyShim (applyContraPolyShim cid ab) pq
    pairShim ab pq = applyCoPolyShim (applyCoPolyShim cid ab) pq
    eitherShim ab pq = applyCoPolyShim (applyCoPolyShim cid ab) pq
    shimExtractFunction :: JMShim Type a (b -> c) -> (forall c'. JMShim Type a (b -> c') -> JMShim Type c' c -> r) -> r
    shimExtractFunction (ConsJMShim CoCCRVarianceType fg cc) call = let
        abc' = applyCoPolyShim fg id
        in call abc' cc
    shimExtractFunction abc call = call abc cid
