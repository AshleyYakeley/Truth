module Data.Shim.JMShim
    ( JMShim
    , JMRange
    , JMIsoShim(..)
    , cfmapNR
    , ccontramapNR
    , crangemapNR
    ) where

import Data.Shim.JoinMeet
import Data.Shim.PolyShim
import Data.Shim.Range

--import Data.Shim.MapRange
import Shapes

data JMShim (a :: k) (b :: k) where
    FuncJMShim :: KindFunction a b -> JMShim a b
    IdentityJMShim :: JMShim t t
    CoerceJMShim :: Coercion a b -> JMShim a b
    InitFJMShim :: JMShim BottomType t
    TermFJMShim :: JMShim t TopType
    Join1JMShim :: JMShim t a -> JMShim t (JoinType a b)
    Join2JMShim :: JMShim t b -> JMShim t (JoinType a b)
    JoinFJMShim :: JMShim a t -> JMShim b t -> JMShim (JoinType a b) t
    Meet1JMShim :: JMShim a t -> JMShim (MeetType a b) t
    Meet2JMShim :: JMShim b t -> JMShim (MeetType a b) t
    MeetFJMShim :: JMShim t a -> JMShim t b -> JMShim t (MeetType a b)
    CoJMShim
        :: forall kp kq (f :: kp -> kq) (g :: kp -> kq) (a :: kp) (b :: kp).
           (CoercibleKind kp, InKind f, InKind g, InKind a, InKind b, CatFunctor KindFunction KindFunction g)
        => Maybe (Dict (RepresentationalRole f))
        -> Maybe (Dict (RepresentationalRole g))
        -> JMShim f g
        -> JMShim a b
        -> JMShim (f a) (g b)
    ContraJMShim
        :: forall kp kq (f :: kp -> kq) (g :: kp -> kq) (a :: kp) (b :: kp).
           (CoercibleKind kp, InKind f, InKind g, InKind a, InKind b, CatFunctor (CatDual KindFunction) KindFunction g)
        => Maybe (Dict (RepresentationalRole f))
        -> Maybe (Dict (RepresentationalRole g))
        -> JMShim f g
        -> JMShim b a
        -> JMShim (f a) (g b)
    RangeJMShim
        :: forall kp kq (f :: (kp, kp) -> kq) (g :: (kp, kp) -> kq) (pa :: kp) (pb :: kp) (qa :: kp) (qb :: kp).
           ( CoercibleKind kp
           , InKind f
           , InKind g
           , InKind pa
           , InKind pb
           , InKind qa
           , InKind qb
           , CatFunctor (CatRange KindFunction) KindFunction g
           )
        => Maybe (Dict (RepresentationalRole f))
        -> Maybe (Dict (RepresentationalRole g))
        -> JMShim f g
        -> CatRange JMShim '( pa, qa) '( pb, qb)
        -> JMShim (f '( pa, qa)) (g '( pb, qb))
    LiftJMShim -- TODO: remove?
        :: forall kp kq (f :: kp -> kq) (g :: kp -> kq) (a :: kp). (CoercibleKind kp, InKind f, InKind g, InKind a)
        => Maybe (Dict (RepresentationalRole f))
        -> Maybe (Dict (RepresentationalRole g))
        -> JMShim f g
        -> JMShim (f a) (g a) -- probably ruins everything

instance CoercibleKind k => InCategory (JMShim :: k -> k -> Type) where
    cid = IdentityJMShim
    (<.>) ::
           forall (a :: k) (b :: k) (c :: k). (InKind a, InKind b, InKind c)
        => JMShim b c
        -> JMShim a b
        -> JMShim a c
    p <.> IdentityJMShim = p
    IdentityJMShim <.> q = q
    _ <.> InitFJMShim = InitFJMShim
    TermFJMShim <.> _ = TermFJMShim
    CoJMShim _ mrrp pf pa <.> CoJMShim mrrq _ qf qa = CoJMShim mrrq mrrp (pf <.> qf) (pa <.> qa)
    ContraJMShim _ mrrp pf pa <.> ContraJMShim mrrq _ qf qa = ContraJMShim mrrq mrrp (pf <.> qf) (qa <.> pa)
    RangeJMShim _ mrrp pf pa <.> RangeJMShim mrrq _ qf qa = RangeJMShim mrrq mrrp (pf <.> qf) (pa <.> qa)
    p <.> q
        | Just pc <- enhancedCoercion p
        , Just qc <- enhancedCoercion q = CoerceJMShim $ pc <.> qc
    Join1JMShim p <.> q = Join1JMShim $ p <.> q
    Join2JMShim p <.> q = Join2JMShim $ p <.> q
    p <.> JoinFJMShim ta tb = JoinFJMShim (p <.> ta) (p <.> tb)
    JoinFJMShim ap _ <.> Join1JMShim qa = ap <.> qa
    JoinFJMShim _ bp <.> Join2JMShim qb = bp <.> qb
    p <.> Meet1JMShim q = Meet1JMShim $ p <.> q
    p <.> Meet2JMShim q = Meet2JMShim $ p <.> q
    MeetFJMShim ta tb <.> q = MeetFJMShim (ta <.> q) (tb <.> q)
    Meet1JMShim aq <.> MeetFJMShim pa _ = aq <.> pa
    Meet2JMShim bq <.> MeetFJMShim _ pb = bq <.> pb
    p <.> q = FuncJMShim $ fromEnhanced p <.> fromEnhanced q

instance Category (JMShim :: Type -> Type -> Type) where
    id = cid
    (.) = (<.>)

type JMRange = Range JMShim

instance LiftPolyCategory JMShim where
    coLift _ _ IdentityJMShim = IdentityJMShim
    coLift mrrf mrrg jmf = LiftJMShim mrrf mrrg jmf

newtype JMIsoShim (a :: k) (b :: k) = MkJMIsoShim
    { unJMIsoShim :: Isomorphism JMShim a b
    }

instance CoercibleKind k => InCategory (JMIsoShim :: k -> k -> Type) where
    cid = MkJMIsoShim cid
    MkJMIsoShim p <.> MkJMIsoShim q = MkJMIsoShim $ p <.> q

instance Category (JMIsoShim :: Type -> Type -> Type) where
    id = cid
    (.) = (<.>)

instance LiftPolyCategory JMIsoShim where
    coLift mrrp mrrq (MkJMIsoShim (MkIsomorphism ab ba)) =
        MkJMIsoShim $ MkIsomorphism (coLift mrrp mrrq ab) (coLift mrrq mrrp ba)

instance forall kp kq (f :: kp -> kq). ( CoercibleKind kp
         , InKind f
         , CatFunctor KindFunction KindFunction f
         , RepresentationalRole f
         ) => CatFunctor JMShim JMShim f where
    cfmap IdentityJMShim = IdentityJMShim
    cfmap f = CoJMShim (Just Dict) (Just Dict) IdentityJMShim f

cfmapNR ::
       forall kp kq (f :: kp -> kq) (a :: kp) (b :: kp).
       (CoercibleKind kp, InKind f, CatFunctor KindFunction KindFunction f, InKind a, InKind b)
    => JMShim a b
    -> JMShim (f a) (f b)
cfmapNR IdentityJMShim = IdentityJMShim
cfmapNR f = CoJMShim Nothing Nothing IdentityJMShim f

ccontramapNR ::
       forall kp kq (f :: kp -> kq) (a :: kp) (b :: kp).
       (CoercibleKind kp, InKind f, CatFunctor (CatDual KindFunction) KindFunction f, InKind a, InKind b)
    => CatDual JMShim a b
    -> JMShim (f a) (f b)
ccontramapNR (MkCatDual IdentityJMShim) = IdentityJMShim
ccontramapNR (MkCatDual f) = ContraJMShim Nothing Nothing IdentityJMShim f

crangemapNR ::
       forall kp kq (f :: (kp, kp) -> kq) (a :: (kp, kp)) (b :: (kp, kp)).
       (CoercibleKind kp, InKind f, CatFunctor (CatRange KindFunction) KindFunction f)
    => CatRange JMShim a b
    -> JMShim (f a) (f b)
crangemapNR (MkCatRange IdentityJMShim IdentityJMShim) = IdentityJMShim
crangemapNR f@(MkCatRange _ _) = RangeJMShim Nothing Nothing IdentityJMShim f

instance forall kp kq (f :: kp -> kq). ( CoercibleKind kp
         , InKind f
         , CatFunctor (CatDual KindFunction) KindFunction f
         , RepresentationalRole f
         ) => CatFunctor (CatDual JMShim) JMShim f where
    cfmap (MkCatDual IdentityJMShim) = IdentityJMShim
    cfmap (MkCatDual jmfa) = ContraJMShim (Just Dict) (Just Dict) IdentityJMShim jmfa

instance ( kp ~ Type
         , CoercibleKind kp
         , InKind f
         , CatFunctor (CatRange KindFunction) KindFunction f
         , RepresentationalRole f
         ) => CatFunctor (CatRange JMShim) JMShim (f :: (kp, kp) -> kq) where
    cfmap ::
           forall a b. (InKind a, InKind b)
        => CatRange JMShim a b
        -> JMShim (f a) (f b)
    cfmap (MkCatRange IdentityJMShim IdentityJMShim) = IdentityJMShim
    cfmap jmf =
        case (inKind @_ @a, inKind @_ @b) of
            (MkPairWitness, MkPairWitness) -> RangeJMShim (Just Dict) (Just Dict) IdentityJMShim jmf

instance JoinMeetCategory JMShim where
    initf = InitFJMShim
    termf = TermFJMShim
    join1 = Join1JMShim id
    join2 = Join2JMShim id
    joinf = JoinFJMShim
    meet1 = Meet1JMShim id
    meet2 = Meet2JMShim id
    meetf = MeetFJMShim

instance CoercibleKind k => EnhancedFunction (JMShim :: k -> k -> Type) where
    toEnhanced = FuncJMShim
    fromEnhanced ::
           forall (a :: k) (b :: k). (InKind a, InKind b)
        => JMShim a b
        -> KindFunction a b
    fromEnhanced f
        | Just c <- enhancedCoercion f = coercionToFunction c
    fromEnhanced (FuncJMShim f) = f
    fromEnhanced IdentityJMShim = cid
    fromEnhanced (CoerceJMShim c) = coercionToFunction c
    fromEnhanced InitFJMShim = initf
    fromEnhanced TermFJMShim = termf
    fromEnhanced (Join1JMShim ta) = join1 <.> fromEnhanced ta
    fromEnhanced (Join2JMShim tb) = join2 <.> fromEnhanced tb
    fromEnhanced (JoinFJMShim ap bp) = joinf (fromEnhanced ap) (fromEnhanced bp)
    fromEnhanced (Meet1JMShim ta) = fromEnhanced ta <.> meet1
    fromEnhanced (Meet2JMShim tb) = fromEnhanced tb <.> meet2
    fromEnhanced (MeetFJMShim pa pb) = meetf (fromEnhanced pa) (fromEnhanced pb)
    fromEnhanced (CoJMShim _ _ (jmf :: JMShim f g) jma) =
        case inKind @_ @g of
            MkFunctionKindWitness ->
                case fromEnhanced jmf of
                    MkNestedMorphism ff -> cfmap (fromEnhanced jma) <.> ff
    fromEnhanced (ContraJMShim _ _ (jmf :: JMShim f g) jma) =
        case inKind @_ @g of
            MkFunctionKindWitness ->
                case fromEnhanced jmf of
                    MkNestedMorphism ff -> ccontramap (fromEnhanced jma) <.> ff
    fromEnhanced (RangeJMShim _ _ (jmf :: JMShim f g) (MkCatRange jmp jmq)) =
        case inKind @_ @g of
            MkFunctionKindWitness ->
                case fromEnhanced jmf of
                    MkNestedMorphism ff -> cfmap (MkCatRange (fromEnhanced jmp) (fromEnhanced jmq)) <.> ff
    fromEnhanced (LiftJMShim _ _ fg) =
        case fromEnhanced fg of
            MkNestedMorphism ff -> ff
    coercionEnhanced = CoerceJMShim
    enhancedCoercion IdentityJMShim = Just cid
    enhancedCoercion (CoerceJMShim c) = Just c
    enhancedCoercion (CoJMShim (Just Dict) _ f a) = do
        cf <- enhancedCoercion f
        ca <- enhancedCoercion a
        return $ applyCoercion1 cf ca
    enhancedCoercion (CoJMShim _ (Just Dict) f a) = do
        cf <- enhancedCoercion f
        ca <- enhancedCoercion a
        return $ applyCoercion2 cf ca
    enhancedCoercion (ContraJMShim (Just Dict) _ f a) = do
        cf <- enhancedCoercion f
        ca <- enhancedCoercion a
        return $ applyCoercion1 cf $ invert ca
    enhancedCoercion (ContraJMShim _ (Just Dict) f a) = do
        cf <- enhancedCoercion f
        ca <- enhancedCoercion a
        return $ applyCoercion2 cf $ invert ca
    enhancedCoercion (LiftJMShim (Just Dict) _ f) = do
        cf <- enhancedCoercion f
        return $ applyCoercion1 cf id
    enhancedCoercion (LiftJMShim _ (Just Dict) f) = do
        cf <- enhancedCoercion f
        return $ applyCoercion2 cf id
    enhancedCoercion _ = Nothing

instance PolyShim JMShim where
    coShimFunc = CoJMShim
    contraShimFunc = ContraJMShim
    rangeShimFunc = RangeJMShim

instance Shim JMShim where
    funcShim ab pq = coShimFuncR (contraShimFuncR cid ab) pq
    pairShim ab pq = coShimFuncR (coShimFuncR cid ab) pq
    eitherShim ab pq = coShimFuncR (coShimFuncR cid ab) pq
