module Data.Shim.JMShim
    ( JMShim
    , JMIsoShim(..)
    ) where

import Data.Shim.CatRange
import Data.Shim.JoinMeet
import Data.Shim.PolyShim
import Data.Shim.Variance
import Shapes

data JMShim (a :: k) (b :: k) where
    FuncJMShim :: String -> KindFunction a b -> JMShim a b
    IdentityJMShim :: JMShim t t
    CoerceJMShim :: String -> Coercion a b -> JMShim a b
    InitFJMShim :: JMShim BottomType t
    TermFJMShim :: JMShim t TopType
    Join1JMShim :: JMShim t a -> JMShim t (JoinType a b)
    Join2JMShim :: JMShim t b -> JMShim t (JoinType a b)
    JoinFJMShim :: JMShim a t -> JMShim b t -> JMShim (JoinType a b) t
    Meet1JMShim :: JMShim a t -> JMShim (MeetType a b) t
    Meet2JMShim :: JMShim b t -> JMShim (MeetType a b) t
    MeetFJMShim :: JMShim t a -> JMShim t b -> JMShim t (MeetType a b)
    ApJMShim
        :: forall (var :: Variance) k (f :: VarianceKind var -> k) (g :: VarianceKind var -> k) (a :: VarianceKind var) (b :: VarianceKind var).
           ( InKind f
           , InKind g
           , InKind a
           , InKind b
           , CatFunctor (VarianceCategory KindFunction var) KindFunction f
           , CatFunctor (VarianceCategory KindFunction var) KindFunction g
           )
        => VarianceType var
        -> Maybe (Dict (RepresentationalRole f))
        -> Maybe (Dict (RepresentationalRole g))
        -> JMShim f g
        -> VarianceCategory JMShim var a b
        -> JMShim (f a) (g b)
    LiftJMShim -- TODO: remove?
        :: forall kp kq (f :: kp -> kq) (g :: kp -> kq) (a :: kp). (CoercibleKind kp, InKind f, InKind g, InKind a)
        => Maybe (Dict (RepresentationalRole f))
        -> Maybe (Dict (RepresentationalRole g))
        -> JMShim f g
        -> JMShim (f a) (g a) -- probably ruins everything

instance Show (JMShim a b) where
    show (FuncJMShim t _) = "[func " <> t <> "]"
    show IdentityJMShim = "id"
    show (CoerceJMShim t _) = "[coerce " <> t <> "]"
    show InitFJMShim = "initf"
    show TermFJMShim = "termf"
    show (Join1JMShim s) = "(join1 " <> show s <> ")"
    show (Join2JMShim s) = "(join2 " <> show s <> ")"
    show (JoinFJMShim s1 s2) = "(joinf " <> show s1 <> " " <> show s2 <> ")"
    show (Meet1JMShim s) = "(meet1 " <> show s <> ")"
    show (Meet2JMShim s) = "(meet2 " <> show s <> ")"
    show (MeetFJMShim s1 s2) = "(meetf " <> show s1 <> " " <> show s2 <> ")"
    show (ApJMShim vt _ _ s1 s2) = "(" <> show vt <> " " <> show s1 <> " " <> varianceCategoryShow @JMShim vt s2 <> ")"
    show (LiftJMShim _ _ s) = "(lift " <> show s <> ")"

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
    ApJMShim pvt _ mrrp pf pa <.> ApJMShim qvt mrrq _ qf qa
        | Just Refl <- testEquality pvt qvt =
            case varianceInCategory @JMShim pvt of
                Dict -> ApJMShim pvt mrrq mrrp (pf <.> qf) (pa <.> qa)
    p <.> q
        | Just pc <- enhancedCoercion p
        , Just qc <- enhancedCoercion q = CoerceJMShim (show p <> " . " <> show q) $ pc <.> qc
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
    p@(FuncJMShim t _) <.> q = FuncJMShim t $ fromEnhanced p <.> fromEnhanced q
    p <.> q@(FuncJMShim t _) = FuncJMShim t $ fromEnhanced p <.> fromEnhanced q
    p <.> q = FuncJMShim (show p <> " . " <> show q) $ fromEnhanced p <.> fromEnhanced q

instance Category (JMShim :: Type -> Type -> Type) where
    id = cid
    (.) = (<.>)

instance ApPolyShim JMShim where
    coLift _ _ IdentityJMShim = IdentityJMShim
    coLift mrrf mrrg jmf = LiftJMShim mrrf mrrg jmf
    apShimFunc CovarianceType _ _ IdentityJMShim IdentityJMShim = IdentityJMShim
    apShimFunc ContravarianceType _ _ IdentityJMShim (MkCatDual IdentityJMShim) = IdentityJMShim
    apShimFunc RangevarianceType _ _ IdentityJMShim (MkCatRange IdentityJMShim IdentityJMShim) = IdentityJMShim
    apShimFunc vt mrrf mrrg jmf jma = ApJMShim vt mrrf mrrg jmf jma

newtype JMIsoShim (a :: k) (b :: k) = MkJMIsoShim
    { unJMIsoShim :: Isomorphism JMShim a b
    }

instance CoercibleKind k => InCategory (JMIsoShim :: k -> k -> Type) where
    cid = MkJMIsoShim cid
    MkJMIsoShim p <.> MkJMIsoShim q = MkJMIsoShim $ p <.> q

instance Category (JMIsoShim :: Type -> Type -> Type) where
    id = cid
    (.) = (<.>)

instance ApPolyShim JMIsoShim where
    coLift mrrp mrrq (MkJMIsoShim (MkIsomorphism ab ba)) =
        MkJMIsoShim $ MkIsomorphism (coLift mrrp mrrq ab) (coLift mrrq mrrp ba)
    apShimFunc CovarianceType mrrp mrrq (MkJMIsoShim (MkIsomorphism fab fba)) (MkJMIsoShim (MkIsomorphism xab xba)) =
        MkJMIsoShim $
        MkIsomorphism (apShimFunc CovarianceType mrrp mrrq fab xab) (apShimFunc CovarianceType mrrq mrrp fba xba)
    apShimFunc ContravarianceType mrrp mrrq (MkJMIsoShim (MkIsomorphism fab fba)) (MkCatDual (MkJMIsoShim (MkIsomorphism xab xba))) =
        MkJMIsoShim $
        MkIsomorphism
            (apShimFunc ContravarianceType mrrp mrrq fab $ MkCatDual xab)
            (apShimFunc ContravarianceType mrrq mrrp fba $ MkCatDual xba)
    apShimFunc RangevarianceType mrrp mrrq (MkJMIsoShim (MkIsomorphism fab fba)) (MkCatRange (MkJMIsoShim (MkIsomorphism xab1 xba1)) (MkJMIsoShim (MkIsomorphism xab2 xba2))) =
        MkJMIsoShim $
        MkIsomorphism
            (apShimFunc RangevarianceType mrrp mrrq fab (MkCatRange xab1 xab2))
            (apShimFunc RangevarianceType mrrq mrrp fba (MkCatRange xba1 xba2))

instance forall kq (f :: Type -> kq). (InKind f, CatFunctor KindFunction KindFunction f, RepresentationalRole f) =>
             CatFunctor JMShim JMShim f where
    cfmap IdentityJMShim = IdentityJMShim
    cfmap f = ApJMShim CovarianceType (Just Dict) (Just Dict) IdentityJMShim f

instance forall kq (f :: Type -> kq). ( InKind f
         , CatFunctor (CatDual KindFunction) KindFunction f
         , RepresentationalRole f
         ) => CatFunctor (CatDual JMShim) JMShim f where
    cfmap (MkCatDual IdentityJMShim) = IdentityJMShim
    cfmap jmfa = ApJMShim ContravarianceType (Just Dict) (Just Dict) IdentityJMShim jmfa

instance (InKind f, CatFunctor (CatRange KindFunction) KindFunction f, RepresentationalRole f) =>
             CatFunctor (CatRange JMShim) JMShim (f :: (Type, Type) -> kq) where
    cfmap ::
           forall a b. (InKind a, InKind b)
        => CatRange JMShim a b
        -> JMShim (f a) (f b)
    cfmap (MkCatRange IdentityJMShim IdentityJMShim) = IdentityJMShim
    cfmap jmf =
        case (inKind @_ @a, inKind @_ @b) of
            (MkPairWitness, MkPairWitness) -> ApJMShim RangevarianceType (Just Dict) (Just Dict) IdentityJMShim jmf

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
    fromEnhanced (FuncJMShim _ f) = f
    fromEnhanced IdentityJMShim = cid
    fromEnhanced (CoerceJMShim _ c) = coercionToFunction c
    fromEnhanced InitFJMShim = initf
    fromEnhanced TermFJMShim = termf
    fromEnhanced (Join1JMShim ta) = join1 <.> fromEnhanced ta
    fromEnhanced (Join2JMShim tb) = join2 <.> fromEnhanced tb
    fromEnhanced (JoinFJMShim ap bp) = joinf (fromEnhanced ap) (fromEnhanced bp)
    fromEnhanced (Meet1JMShim ta) = fromEnhanced ta <.> meet1
    fromEnhanced (Meet2JMShim tb) = fromEnhanced tb <.> meet2
    fromEnhanced (MeetFJMShim pa pb) = meetf (fromEnhanced pa) (fromEnhanced pb)
    fromEnhanced (ApJMShim vt _ _ jmf jma) = let
        fromAp ::
               forall (var :: Variance) (f :: VarianceKind var -> k) (g :: VarianceKind var -> k) (a' :: VarianceKind var) (b' :: VarianceKind var).
               ( InKind f
               , InKind g
               , InKind a'
               , InKind b'
               , CatFunctor (VarianceCategory KindFunction var) KindFunction f
               , CatFunctor (VarianceCategory KindFunction var) KindFunction g
               )
            => VarianceType var
            -> JMShim f g
            -> VarianceCategory JMShim var a' b'
            -> KindFunction (f a') (g b')
        fromAp CovarianceType jmf' jma' =
            case inKind @_ @f of
                MkFunctionKindWitness ->
                    case inKind @_ @g of
                        MkFunctionKindWitness ->
                            case fromEnhanced jmf' of
                                MkNestedMorphism ff -> cfmap (fromEnhanced jma') <.> ff
        fromAp ContravarianceType jmf' (MkCatDual jma') =
            case inKind @_ @f of
                MkFunctionKindWitness ->
                    case inKind @_ @g of
                        MkFunctionKindWitness ->
                            case fromEnhanced jmf' of
                                MkNestedMorphism ff -> cfmap (MkCatDual (fromEnhanced jma')) <.> ff
        fromAp RangevarianceType jmf' (MkCatRange jma1 jma2) =
            case inKind @_ @f of
                MkFunctionKindWitness ->
                    case inKind @_ @g of
                        MkFunctionKindWitness ->
                            case fromEnhanced jmf' of
                                MkNestedMorphism ff -> cfmap (MkCatRange (fromEnhanced jma1) (fromEnhanced jma2)) <.> ff
        in fromAp vt jmf jma
    fromEnhanced (LiftJMShim _ _ fg) =
        case fromEnhanced fg of
            MkNestedMorphism ff -> ff
    coercionEnhanced = CoerceJMShim
    enhancedCoercion IdentityJMShim = Just cid
    enhancedCoercion (CoerceJMShim _ c) = Just c
    enhancedCoercion (ApJMShim CovarianceType (Just Dict) _ f a) = do
        cf <- enhancedCoercion f
        ca <- enhancedCoercion a
        return $ applyCoercion1 cf ca
    enhancedCoercion (ApJMShim CovarianceType _ (Just Dict) f a) = do
        cf <- enhancedCoercion f
        ca <- enhancedCoercion a
        return $ applyCoercion2 cf ca
    enhancedCoercion (ApJMShim ContravarianceType (Just Dict) _ f (MkCatDual a)) = do
        cf <- enhancedCoercion f
        ca <- enhancedCoercion a
        return $ applyCoercion1 cf $ invert ca
    enhancedCoercion (ApJMShim ContravarianceType _ (Just Dict) f (MkCatDual a)) = do
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

instance EnhancedPolyShim JMShim

instance Shim JMShim where
    funcShim ab pq = apShimFuncR CovarianceType (apShimFuncR ContravarianceType cid (MkCatDual ab)) pq
    pairShim ab pq = apShimFuncR CovarianceType (apShimFuncR CovarianceType cid ab) pq
    eitherShim ab pq = apShimFuncR CovarianceType (apShimFuncR CovarianceType cid ab) pq
