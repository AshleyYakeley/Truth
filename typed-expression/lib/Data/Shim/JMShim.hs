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
    ApplFJMShim :: JMShim t1 (a -> b) -> JMShim t2 a -> JMShim (MeetType t1 t2) b
    ConsJMShim
        :: forall (v :: Variance) k (f :: VarianceKind v -> k) (g :: VarianceKind v -> k) (a :: VarianceKind v) (b :: VarianceKind v).
           (InKind a, InKind b, HasVariance v f, HasVariance v g)
        => VarianceType v
        -> JMShim f g
        -> VarianceCategory JMShim v a b
        -> JMShim (f a) (g b)

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
    show (ApplFJMShim s1 s2) = "(applf " <> show s1 <> " " <> show s2 <> ")"
    show (ConsJMShim vt s1 s2) = "(" <> show vt <> " " <> show s1 <> " " <> varianceCategoryShow @JMShim vt s2 <> ")"

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
    ConsJMShim pvt pf pa <.> ConsJMShim qvt qf qa
        | Just Refl <- testEquality pvt qvt =
            case varianceInCategory @JMShim pvt of
                Dict -> consShimFunc pvt (pf <.> qf) (pa <.> qa)
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
    p <.> q = FuncJMShim (show p <> " . " <> show q) $ fromEnhanced p <.> fromEnhanced q

instance Category (JMShim :: Type -> Type -> Type) where
    id = cid
    (.) = (<.>)

instance ConPolyShim JMShim where
    consShimFunc CovarianceType IdentityJMShim IdentityJMShim = IdentityJMShim
    consShimFunc ContravarianceType IdentityJMShim (MkCatDual IdentityJMShim) = IdentityJMShim
    consShimFunc RangevarianceType IdentityJMShim (MkCatRange IdentityJMShim IdentityJMShim) = IdentityJMShim
    consShimFunc vt jmf jma = ConsJMShim vt jmf jma

newtype JMIsoShim (a :: k) (b :: k) = MkJMIsoShim
    { unJMIsoShim :: Isomorphism JMShim a b
    }

instance CoercibleKind k => InCategory (JMIsoShim :: k -> k -> Type) where
    cid = MkJMIsoShim cid
    MkJMIsoShim p <.> MkJMIsoShim q = MkJMIsoShim $ p <.> q

instance Category (JMIsoShim :: Type -> Type -> Type) where
    id = cid
    (.) = (<.>)

instance ConPolyShim JMIsoShim where
    consShimFunc CovarianceType (MkJMIsoShim (MkIsomorphism fab fba)) (MkJMIsoShim (MkIsomorphism xab xba)) =
        MkJMIsoShim $ MkIsomorphism (consShimFunc CovarianceType fab xab) (consShimFunc CovarianceType fba xba)
    consShimFunc ContravarianceType (MkJMIsoShim (MkIsomorphism fab fba)) (MkCatDual (MkJMIsoShim (MkIsomorphism xab xba))) =
        MkJMIsoShim $
        MkIsomorphism
            (consShimFunc ContravarianceType fab $ MkCatDual xab)
            (consShimFunc ContravarianceType fba $ MkCatDual xba)
    consShimFunc RangevarianceType (MkJMIsoShim (MkIsomorphism fab fba)) (MkCatRange (MkJMIsoShim (MkIsomorphism xab1 xba1)) (MkJMIsoShim (MkIsomorphism xab2 xba2))) =
        MkJMIsoShim $
        MkIsomorphism
            (consShimFunc RangevarianceType fab (MkCatRange xab1 xab2))
            (consShimFunc RangevarianceType fba (MkCatRange xba1 xba2))

instance HasVariance 'Covariance f => CatFunctor JMShim JMShim f where
    cfmap f = consShimFunc CovarianceType IdentityJMShim f

instance HasVariance 'Contravariance f => CatFunctor (CatDual JMShim) JMShim f where
    cfmap jmfa = consShimFunc ContravarianceType IdentityJMShim jmfa

instance HasVariance 'Rangevariance f => CatFunctor (CatRange JMShim) JMShim (f :: (Type, Type) -> kq) where
    cfmap ::
           forall a b. (InKind a, InKind b)
        => CatRange JMShim a b
        -> JMShim (f a) (f b)
    cfmap (MkCatRange IdentityJMShim IdentityJMShim) = IdentityJMShim
    cfmap jmf =
        case (inKind @_ @a, inKind @_ @b) of
            (MkPairWitness, MkPairWitness) -> consShimFunc RangevarianceType IdentityJMShim jmf

instance JoinMeetCategory JMShim where
    initf = InitFJMShim
    termf = TermFJMShim
    join1 = Join1JMShim cid
    join2 = Join2JMShim cid
    joinf = JoinFJMShim
    meet1 = Meet1JMShim cid
    meet2 = Meet2JMShim cid
    meetf = MeetFJMShim
    applf (Meet1JMShim p) q = applf p q <.> meetBimap meet1 cid
    applf (ConsJMShim CovarianceType IdentityJMShim p) q = p <.> applf cid q
    applf (ConsJMShim CovarianceType (ConsJMShim ContravarianceType IdentityJMShim (MkCatDual pa)) pb) q =
        pb <.> applf cid (pa <.> q)
    applf p q = ApplFJMShim p q

varrep1 ::
       forall v shim f g. HasVariance v f
    => shim f g
    -> Maybe (Dict (RepresentationalRole f))
varrep1 _ = varianceRepresentational @_ @v @f

varrep2 ::
       forall v shim f g. HasVariance v g
    => shim f g
    -> Maybe (Dict (RepresentationalRole g))
varrep2 _ = varianceRepresentational @_ @v @g

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
    fromEnhanced (ApplFJMShim pa pb) = applf (fromEnhanced pa) (fromEnhanced pb)
    fromEnhanced (ConsJMShim vt jmf jma) = let
        fromCon ::
               forall (v :: Variance) (f :: VarianceKind v -> k) (g :: VarianceKind v -> k) (a' :: VarianceKind v) (b' :: VarianceKind v).
               ( InKind f
               , InKind g
               , InKind a'
               , InKind b'
               , CatFunctor (VarianceCategory KindFunction v) KindFunction f
               , CatFunctor (VarianceCategory KindFunction v) KindFunction g
               )
            => VarianceType v
            -> JMShim f g
            -> VarianceCategory JMShim v a' b'
            -> KindFunction (f a') (g b')
        fromCon CovarianceType jmf' jma' =
            case inKind @_ @f of
                MkFunctionKindWitness ->
                    case inKind @_ @g of
                        MkFunctionKindWitness ->
                            case fromEnhanced jmf' of
                                MkNestedMorphism ff -> cfmap (fromEnhanced jma') <.> ff
        fromCon ContravarianceType jmf' (MkCatDual jma') =
            case inKind @_ @f of
                MkFunctionKindWitness ->
                    case inKind @_ @g of
                        MkFunctionKindWitness ->
                            case fromEnhanced jmf' of
                                MkNestedMorphism ff -> cfmap (MkCatDual (fromEnhanced jma')) <.> ff
        fromCon RangevarianceType jmf' (MkCatRange jma1 jma2) =
            case inKind @_ @f of
                MkFunctionKindWitness ->
                    case inKind @_ @g of
                        MkFunctionKindWitness ->
                            case fromEnhanced jmf' of
                                MkNestedMorphism ff -> cfmap (MkCatRange (fromEnhanced jma1) (fromEnhanced jma2)) <.> ff
        in fromCon vt jmf jma
    coercionEnhanced = CoerceJMShim
    enhancedCoercion IdentityJMShim = Just cid
    enhancedCoercion (CoerceJMShim _ c) = Just c
    enhancedCoercion (ConsJMShim CovarianceType f a)
        | Just Dict <- varrep1 @'Covariance f = do
            cf <- enhancedCoercion f
            ca <- enhancedCoercion a
            return $ applyCoercion1 cf ca
    enhancedCoercion (ConsJMShim CovarianceType f a)
        | Just Dict <- varrep2 @'Covariance f = do
            cf <- enhancedCoercion f
            ca <- enhancedCoercion a
            return $ applyCoercion2 cf ca
    enhancedCoercion (ConsJMShim ContravarianceType f (MkCatDual a))
        | Just Dict <- varrep1 @'Contravariance f = do
            cf <- enhancedCoercion f
            ca <- enhancedCoercion a
            return $ applyCoercion1 cf $ invert ca
    enhancedCoercion (ConsJMShim ContravarianceType f (MkCatDual a))
        | Just Dict <- varrep2 @'Contravariance f = do
            cf <- enhancedCoercion f
            ca <- enhancedCoercion a
            return $ applyCoercion2 cf $ invert ca
    enhancedCoercion _ = Nothing

instance Shim JMShim where
    funcShim ab pq = consShimFunc CovarianceType (consShimFunc ContravarianceType cid (MkCatDual ab)) pq
    pairShim ab pq = consShimFunc CovarianceType (consShimFunc CovarianceType cid ab) pq
    eitherShim ab pq = consShimFunc CovarianceType (consShimFunc CovarianceType cid ab) pq
    shimExtractFunction :: JMShim a (b -> c) -> (forall c'. JMShim a (b -> c') -> JMShim c' c -> r) -> r
    shimExtractFunction (ConsJMShim CovarianceType fg cc) call = let
        abc' = consShimFunc CovarianceType fg id
        in call abc' cc
    shimExtractFunction abc call = call abc cid
