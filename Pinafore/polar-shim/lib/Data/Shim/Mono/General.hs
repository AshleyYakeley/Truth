module Data.Shim.Mono.General where

import Data.Shim.Polar.JoinMeet
import Shapes

type ShimKind k = k -> k -> Type

class Category shim => JoinMeetIsoShim (shim :: ShimKind Type) where
    iJoinL1 :: shim (JoinType a BottomType) a
    default iJoinL1 :: JoinMeetShim shim => shim (JoinType a BottomType) a
    iJoinL1 = joinf id initf
    iJoinL2 :: shim (JoinType BottomType a) a
    default iJoinL2 :: JoinMeetShim shim => shim (JoinType BottomType a) a
    iJoinL2 = joinf initf id
    iJoinR1 :: shim a (JoinType a BottomType)
    default iJoinR1 :: JoinMeetShim shim => shim a (JoinType a BottomType)
    iJoinR1 = join1
    iJoinR2 :: shim a (JoinType BottomType a)
    default iJoinR2 :: JoinMeetShim shim => shim a (JoinType BottomType a)
    iJoinR2 = join2
    iJoinPair :: shim a1 a2 -> shim b1 b2 -> shim (JoinType a1 b1) (JoinType a2 b2)
    default iJoinPair :: JoinMeetShim shim => shim a1 a2 -> shim b1 b2 -> shim (JoinType a1 b1) (JoinType a2 b2)
    iJoinPair aa bb = joinf (join1 . aa) (join2 . bb)
    iJoinSwap :: shim (JoinType a b) (JoinType b a)
    default iJoinSwap :: JoinMeetShim shim => shim (JoinType a b) (JoinType b a)
    iJoinSwap = joinf join2 join1
    iJoinSwapL :: shim (JoinType (JoinType a b) c) (JoinType a (JoinType b c))
    default iJoinSwapL :: JoinMeetShim shim => shim (JoinType (JoinType a b) c) (JoinType a (JoinType b c))
    iJoinSwapL = joinf (joinf join1 (join2 . join1)) (join2 . join2)
    iJoinSwapR :: shim (JoinType a (JoinType b c)) (JoinType (JoinType a b) c)
    default iJoinSwapR :: JoinMeetShim shim => shim (JoinType a (JoinType b c)) (JoinType (JoinType a b) c)
    iJoinSwapR = joinf (join1 . join1) (joinf (join1 . join2) join2)
    iMeetL1 :: shim (MeetType a TopType) a
    default iMeetL1 :: JoinMeetShim shim => shim (MeetType a TopType) a
    iMeetL1 = meet1
    iMeetL2 :: shim (MeetType TopType a) a
    default iMeetL2 :: JoinMeetShim shim => shim (MeetType TopType a) a
    iMeetL2 = meet2
    iMeetR1 :: shim a (MeetType a TopType)
    default iMeetR1 :: JoinMeetShim shim => shim a (MeetType a TopType)
    iMeetR1 = meetf id termf
    iMeetR2 :: shim a (MeetType TopType a)
    default iMeetR2 :: JoinMeetShim shim => shim a (MeetType TopType a)
    iMeetR2 = meetf termf id
    iMeetPair :: shim a1 a2 -> shim b1 b2 -> shim (MeetType a1 b1) (MeetType a2 b2)
    default iMeetPair :: JoinMeetShim shim => shim a1 a2 -> shim b1 b2 -> shim (MeetType a1 b1) (MeetType a2 b2)
    iMeetPair aa bb = meetf (aa . meet1) (bb . meet2)
    iMeetSwap :: shim (MeetType a b) (MeetType b a)
    default iMeetSwap :: JoinMeetShim shim => shim (MeetType a b) (MeetType b a)
    iMeetSwap = meetf meet2 meet1
    iMeetSwapL :: shim (MeetType (MeetType a b) c) (MeetType a (MeetType b c))
    default iMeetSwapL :: JoinMeetShim shim => shim (MeetType (MeetType a b) c) (MeetType a (MeetType b c))
    iMeetSwapL = meetf (meet1 . meet1) (meetf (meet2 . meet1) meet2)
    iMeetSwapR :: shim (MeetType a (MeetType b c)) (MeetType (MeetType a b) c)
    default iMeetSwapR :: JoinMeetShim shim => shim (MeetType a (MeetType b c)) (MeetType (MeetType a b) c)
    iMeetSwapR = meetf (meetf meet1 (meet1 . meet2)) (meet2 . meet2)

instance JoinMeetIsoShim shim => JoinMeetIsoShim (CatDual shim) where
    iJoinL1 = MkCatDual iJoinR1
    iJoinL2 = MkCatDual iJoinR2
    iJoinR1 = MkCatDual iJoinL1
    iJoinR2 = MkCatDual iJoinL2
    iJoinPair (MkCatDual a2a1) (MkCatDual b2b1) = MkCatDual (iJoinPair a2a1 b2b1)
    iJoinSwap = MkCatDual iJoinSwap
    iJoinSwapL = MkCatDual iJoinSwapR
    iJoinSwapR = MkCatDual iJoinSwapL
    iMeetL1 = MkCatDual iMeetR1
    iMeetL2 = MkCatDual iMeetR2
    iMeetR1 = MkCatDual iMeetL1
    iMeetR2 = MkCatDual iMeetL2
    iMeetPair (MkCatDual a2a1) (MkCatDual b2b1) = MkCatDual (iMeetPair a2a1 b2b1)
    iMeetSwap = MkCatDual iMeetSwap
    iMeetSwapL = MkCatDual iMeetSwapR
    iMeetSwapR = MkCatDual iMeetSwapL

instance JoinMeetIsoShim shim => JoinMeetIsoShim (Isomorphism shim) where
    iJoinL1 = MkIsomorphism iJoinL1 iJoinR1
    iJoinL2 = MkIsomorphism iJoinL2 iJoinR2
    iJoinR1 = MkIsomorphism iJoinR1 iJoinL1
    iJoinR2 = MkIsomorphism iJoinR2 iJoinL2
    iJoinPair (MkIsomorphism a1a2 a2a1) (MkIsomorphism b1b2 b2b1) =
        MkIsomorphism (iJoinPair a1a2 b1b2) (iJoinPair a2a1 b2b1)
    iJoinSwap = MkIsomorphism iJoinSwap iJoinSwap
    iJoinSwapL = MkIsomorphism iJoinSwapL iJoinSwapR
    iJoinSwapR = MkIsomorphism iJoinSwapR iJoinSwapL
    iMeetL1 = MkIsomorphism iMeetL1 iMeetR1
    iMeetL2 = MkIsomorphism iMeetL2 iMeetR2
    iMeetR1 = MkIsomorphism iMeetR1 iMeetL1
    iMeetR2 = MkIsomorphism iMeetR2 iMeetL2
    iMeetPair (MkIsomorphism a1a2 a2a1) (MkIsomorphism b1b2 b2b1) =
        MkIsomorphism (iMeetPair a1a2 b1b2) (iMeetPair a2a1 b2b1)
    iMeetSwap = MkIsomorphism iMeetSwap iMeetSwap
    iMeetSwapL = MkIsomorphism iMeetSwapL iMeetSwapR
    iMeetSwapR = MkIsomorphism iMeetSwapR iMeetSwapL

class JoinMeetIsoShim shim => JoinMeetShim (shim :: ShimKind Type) where
    initf :: shim BottomType a
    termf :: shim a TopType
    join1 :: shim a (JoinType a b)
    join2 :: shim b (JoinType a b)
    joinf :: shim a r -> shim b r -> shim (JoinType a b) r
    meet1 :: shim (MeetType a b) a
    meet2 :: shim (MeetType a b) b
    meetf :: shim r a -> shim r b -> shim r (MeetType a b)

instance JoinMeetIsoShim (->)

instance JoinMeetShim (->) where
    initf = never
    termf _ = MkTopType
    join1 = LeftJoinType
    join2 = RightJoinType
    joinf f _ (LeftJoinType v) = f v
    joinf _ f (RightJoinType v) = f v
    meet1 (BothMeetType v _) = v
    meet2 (BothMeetType _ v) = v
    meetf f1 f2 v = BothMeetType (f1 v) (f2 v)

class (CoercibleKind k, Category shim) => IsoMapShim (shim :: ShimKind k) where
    isoMapShim ::
           String
        -> (KindFunction pa pb -> KindFunction qa qb)
        -> (KindFunction pb pa -> KindFunction qb qa)
        -> shim pa pb
        -> shim qa qb
    default isoMapShim ::
        RecoverShim shim =>
                String -> (KindFunction pa pb -> KindFunction qa qb) -> (KindFunction pb pa -> KindFunction qb qa) -> shim pa pb -> shim qa qb
    isoMapShim t f _ pp = functionToShim t $ f $ shimToFunction pp

isoFunctionToShim ::
       forall k (shim :: ShimKind k) (a :: k) (b :: k). FunctionShim shim
    => String
    -> Isomorphism KindFunction a b
    -> Isomorphism shim a b
isoFunctionToShim s (MkIsomorphism ab ba) = MkIsomorphism (functionToShim s ab) (functionToShim s ba)

isoShimToFunction ::
       forall k (shim :: ShimKind k) (a :: k) (b :: k). RecoverShim shim
    => Isomorphism shim a b
    -> Isomorphism KindFunction a b
isoShimToFunction (MkIsomorphism ab ba) = MkIsomorphism (shimToFunction ab) (shimToFunction ba)

class IsoMapShim shim => CoerceShim (shim :: ShimKind k) where
    coercionToShim :: String -> Coercion a b -> shim a b
    shimToCoercion :: shim a b -> Maybe (Coercion a b)

coerceShim ::
       forall k (shim :: ShimKind k) (a :: k) (b :: k). (CoerceShim shim, Coercible a b)
    => String
    -> shim a b
coerceShim t = coercionToShim t MkCoercion

class CoerceShim shim => FunctionShim (shim :: ShimKind k) where
    functionToShim :: String -> KindFunction a b -> shim a b

instance IsoMapShim (->)

instance CoerceShim (->) where
    coercionToShim _ MkCoercion = coerce
    shimToCoercion _ = Nothing

instance FunctionShim (->) where
    functionToShim _ = id

class FunctionShim shim => RecoverShim (shim :: ShimKind k) where
    shimToFunction :: shim a b -> KindFunction a b

instance RecoverShim (->) where
    shimToFunction = id

class Category shim => CartesianShim (shim :: ShimKind Type) where
    funcShim :: forall a b p q. shim a b -> shim p q -> shim (b -> p) (a -> q)
    pairShim :: forall a b p q. shim a b -> shim p q -> shim (a, p) (b, q)
    eitherShim :: forall a b p q. shim a b -> shim p q -> shim (Either a p) (Either b q)
    shimExtractFunction :: shim a (b -> c) -> (forall c'. shim a (b -> c') -> shim c' c -> r) -> r
    shimExtractFunction abc call = call abc id

instance CartesianShim (:~:) where
    funcShim Refl Refl = Refl
    pairShim Refl Refl = Refl
    eitherShim Refl Refl = Refl

instance CartesianShim (->) where
    funcShim ab pq bp = pq . bp . ab
    pairShim ab pq (a, p) = (ab a, pq p)
    eitherShim ab _ (Left a) = Left $ ab a
    eitherShim _ pq (Right p) = Right $ pq p

type LazyCategory :: ShimKind Type -> Constraint
class JoinMeetIsoShim shim => LazyCategory shim where
    iLazy :: forall a b. shim a b -> shim a b
    default iLazy :: forall a b. RecoverShim shim => shim a b -> shim a b
    iLazy sab = functionToShim "recursive" $ shimToFunction sab

instance LazyCategory (->)

instance LazyCategory shim => LazyCategory (CatDual shim) where
    iLazy ~(MkCatDual ba) = MkCatDual $ iLazy ba

instance LazyCategory shim => LazyCategory (Isomorphism shim) where
    iLazy ~(MkIsomorphism ab ba) = MkIsomorphism (iLazy ab) (iLazy ba)
