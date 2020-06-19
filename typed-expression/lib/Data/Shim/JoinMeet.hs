module Data.Shim.JoinMeet where

import Shapes

data TopType =
    MkTopType

newtype BottomType =
    MkBottomType None
    deriving (Eq, Searchable, Countable, Empty)

instance Finite BottomType where
    allValues = []
    assemble _ = pure never

newtype JoinType a b =
    MkJoinType (Either a b)

pattern LeftJoinType :: a -> JoinType a b

pattern LeftJoinType v = MkJoinType (Left v)

pattern RightJoinType :: b -> JoinType a b

pattern RightJoinType v = MkJoinType (Right v)

{-# COMPLETE LeftJoinType, RightJoinType #-}

newtype MeetType a b =
    MkMeetType (a, b)

pattern BothMeetType :: a -> b -> MeetType a b

pattern BothMeetType a b = MkMeetType (a, b)

{-# COMPLETE BothMeetType #-}

class InCategory shim => JoinMeetIsoCategory (shim :: Type -> Type -> Type) where
    iJoinL1 :: shim (JoinType a BottomType) a
    default iJoinL1 :: JoinMeetCategory shim => shim (JoinType a BottomType) a
    iJoinL1 = joinf cid initf
    iJoinL2 :: shim (JoinType BottomType a) a
    default iJoinL2 :: JoinMeetCategory shim => shim (JoinType BottomType a) a
    iJoinL2 = joinf initf cid
    iJoinR1 :: shim a (JoinType a BottomType)
    default iJoinR1 :: JoinMeetCategory shim => shim a (JoinType a BottomType)
    iJoinR1 = join1
    iJoinR2 :: shim a (JoinType BottomType a)
    default iJoinR2 :: JoinMeetCategory shim => shim a (JoinType BottomType a)
    iJoinR2 = join2
    iJoinPair :: shim a1 a2 -> shim b1 b2 -> shim (JoinType a1 b1) (JoinType a2 b2)
    default iJoinPair :: JoinMeetCategory shim => shim a1 a2 -> shim b1 b2 -> shim (JoinType a1 b1) (JoinType a2 b2)
    iJoinPair aa bb = joinf (join1 <.> aa) (join2 <.> bb)
    iJoinSwapL :: shim (JoinType (JoinType a b) c) (JoinType a (JoinType b c))
    default iJoinSwapL :: JoinMeetCategory shim => shim (JoinType (JoinType a b) c) (JoinType a (JoinType b c))
    iJoinSwapL = joinf (joinf join1 (join2 <.> join1)) (join2 <.> join2)
    iJoinSwapR :: shim (JoinType a (JoinType b c)) (JoinType (JoinType a b) c)
    default iJoinSwapR :: JoinMeetCategory shim => shim (JoinType a (JoinType b c)) (JoinType (JoinType a b) c)
    iJoinSwapR = joinf (join1 <.> join1) (joinf (join1 <.> join2) join2)
    iMeetL1 :: shim (MeetType a TopType) a
    default iMeetL1 :: JoinMeetCategory shim => shim (MeetType a TopType) a
    iMeetL1 = meet1
    iMeetL2 :: shim (MeetType TopType a) a
    default iMeetL2 :: JoinMeetCategory shim => shim (MeetType TopType a) a
    iMeetL2 = meet2
    iMeetR1 :: shim a (MeetType a TopType)
    default iMeetR1 :: JoinMeetCategory shim => shim a (MeetType a TopType)
    iMeetR1 = meetf cid termf
    iMeetR2 :: shim a (MeetType TopType a)
    default iMeetR2 :: JoinMeetCategory shim => shim a (MeetType TopType a)
    iMeetR2 = meetf termf cid
    iMeetPair :: shim a1 a2 -> shim b1 b2 -> shim (MeetType a1 b1) (MeetType a2 b2)
    default iMeetPair :: JoinMeetCategory shim => shim a1 a2 -> shim b1 b2 -> shim (MeetType a1 b1) (MeetType a2 b2)
    iMeetPair aa bb = meetf (aa <.> meet1) (bb <.> meet2)
    iMeetSwapL :: shim (MeetType (MeetType a b) c) (MeetType a (MeetType b c))
    default iMeetSwapL :: JoinMeetCategory shim => shim (MeetType (MeetType a b) c) (MeetType a (MeetType b c))
    iMeetSwapL = meetf (meet1 <.> meet1) (meetf (meet2 <.> meet1) meet2)
    iMeetSwapR :: shim (MeetType a (MeetType b c)) (MeetType (MeetType a b) c)
    default iMeetSwapR :: JoinMeetCategory shim => shim (MeetType a (MeetType b c)) (MeetType (MeetType a b) c)
    iMeetSwapR = meetf (meetf meet1 (meet1 <.> meet2)) (meet2 <.> meet2)

instance JoinMeetIsoCategory shim => JoinMeetIsoCategory (Isomorphism shim) where
    iJoinL1 = MkIsomorphism iJoinL1 iJoinR1
    iJoinL2 = MkIsomorphism iJoinL2 iJoinR2
    iJoinR1 = MkIsomorphism iJoinR1 iJoinL1
    iJoinR2 = MkIsomorphism iJoinR2 iJoinL2
    iJoinPair (MkIsomorphism a1a2 a2a1) (MkIsomorphism b1b2 b2b1) =
        MkIsomorphism (iJoinPair a1a2 b1b2) (iJoinPair a2a1 b2b1)
    iJoinSwapL = MkIsomorphism iJoinSwapL iJoinSwapR
    iJoinSwapR = MkIsomorphism iJoinSwapR iJoinSwapL
    iMeetL1 = MkIsomorphism iMeetL1 iMeetR1
    iMeetL2 = MkIsomorphism iMeetL2 iMeetR2
    iMeetR1 = MkIsomorphism iMeetR1 iMeetL1
    iMeetR2 = MkIsomorphism iMeetR2 iMeetL2
    iMeetPair (MkIsomorphism a1a2 a2a1) (MkIsomorphism b1b2 b2b1) =
        MkIsomorphism (iMeetPair a1a2 b1b2) (iMeetPair a2a1 b2b1)
    iMeetSwapL = MkIsomorphism iMeetSwapL iMeetSwapR
    iMeetSwapR = MkIsomorphism iMeetSwapR iMeetSwapL

class JoinMeetIsoCategory shim => JoinMeetCategory (shim :: Type -> Type -> Type) where
    initf :: shim BottomType a
    termf :: shim a TopType
    join1 :: shim a (JoinType a b)
    join2 :: shim b (JoinType a b)
    joinf :: shim a r -> shim b r -> shim (JoinType a b) r
    meet1 :: shim (MeetType a b) a
    meet2 :: shim (MeetType a b) b
    meetf :: shim r a -> shim r b -> shim r (MeetType a b)
    applf :: shim r1 (a -> b) -> shim r2 a -> shim (MeetType r1 r2) b

instance JoinMeetIsoCategory (->)

instance JoinMeetCategory (->) where
    initf = never
    termf _ = MkTopType
    join1 = LeftJoinType
    join2 = RightJoinType
    joinf f _ (LeftJoinType v) = f v
    joinf _ f (RightJoinType v) = f v
    meet1 (BothMeetType v _) = v
    meet2 (BothMeetType _ v) = v
    meetf f1 f2 v = BothMeetType (f1 v) (f2 v)
    applf rab ra (BothMeetType r1 r2) = rab r1 (ra r2)

class (CoercibleKind k, InCategory shim) => EnhancedFunction (shim :: k -> k -> Type) where
    toEnhanced :: (InKind a, InKind b) => String -> KindFunction a b -> shim a b
    fromEnhanced :: (InKind a, InKind b) => shim a b -> KindFunction a b
    coercionEnhanced :: (InKind a, InKind b) => String -> Coercion a b -> shim a b
    enhancedCoercion :: (InKind a, InKind b) => shim a b -> Maybe (Coercion a b)

coerceEnhanced :: (EnhancedFunction shim, InKind a, InKind b, Coercible a b) => String -> shim a b
coerceEnhanced t = coercionEnhanced t MkCoercion

instance EnhancedFunction (->) where
    toEnhanced _ = id
    fromEnhanced = id
    coercionEnhanced _ MkCoercion = coerce
    enhancedCoercion _ = Nothing

instance Eq a => Eq (MeetType a b) where
    BothMeetType a1 _ == BothMeetType a2 _ = a1 == a2

class (InCategory shim, JoinMeetCategory shim, EnhancedFunction shim) => Shim (shim :: Type -> Type -> Type) where
    funcShim :: forall a b p q. shim a b -> shim p q -> shim (b -> p) (a -> q)
    pairShim :: forall a b p q. shim a b -> shim p q -> shim (a, p) (b, q)
    eitherShim :: forall a b p q. shim a b -> shim p q -> shim (Either a p) (Either b q)
    shimExtractFunction :: shim a (b -> c) -> (forall c'. shim a (b -> c') -> shim c' c -> r) -> r
    shimExtractFunction abc call = call abc cid

instance Shim (->) where
    funcShim ab pq bp = pq . bp . ab
    pairShim ab pq (a, p) = (ab a, pq p)
    eitherShim ab _ (Left a) = Left $ ab a
    eitherShim _ pq (Right p) = Right $ pq p
