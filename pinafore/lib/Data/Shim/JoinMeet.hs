module Data.Shim.JoinMeet where

import Data.Shim.Polarity
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

class InCategory cat => JoinMeetCategory cat where
    initf :: cat BottomType a
    termf :: cat a TopType
    join1 :: cat a (JoinType a b)
    join2 :: cat b (JoinType a b)
    joinf :: cat a r -> cat b r -> cat (JoinType a b) r
    meet1 :: cat (MeetType a b) a
    meet2 :: cat (MeetType a b) b
    meetf :: cat r a -> cat r b -> cat r (MeetType a b)
    applf :: cat r1 (a -> b) -> cat r2 a -> cat (MeetType r1 r2) b

unjoin1 :: JoinMeetCategory cat => cat (JoinType a BottomType) a
unjoin1 = joinf cid initf

unjoin2 :: JoinMeetCategory cat => cat (JoinType BottomType a) a
unjoin2 = joinf initf cid

joinBimap :: JoinMeetCategory cat => cat a1 a2 -> cat b1 b2 -> cat (JoinType a1 b1) (JoinType a2 b2)
joinBimap aa bb = joinf (join1 <.> aa) (join2 <.> bb)

unmeet1 :: JoinMeetCategory cat => cat a (MeetType a TopType)
unmeet1 = meetf cid termf

unmeet2 :: JoinMeetCategory cat => cat a (MeetType TopType a)
unmeet2 = meetf termf cid

meetBimap :: JoinMeetCategory cat => cat a1 a2 -> cat b1 b2 -> cat (MeetType a1 b1) (MeetType a2 b2)
meetBimap aa bb = meetf (aa <.> meet1) (bb <.> meet2)

swapJoinRight :: JoinMeetCategory cat => cat (JoinType a (JoinType b c)) (JoinType (JoinType a b) c)
swapJoinRight = joinf (join1 <.> join1) (joinf (join1 <.> join2) join2)

swapMeetRight :: JoinMeetCategory cat => cat (MeetType (MeetType a b) c) (MeetType a (MeetType b c))
swapMeetRight = meetf (meet1 <.> meet1) (meetf (meet2 <.> meet1) meet2)

bijoin1 :: JoinMeetCategory cat => Isomorphism cat (JoinType a BottomType) a
bijoin1 = MkIsomorphism unjoin1 join1

biJoinBimap ::
       JoinMeetCategory cat
    => Isomorphism cat a1 a2
    -> Isomorphism cat b1 b2
    -> Isomorphism cat (JoinType a1 b1) (JoinType a2 b2)
biJoinBimap (MkIsomorphism a1a2 a2a1) (MkIsomorphism b1b2 b2b1) =
    MkIsomorphism (joinBimap a1a2 b1b2) (joinBimap a2a1 b2b1)

bimeet1 :: JoinMeetCategory cat => Isomorphism cat a (MeetType a TopType)
bimeet1 = MkIsomorphism unmeet1 meet1

biMeetBimap ::
       JoinMeetCategory cat
    => Isomorphism cat a1 a2
    -> Isomorphism cat b1 b2
    -> Isomorphism cat (MeetType a1 b1) (MeetType a2 b2)
biMeetBimap (MkIsomorphism a1a2 a2a1) (MkIsomorphism b1b2 b2b1) =
    MkIsomorphism (meetBimap a1a2 b1b2) (meetBimap a2a1 b2b1)

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

class (CoercibleKind k, InCategory cat) => EnhancedFunction (cat :: k -> k -> Type) where
    toEnhanced :: (InKind a, InKind b) => String -> KindFunction a b -> cat a b
    fromEnhanced :: (InKind a, InKind b) => cat a b -> KindFunction a b
    coercionEnhanced :: (InKind a, InKind b) => String -> Coercion a b -> cat a b
    enhancedCoercion :: (InKind a, InKind b) => cat a b -> Maybe (Coercion a b)

coerceEnhanced :: (EnhancedFunction cat, InKind a, InKind b, Coercible a b) => String -> cat a b
coerceEnhanced t = coercionEnhanced t MkCoercion

instance EnhancedFunction (->) where
    toEnhanced _ = id
    fromEnhanced = id
    coercionEnhanced _ MkCoercion = coerce
    enhancedCoercion _ = Nothing

instance Eq a => Eq (MeetType a b) where
    BothMeetType a1 _ == BothMeetType a2 _ = a1 == a2

type family LimitType polarity :: Type where
    LimitType 'Positive = BottomType
    LimitType 'Negative = TopType

type family JoinMeetType polarity :: Type -> Type -> Type where
    JoinMeetType 'Positive = JoinType
    JoinMeetType 'Negative = MeetType

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
