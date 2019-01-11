module Language.Expression.Dolan.JoinMeet where

import Language.Expression.Polarity
import Shapes

data TopType =
    MkTopType

alwaysTop :: a -> TopType
alwaysTop _ = MkTopType

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

join1 :: a -> JoinType a b
join1 = LeftJoinType

join2 :: b -> JoinType a b
join2 = RightJoinType

joinf :: (a -> r) -> (b -> r) -> JoinType a b -> r
joinf f _ (LeftJoinType v) = f v
joinf _ f (RightJoinType v) = f v

unjoin1 :: JoinType a BottomType -> a
unjoin1 (MkJoinType (Left a)) = a
unjoin1 (MkJoinType (Right n)) = never n

unjoin2 :: JoinType BottomType a -> a
unjoin2 (MkJoinType (Left n)) = never n
unjoin2 (MkJoinType (Right a)) = a

joinBimap :: (a1 -> a2) -> (b1 -> b2) -> JoinType a1 b1 -> JoinType a2 b2
joinBimap f _ (LeftJoinType v) = LeftJoinType $ f v
joinBimap _ f (RightJoinType v) = RightJoinType $ f v

biJoinBimap :: Bijection a1 a2 -> Bijection b1 b2 -> Bijection (JoinType a1 b1) (JoinType a2 b2)
biJoinBimap (MkBijection a1a2 a2a1) (MkBijection b1b2 b2b1) = MkBijection (joinBimap a1a2 b1b2) (joinBimap a2a1 b2b1)

newtype MeetType a b =
    MkMeetType (a, b)

meet1 :: MeetType a b -> a
meet1 (MkMeetType (v, _)) = v

meet2 :: MeetType a b -> b
meet2 (MkMeetType (_, v)) = v

unmeet1 :: a -> MeetType a TopType
unmeet1 a = MkMeetType (a, MkTopType)

unmeet2 :: a -> MeetType TopType a
unmeet2 a = MkMeetType (MkTopType, a)

meetf :: (r -> a) -> (r -> b) -> r -> MeetType a b
meetf f1 f2 v = MkMeetType (f1 v, f2 v)

meetBimap :: (a1 -> a2) -> (b1 -> b2) -> MeetType a1 b1 -> MeetType a2 b2
meetBimap aa bb (MkMeetType (a, b)) = MkMeetType (aa a, bb b)

biMeetBimap :: Bijection a1 a2 -> Bijection b1 b2 -> Bijection (MeetType a1 b1) (MeetType a2 b2)
biMeetBimap (MkBijection a1a2 a2a1) (MkBijection b1b2 b2b1) = MkBijection (meetBimap a1a2 b1b2) (meetBimap a2a1 b2b1)

instance Eq a => Eq (MeetType a b) where
    (MkMeetType (a1, _)) == (MkMeetType (a2, _)) = a1 == a2

type family LimitType polarity :: Type where
    LimitType 'Positive = BottomType
    LimitType 'Negative = TopType

type family JoinMeetType polarity :: Type -> Type -> Type where
    JoinMeetType 'Positive = JoinType
    JoinMeetType 'Negative = MeetType
