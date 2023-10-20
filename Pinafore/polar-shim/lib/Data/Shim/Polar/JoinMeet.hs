module Data.Shim.Polar.JoinMeet where

import Shapes

data TopType =
    MkTopType
    deriving (Eq)

instance Show TopType where
    show MkTopType = "T"

newtype BottomType =
    MkBottomType Void
    deriving (Eq, Show, Searchable, Countable, Empty)

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

instance Eq a => Eq (MeetType a b) where
    BothMeetType a1 _ == BothMeetType a2 _ = a1 == a2
