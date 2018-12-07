module Language.Expression.Dolan.Polarity where

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

joinBimap :: (a1 -> a2) -> (b1 -> b2) -> JoinType a1 b1 -> JoinType a2 b2
joinBimap f _ (LeftJoinType v) = LeftJoinType $ f v
joinBimap _ f (RightJoinType v) = RightJoinType $ f v

unjoin1 :: JoinType a BottomType -> a
unjoin1 (MkJoinType (Left a)) = a
unjoin1 (MkJoinType (Right n)) = never n

unjoin2 :: JoinType BottomType a -> a
unjoin2 (MkJoinType (Left n)) = never n
unjoin2 (MkJoinType (Right a)) = a

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

instance Eq a => Eq (MeetType a b) where
    (MkMeetType (a1, _)) == (MkMeetType (a2, _)) = a1 == a2

data TypePolarity
    = PositivePolarity
    | NegativePolarity

class IsTypePolarity (polarity :: TypePolarity) where
    whichTypePolarity :: Either (polarity :~: 'PositivePolarity) (polarity :~: 'NegativePolarity)
    type InvertPolarity polarity = (inv :: TypePolarity) | inv -> polarity
    isInvertPolarity :: Dict (IsTypePolarity (InvertPolarity polarity))
    type LimitType polarity :: Type
    showLimitType :: Text
    type JoinMeetType polarity :: Type -> Type -> Type
    showJoinMeetType :: Text
    type ConvertType polarity (a :: k) (b :: k) :: Type
    jmLeftIdentity :: Bijection (JoinMeetType polarity (LimitType polarity) a) a
    jmRightIdentity :: Bijection (JoinMeetType polarity a (LimitType polarity)) a
    jmMap :: (a1 -> a2) -> (b1 -> b2) -> JoinMeetType polarity a1 b1 -> JoinMeetType polarity a2 b2

invertPolarity ::
       forall polarity r. IsTypePolarity polarity
    => (IsTypePolarity (InvertPolarity polarity) => r)
    -> r
invertPolarity v =
    case isInvertPolarity @polarity of
        Dict -> v

jmBiMap ::
       forall polarity a1 a2 b1 b2. IsTypePolarity polarity
    => Bijection a1 a2
    -> Bijection b1 b2
    -> Bijection (JoinMeetType polarity a1 b1) (JoinMeetType polarity a2 b2)
jmBiMap (MkBijection a1a2 a2a1) (MkBijection b1b2 b2b1) =
    MkBijection (jmMap @polarity a1a2 b1b2) (jmMap @polarity a2a1 b2b1)

instance IsTypePolarity 'PositivePolarity where
    whichTypePolarity = Left Refl
    type InvertPolarity 'PositivePolarity = 'NegativePolarity
    isInvertPolarity = Dict
    type LimitType 'PositivePolarity = BottomType
    showLimitType = "None"
    type JoinMeetType 'PositivePolarity = JoinType
    showJoinMeetType = "|"
    type ConvertType 'PositivePolarity (a :: k) (b :: k) = KindFunction k a b
    jmLeftIdentity = MkBijection unjoin2 $ \a -> MkJoinType (Right a)
    jmRightIdentity = MkBijection unjoin1 $ \a -> MkJoinType (Left a)
    jmMap a1a2 _ (MkJoinType (Left a)) = MkJoinType $ Left $ a1a2 a
    jmMap _ b1b2 (MkJoinType (Right b)) = MkJoinType $ Right $ b1b2 b

instance IsTypePolarity 'NegativePolarity where
    whichTypePolarity = Right Refl
    type InvertPolarity 'NegativePolarity = 'PositivePolarity
    isInvertPolarity = Dict
    type LimitType 'NegativePolarity = TopType
    showLimitType = "Any"
    type JoinMeetType 'NegativePolarity = MeetType
    showJoinMeetType = "&"
    type ConvertType 'NegativePolarity (a :: k) (b :: k) = KindFunction k b a
    jmLeftIdentity = MkBijection (\(MkMeetType (MkTopType, a)) -> a) $ \a -> MkMeetType (MkTopType, a)
    jmRightIdentity = MkBijection (\(MkMeetType (a, MkTopType)) -> a) $ \a -> MkMeetType (a, MkTopType)
    jmMap a1a2 b1b2 (MkMeetType (a, b)) = MkMeetType (a1a2 a, b1b2 b)
