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

join1 :: a -> JoinType a b
join1 v = MkJoinType $ Left v

join2 :: b -> JoinType a b
join2 v = MkJoinType $ Right v

joinf :: (a -> r) -> (b -> r) -> JoinType a b -> r
joinf f _ (MkJoinType (Left v)) = f v
joinf _ f (MkJoinType (Right v)) = f v

joinBimap :: (a1 -> a2) -> (b1 -> b2) -> JoinType a1 b1 -> JoinType a2 b2
joinBimap f _ (MkJoinType (Left v)) = MkJoinType $ Left $ f v
joinBimap _ f (MkJoinType (Right v)) = MkJoinType $ Right $ f v

newtype MeetType a b =
    MkMeetType (a, b)

meet1 :: MeetType a b -> a
meet1 (MkMeetType (v, _)) = v

meet2 :: MeetType a b -> b
meet2 (MkMeetType (_, v)) = v

meetf :: (r -> a) -> (r -> b) -> r -> MeetType a b
meetf f1 f2 v = MkMeetType (f1 v, f2 v)

meetBimap :: (a1 -> a2) -> (b1 -> b2) -> MeetType a1 b1 -> MeetType a2 b2
meetBimap aa bb (MkMeetType (a, b)) = MkMeetType (aa a, bb b)

data TypePolarity
    = PositivePolarity
    | NegativePolarity

class IsTypePolarity (polarity :: TypePolarity) where
    whichTypePolarity :: Either (polarity :~: 'PositivePolarity) (polarity :~: 'NegativePolarity)
    type InvertPolarity polarity :: TypePolarity
    isInvertPolarity :: Dict (IsTypePolarity (InvertPolarity polarity))
    type LimitType polarity :: Type
    showLimitType :: Text
    type JoinMeetType polarity :: Type -> Type -> Type
    showJoinMeetType :: Text
    type ConvertType polarity (a :: Type) (b :: Type) :: Type
    jmLeftIdentity :: Bijection (JoinMeetType polarity (LimitType polarity) a) a
    jmRightIdentity :: Bijection (JoinMeetType polarity a (LimitType polarity)) a
    jmMap :: (a1 -> a2) -> (b1 -> b2) -> JoinMeetType polarity a1 b1 -> JoinMeetType polarity a2 b2

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
    showLimitType = "Bottom"
    type JoinMeetType 'PositivePolarity = JoinType
    showJoinMeetType = "|"
    type ConvertType 'PositivePolarity a b = a -> b
    jmLeftIdentity = let
        unjoin :: JoinType BottomType a -> a
        unjoin (MkJoinType (Left n)) = never n
        unjoin (MkJoinType (Right a)) = a
        in MkBijection unjoin $ \a -> MkJoinType (Right a)
    jmRightIdentity = let
        unjoin :: JoinType a BottomType -> a
        unjoin (MkJoinType (Left a)) = a
        unjoin (MkJoinType (Right n)) = never n
        in MkBijection unjoin $ \a -> MkJoinType (Left a)
    jmMap a1a2 _ (MkJoinType (Left a)) = MkJoinType $ Left $ a1a2 a
    jmMap _ b1b2 (MkJoinType (Right b)) = MkJoinType $ Right $ b1b2 b

instance IsTypePolarity 'NegativePolarity where
    whichTypePolarity = Right Refl
    type InvertPolarity 'NegativePolarity = 'PositivePolarity
    isInvertPolarity = Dict
    type LimitType 'NegativePolarity = TopType
    showLimitType = "Top"
    type JoinMeetType 'NegativePolarity = MeetType
    showJoinMeetType = "&"
    type ConvertType 'NegativePolarity a b = b -> a
    jmLeftIdentity = MkBijection (\(MkMeetType (MkTopType, a)) -> a) $ \a -> MkMeetType (MkTopType, a)
    jmRightIdentity = MkBijection (\(MkMeetType (a, MkTopType)) -> a) $ \a -> MkMeetType (a, MkTopType)
    jmMap a1a2 b1b2 (MkMeetType (a, b)) = MkMeetType (a1a2 a, b1b2 b)
