module Language.Expression.SoftType.Dolan where

import Shapes

type TopType = ()

type BottomType = None

newtype JoinType a b =
    MkJoinType (Either a b)

newtype MeetType a b =
    MkMeetType (a, b)

data TypePolarity
    = PositivePolarity
    | NegativePolarity

class IsTypePolarity (polarity :: TypePolarity) where
    type InvertPolarity polarity :: TypePolarity
    isInvertPolarity :: Dict (IsTypePolarity (InvertPolarity polarity))
    type LimitType polarity :: Type
    showLimitType :: Text
    type JoinMeetType polarity :: Type -> Type -> Type
    showJoinMeetType :: Text
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
    type InvertPolarity 'PositivePolarity = 'NegativePolarity
    isInvertPolarity = Dict
    type LimitType 'PositivePolarity = BottomType
    showLimitType = "Bottom"
    type JoinMeetType 'PositivePolarity = JoinType
    showJoinMeetType = "|"
    jmLeftIdentity = let
        unjoin :: JoinType None a -> a
        unjoin (MkJoinType (Left n)) = never n
        unjoin (MkJoinType (Right a)) = a
        in MkBijection unjoin $ \a -> MkJoinType (Right a)
    jmRightIdentity = let
        unjoin :: JoinType a None -> a
        unjoin (MkJoinType (Left a)) = a
        unjoin (MkJoinType (Right n)) = never n
        in MkBijection unjoin $ \a -> MkJoinType (Left a)
    jmMap a1a2 _ (MkJoinType (Left a)) = MkJoinType $ Left $ a1a2 a
    jmMap _ b1b2 (MkJoinType (Right b)) = MkJoinType $ Right $ b1b2 b

instance IsTypePolarity 'NegativePolarity where
    type InvertPolarity 'NegativePolarity = 'PositivePolarity
    isInvertPolarity = Dict
    type LimitType 'NegativePolarity = TopType
    showLimitType = "Top"
    type JoinMeetType 'NegativePolarity = MeetType
    showJoinMeetType = "&"
    jmLeftIdentity = MkBijection (\(MkMeetType ((), a)) -> a) $ \a -> MkMeetType ((), a)
    jmRightIdentity = MkBijection (\(MkMeetType (a, ())) -> a) $ \a -> MkMeetType (a, ())
    jmMap a1a2 b1b2 (MkMeetType (a, b)) = MkMeetType (a1a2 a, b1b2 b)
