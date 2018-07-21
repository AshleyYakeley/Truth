module Pinafore.Language.Type where

import GHC.TypeLits
import Pinafore.Language.Order
import Pinafore.Language.Value
import Pinafore.Literal
import Pinafore.Morphism
import Pinafore.Number
import Pinafore.Table (Point)
import Pinafore.Types
import Prelude (Bounded(..))
import Shapes
import Truth.Core

-- This is "soft" typing: it mostly represents types, but relies on coercing to and from a raw type (QValue baseedit) for type variables.
type Func a b = a -> b

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
    type LimitType polarity :: Type
    showLimitType :: Text
    type JoinMeetType polarity :: Type -> Type -> Type
    showJoinMeetType :: Text
    jmLeftIdentity :: Bijection (JoinMeetType polarity (LimitType polarity) a) a
    jmRightIdentity :: Bijection (JoinMeetType polarity a (LimitType polarity)) a

instance IsTypePolarity 'PositivePolarity where
    type InvertPolarity 'PositivePolarity = 'NegativePolarity
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

instance IsTypePolarity 'NegativePolarity where
    type InvertPolarity 'NegativePolarity = 'PositivePolarity
    type LimitType 'NegativePolarity = TopType
    showLimitType = "Top"
    type JoinMeetType 'NegativePolarity = MeetType
    showJoinMeetType = "&"
    jmLeftIdentity = MkBijection (\(MkMeetType ((), a)) -> a) $ \a -> MkMeetType ((), a)
    jmRightIdentity = MkBijection (\(MkMeetType (a, ())) -> a) $ \a -> MkMeetType (a, ())

class ExprShow t where
    exprShowPrec :: t -> (Text, Int)

exprPrecShow :: ExprShow t => Int -> t -> Text
exprPrecShow c t =
    case exprShowPrec t of
        (s, p) ->
            if c < p
                then "(" <> s <> ")"
                else s

exprShow :: ExprShow t => t -> Text
exprShow = exprPrecShow maxBound

type family MInvertPolarity (polarity :: Maybe TypePolarity) :: Maybe TypePolarity where
    MInvertPolarity ('Just polarity) = 'Just (InvertPolarity polarity)
    MInvertPolarity 'Nothing = 'Nothing

type QMutableReference baseedit t = PinaforeLensValue baseedit (WholeEdit t)

type QConstReference baseedit t = PinaforeFunctionValue baseedit t

type QMutableSet baseedit t = PinaforeLensValue baseedit (FiniteSetEdit t)

type QConstSet baseedit t = PinaforeFunctionValue baseedit (FiniteSet t)

type QMorphism baseedit a b = PinaforeLensMorphism baseedit a b

newtype Entity (name :: Symbol) =
    MkEntity Point

data PinaforeType (baseedit :: Type) (polarity :: Maybe TypePolarity) (t :: Type) where
    LimitPinaforeType :: IsTypePolarity polarity => PinaforeType baseedit ('Just polarity) (LimitType polarity)
    JoinMeetPinaforeType
        :: IsTypePolarity polarity
        => PinaforeType baseedit ('Just polarity) a
        -> PinaforeType baseedit ('Just polarity) b
        -> PinaforeType baseedit ('Just polarity) (JoinMeetType polarity a b)
    FunctionPinaforeType
        :: PinaforeType baseedit (MInvertPolarity polarity) a
        -> PinaforeType baseedit polarity b
        -> PinaforeType baseedit polarity (Func a b)
    ListPinaforeType :: PinaforeType baseedit polarity a -> PinaforeType baseedit polarity [a]
    PairPinaforeType
        :: PinaforeType baseedit polarity a -> PinaforeType baseedit polarity b -> PinaforeType baseedit polarity (a, b)
    MutableReferencePinaforeType
        :: PinaforeType baseedit 'Nothing a -> PinaforeType baseedit polarity (QMutableReference baseedit a)
    ConstReferencePinaforeType
        :: PinaforeType baseedit polarity a -> PinaforeType baseedit polarity (QConstReference baseedit a)
    MutableSetPinaforeType
        :: PinaforeType baseedit 'Nothing a -> PinaforeType baseedit polarity (QMutableSet baseedit a)
    ConstSetPinaforeType :: PinaforeType baseedit polarity a -> PinaforeType baseedit polarity (QConstSet baseedit a)
    MorphismPinaforeType
        :: PinaforeType baseedit 'Nothing a
        -> PinaforeType baseedit 'Nothing b
        -> PinaforeType baseedit polarity (QMorphism baseedit a b)
    InverseMorphismPinaforeType
        :: PinaforeType baseedit 'Nothing a
        -> PinaforeType baseedit 'Nothing b
        -> PinaforeType baseedit polarity (QMorphism baseedit a b)
    GroundPinaforeType :: GroundType baseedit t -> PinaforeType baseedit polarity t
    VarPinaforeType :: Int -> PinaforeType baseedit polarity (QValue baseedit)

simplifyType ::
       PinaforeType baseedit mpolarity a -> (forall b. PinaforeType baseedit mpolarity b -> Bijection a b -> r) -> r
simplifyType (JoinMeetPinaforeType LimitPinaforeType (tb :: PinaforeType _ ('Just polarity) _)) cont =
    cont tb $ jmLeftIdentity @polarity
simplifyType (JoinMeetPinaforeType ta (LimitPinaforeType :: PinaforeType _ ('Just polarity) _)) cont =
    cont ta $ jmRightIdentity @polarity
simplifyType t cont = cont t id

instance ExprShow (PinaforeType baseedit mpolarity t) where
    exprShowPrec t@LimitPinaforeType = let
        s :: forall polarity. IsTypePolarity polarity
          => PinaforeType _ ('Just polarity) _
          -> Text
        s _ = showLimitType @polarity
        in (s t, 0)
    exprShowPrec t@(JoinMeetPinaforeType ta tb) = let
        s :: forall polarity. IsTypePolarity polarity
          => PinaforeType _ ('Just polarity) _
          -> Text
        s _ = showJoinMeetType @polarity
        in (exprPrecShow 2 ta <> " " <> s t <> " " <> exprPrecShow 2 tb, 3)
    exprShowPrec (FunctionPinaforeType ta tb) = (exprPrecShow 2 ta <> " -> " <> exprPrecShow 3 tb, 3)
    exprShowPrec (ListPinaforeType ta) = ("[" <> exprShow ta <> "]", 0)
    exprShowPrec (PairPinaforeType ta tb) = ("(" <> exprShow ta <> ", " <> exprShow tb <> ")", 0)
    exprShowPrec (MutableReferencePinaforeType ta) = ("MRef " <> exprPrecShow 0 ta, 2)
    exprShowPrec (ConstReferencePinaforeType ta) = ("CRef " <> exprPrecShow 0 ta, 2)
    exprShowPrec (MutableSetPinaforeType ta) = ("MSet " <> exprPrecShow 0 ta, 2)
    exprShowPrec (ConstSetPinaforeType ta) = ("CSet " <> exprPrecShow 0 ta, 2)
    exprShowPrec (MorphismPinaforeType ta tb) = (exprPrecShow 1 ta <> " ~> " <> exprPrecShow 1 tb, 2)
    exprShowPrec (InverseMorphismPinaforeType ta tb) = (exprPrecShow 1 tb <> " <~ " <> exprPrecShow 1 ta, 2)
    exprShowPrec (GroundPinaforeType t) = exprShowPrec t
    exprShowPrec (VarPinaforeType i) = ("a" <> pack (show i), 0)

data GroundType (baseedit :: Type) (t :: Type) where
    ActionGroundType :: GroundType baseedit (QAction baseedit)
    OrderGroundType :: GroundType baseedit (QOrder baseedit)
    UserInterfaceGroundType :: GroundType baseedit (UISpec (ConstEdit Point) baseedit)
    LiteralGroundType :: LiteralType t -> GroundType baseedit (Maybe t)
    PointGroundType :: KnownSymbol name => GroundType baseedit (Entity name)

instance ExprShow (GroundType baseedit t) where
    exprShowPrec ActionGroundType = ("Action", 0)
    exprShowPrec OrderGroundType = ("Order", 0)
    exprShowPrec UserInterfaceGroundType = ("UI", 0)
    exprShowPrec (LiteralGroundType t) = exprShowPrec t
    exprShowPrec t@PointGroundType = let
        s :: forall name. KnownSymbol name
          => GroundType _ (Entity name)
          -> String
        s _ = symbolVal (Proxy :: Proxy name)
        in (pack $ s t, 0)

data LiteralType (t :: Type) where
    LiteralLiteralType :: LiteralType Literal
    TextLiteralType :: LiteralType Text
    NumberLiteralType :: LiteralType Number
    BottomLiteralType :: LiteralType BottomType

instance ExprShow (LiteralType t) where
    exprShowPrec LiteralLiteralType = ("Literal", 0)
    exprShowPrec TextLiteralType = ("Text", 0)
    exprShowPrec NumberLiteralType = ("Number", 0)
    exprShowPrec BottomLiteralType = ("LiteralBottom", 0)

class IsSubtype w where
    isSubtype :: w a -> w b -> Maybe (a -> b)

instance IsSubtype (GroundType baseedit) where
    isSubtype _ _ = Nothing
{-
class PinaforeTypeWitness w where
    castToRaw :: w baseedit t -> t -> QValue baseedit
    castFromRaw :: w baseedit t -> QValue baseedit -> Maybe t
-}
