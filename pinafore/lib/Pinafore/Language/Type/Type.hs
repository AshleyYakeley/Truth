module Pinafore.Language.Type.Type where

import qualified Data.List as List
import Language.Expression.Dolan
import Language.Expression.Sealed
import Language.Expression.UVar
import Pinafore.Language.GroundType
import Pinafore.Language.Literal
import Pinafore.Language.Show
import Pinafore.Language.SimpleEntityType
import Shapes

type PinaforeRangeType baseedit = RangeType (PinaforeType baseedit)

data PinaforeType (baseedit :: Type) (polarity :: TypePolarity) (t :: Type) where
    NilPinaforeType :: PinaforeType baseedit polarity (LimitType polarity)
    ConsPinaforeType
        :: PinaforeSingularType baseedit polarity t
        -> PinaforeType baseedit polarity tr
        -> PinaforeType baseedit polarity (JoinMeetType polarity t tr)

-- | This is \"soft\" typing: it mostly represents types, but relies on unsafe coercing to and from a raw type ('UVar') for type variables.
data PinaforeSingularType (baseedit :: Type) (polarity :: TypePolarity) (t :: Type) where
    GroundPinaforeSingularType
        :: PinaforeGroundType baseedit polarity dv t
        -> DolanArguments dv (PinaforeType baseedit) t polarity ta
        -> PinaforeSingularType baseedit polarity ta
    VarPinaforeSingularType :: SymbolWitness name -> PinaforeSingularType baseedit polarity (UVar name)

type PinaforeTypeF (baseedit :: Type) = TypeF (PinaforeType baseedit)

singlePinaforeTypeF ::
       forall baseedit polarity t. IsTypePolarity polarity
    => TypeF (PinaforeSingularType baseedit) polarity t
    -> PinaforeTypeF baseedit polarity t
singlePinaforeTypeF (MkTypeF st conv) =
    case whichTypePolarity @polarity of
        Left Refl -> contramap conv $ MkTypeF (singlePinaforeType st) join1
        Right Refl -> fmap conv $ MkTypeF (singlePinaforeType st) meet1

singlePinaforeType ::
       PinaforeSingularType baseedit polarity t
    -> PinaforeType baseedit polarity (JoinMeetType polarity t (LimitType polarity))
singlePinaforeType st = ConsPinaforeType st NilPinaforeType

literalPinaforeType :: LiteralType t -> PinaforeType baseedit polarity (JoinMeetType polarity t (LimitType polarity))
literalPinaforeType t =
    singlePinaforeType $
    GroundPinaforeSingularType (SimpleEntityPinaforeGroundType $ LiteralSimpleEntityType t) NilDolanArguments

joinPinaforeTypes ::
       forall baseedit (a :: Type) (b :: Type) r.
       PinaforeType baseedit 'PositivePolarity a
    -> PinaforeType baseedit 'PositivePolarity b
    -> (forall ab. PinaforeType baseedit 'PositivePolarity ab -> (a -> ab) -> (b -> ab) -> r)
    -> r
joinPinaforeTypes NilPinaforeType tb cont = cont tb never id
joinPinaforeTypes (ConsPinaforeType ta tr) tb cont =
    joinPinaforeTypes tr tb $ \trb conva convb -> cont (ConsPinaforeType ta trb) (joinBimap id conva) (join2 . convb)

joinPinaforeTypeF ::
       forall baseedit (a :: Type) (b :: Type).
       PinaforeTypeF baseedit 'PositivePolarity a
    -> PinaforeTypeF baseedit 'PositivePolarity b
    -> PinaforeTypeF baseedit 'PositivePolarity (JoinType a b)
joinPinaforeTypeF (MkTypeF ta conva) (MkTypeF tb convb) =
    contramap (joinBimap conva convb) $
    joinPinaforeTypes ta tb $ \tab conva' convb' -> MkTypeF tab $ joinf conva' convb'

meetPinaforeTypes ::
       forall baseedit (a :: Type) (b :: Type) r.
       PinaforeType baseedit 'NegativePolarity a
    -> PinaforeType baseedit 'NegativePolarity b
    -> (forall ab. PinaforeType baseedit 'NegativePolarity ab -> (ab -> a) -> (ab -> b) -> r)
    -> r
meetPinaforeTypes NilPinaforeType tb cont = cont tb alwaysTop id
meetPinaforeTypes (ConsPinaforeType ta tr) tb cont =
    meetPinaforeTypes tr tb $ \trb conva convb -> cont (ConsPinaforeType ta trb) (meetBimap id conva) (convb . meet2)

meetPinaforeTypeF ::
       forall baseedit (a :: Type) (b :: Type).
       PinaforeTypeF baseedit 'NegativePolarity a
    -> PinaforeTypeF baseedit 'NegativePolarity b
    -> PinaforeTypeF baseedit 'NegativePolarity (MeetType a b)
meetPinaforeTypeF (MkTypeF ta conva) (MkTypeF tb convb) =
    fmap (meetBimap conva convb) $ meetPinaforeTypes ta tb $ \tab conva' convb' -> MkTypeF tab $ meetf conva' convb'

instance IsTypePolarity polarity => Semigroup (AnyW (PinaforeType baseedit polarity)) where
    MkAnyW ta <> MkAnyW tb =
        case whichTypePolarity @polarity of
            Left Refl -> joinPinaforeTypes ta tb $ \tab _ _ -> MkAnyW tab
            Right Refl -> meetPinaforeTypes ta tb $ \tab _ _ -> MkAnyW tab

instance IsTypePolarity polarity => Monoid (AnyW (PinaforeType baseedit polarity)) where
    mappend = (<>)
    mempty = MkAnyW NilPinaforeType

instance IsTypePolarity polarity => Semigroup (AnyInKind (RangeType (PinaforeType baseedit) polarity)) where
    MkAnyInKind (MkRangeType tp1 tq1) <> MkAnyInKind (MkRangeType tp2 tq2) =
        invertPolarity @polarity $
        case (MkAnyW tp1 <> MkAnyW tp2, MkAnyW tq1 <> MkAnyW tq2) of
            (MkAnyW tp12, MkAnyW tq12) -> MkAnyInKind (MkRangeType tp12 tq12)

instance IsTypePolarity polarity => Monoid (AnyInKind (RangeType (PinaforeType baseedit) polarity)) where
    mappend = (<>)
    mempty = MkAnyInKind (MkRangeType NilPinaforeType NilPinaforeType)

instance IsTypePolarity polarity => ExprShow (PinaforeSingularType baseedit polarity t) where
    exprShowPrec (VarPinaforeSingularType namewit) = (pack $ show namewit, 0)
    exprShowPrec (GroundPinaforeSingularType gt args) = exprShowPrecGroundType gt args

instance IsTypePolarity polarity => ExprShow (PinaforeType baseedit polarity t) where
    exprShowPrec NilPinaforeType = (showLimitType @polarity, 0)
    exprShowPrec (ConsPinaforeType ta NilPinaforeType) = exprShowPrec ta
    exprShowPrec (ConsPinaforeType ta tb) =
        (exprPrecShow 2 ta <> " " <> showJoinMeetType @polarity <> " " <> exprPrecShow 2 tb, 3)

instance IsTypePolarity polarity => Show (PinaforeType baseedit polarity t) where
    show v = unpack $ exprShow v

exprShowPrecGroundType ::
       forall baseedit polarity dv t ta. IsTypePolarity polarity
    => PinaforeGroundType baseedit polarity dv t
    -> DolanArguments dv (PinaforeType baseedit) t polarity ta
    -> (Text, Int)
exprShowPrecGroundType ActionPinaforeGroundType NilDolanArguments = ("Action", 0)
exprShowPrecGroundType OrderPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    invertPolarity @polarity ("Order " <> exprPrecShow 0 ta, 2)
exprShowPrecGroundType UserInterfacePinaforeGroundType NilDolanArguments = ("UI", 0)
exprShowPrecGroundType (SimpleEntityPinaforeGroundType t) NilDolanArguments = exprShowPrec t
exprShowPrecGroundType FuncPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) =
    invertPolarity @polarity (exprPrecShow 2 ta <> " -> " <> exprPrecShow 3 tb, 3)
exprShowPrecGroundType ListPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) = ("[" <> exprShow ta <> "]", 0)
exprShowPrecGroundType PairPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) =
    ("(" <> exprShow ta <> ", " <> exprShow tb <> ")", 0)
exprShowPrecGroundType EitherPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) =
    ("Either " <> exprShow ta <> " " <> exprShow tb, 2)
exprShowPrecGroundType ReferencePinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    ("Ref " <> exprPrecShow 0 ta, 2)
exprShowPrecGroundType SetPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    ("Set " <> exprPrecShow 0 ta, 2)
exprShowPrecGroundType MorphismPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) =
    invertPolarity @polarity (exprPrecShow 2 ta <> " ~> " <> exprPrecShow 3 tb, 3)

instance IsTypePolarity polarity => ExprShow (PinaforeRangeType baseedit polarity a) where
    exprShowPrec (MkRangeType t1 t2) = let
        getpieces ::
               forall pol t. IsTypePolarity pol
            => PinaforeType baseedit pol t
            -> [Text]
        getpieces NilPinaforeType = []
        getpieces (ConsPinaforeType t tr) = exprPrecShow 0 t : getpieces tr
        contrapieces = nub $ invertPolarity @polarity $ getpieces t1
        copieces = nub $ getpieces t2
        bothpieces = List.intersect contrapieces copieces
        rcontrapieces = contrapieces List.\\ bothpieces
        rcopieces = copieces List.\\ bothpieces
        pieces :: [Text]
        pieces = bothpieces <> fmap ("-" <>) rcontrapieces <> fmap ("+" <>) rcopieces
        text :: Text
        text =
            case pieces of
                [t] -> t
                _ -> "{" <> ointercalate "," pieces <> "}"
        in (text, 0)

type PinaforeExpression baseedit name
     = SealedExpression name (PinaforeType baseedit 'NegativePolarity) (PinaforeType baseedit 'PositivePolarity)

data PinaforeTypeSystem (baseedit :: Type)
