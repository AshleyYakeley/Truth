module Pinafore.Language.TypeSystem.Type where

import qualified Data.List as List
import Data.Shim
import Language.Expression.Dolan
import Language.Expression.Renamer
import Language.Expression.Sealed
import Language.Expression.UVar
import Pinafore.Language.Error
import Pinafore.Language.Name
import Pinafore.Language.Scope
import Pinafore.Language.Type.Entity
import Pinafore.Language.Type.Ground
import Pinafore.Language.TypeSystem.Show
import Shapes

type PinaforeRangeType baseupdate = RangeType (PinaforeType baseupdate)

data PinaforeType (baseupdate :: Type) (polarity :: Polarity) (t :: Type) where
    NilPinaforeType :: PinaforeType baseupdate polarity (LimitType polarity)
    ConsPinaforeType
        :: PinaforeSingularType baseupdate polarity t
        -> PinaforeType baseupdate polarity tr
        -> PinaforeType baseupdate polarity (JoinMeetType polarity t tr)

-- | This is \"soft\" typing: it mostly represents types, but relies on unsafe coercing to and from a raw type ('UVar') for type variables.
data PinaforeSingularType (baseupdate :: Type) (polarity :: Polarity) (t :: Type) where
    GroundPinaforeSingularType
        :: PinaforeGroundType baseupdate dv polarity t
        -> DolanArguments dv (PinaforeType baseupdate) t polarity ta
        -> PinaforeSingularType baseupdate polarity ta
    VarPinaforeSingularType :: SymbolType name -> PinaforeSingularType baseupdate polarity (UVar name)

type PinaforeShim = JMShim

type PinaforeShimWit (baseupdate :: Type) polarity = PJMShimWit (PinaforeType baseupdate) polarity

singlePinaforeShimWit ::
       forall baseupdate polarity t. Is PolarityType polarity
    => PJMShimWit (PinaforeSingularType baseupdate) polarity t
    -> PinaforeShimWit baseupdate polarity t
singlePinaforeShimWit (MkShimWit st conv) =
    case representative @_ @_ @polarity of
        PositiveType -> ccontramap conv $ MkShimWit (singlePinaforeType st) join1
        NegativeType -> cfmap conv $ MkShimWit (singlePinaforeType st) meet1

singlePinaforeType ::
       PinaforeSingularType baseupdate polarity t
    -> PinaforeType baseupdate polarity (JoinMeetType polarity t (LimitType polarity))
singlePinaforeType st = ConsPinaforeType st NilPinaforeType

joinPinaforeTypes ::
       forall baseupdate (a :: Type) (b :: Type) r.
       PinaforeType baseupdate 'Positive a
    -> PinaforeType baseupdate 'Positive b
    -> (forall ab. PinaforeType baseupdate 'Positive ab -> JMShim a ab -> JMShim b ab -> r)
    -> r
joinPinaforeTypes NilPinaforeType tb cont = cont tb initf id
joinPinaforeTypes (ConsPinaforeType ta tr) tb cont =
    joinPinaforeTypes tr tb $ \trb conva convb -> cont (ConsPinaforeType ta trb) (joinBimap id conva) (join2 . convb)

joinPinaforeShimWit ::
       forall baseupdate (a :: Type) (b :: Type).
       PinaforeShimWit baseupdate 'Positive a
    -> PinaforeShimWit baseupdate 'Positive b
    -> PinaforeShimWit baseupdate 'Positive (JoinType a b)
joinPinaforeShimWit (MkShimWit ta conva) (MkShimWit tb convb) =
    ccontramap (joinBimap conva convb) $
    joinPinaforeTypes ta tb $ \tab conva' convb' -> MkShimWit tab $ joinf conva' convb'

meetPinaforeTypes ::
       forall baseupdate (a :: Type) (b :: Type) r.
       PinaforeType baseupdate 'Negative a
    -> PinaforeType baseupdate 'Negative b
    -> (forall ab. PinaforeType baseupdate 'Negative ab -> JMShim ab a -> JMShim ab b -> r)
    -> r
meetPinaforeTypes NilPinaforeType tb cont = cont tb termf id
meetPinaforeTypes (ConsPinaforeType ta tr) tb cont =
    meetPinaforeTypes tr tb $ \trb conva convb -> cont (ConsPinaforeType ta trb) (meetBimap id conva) (convb . meet2)

meetPinaforeShimWit ::
       forall baseupdate (a :: Type) (b :: Type).
       PinaforeShimWit baseupdate 'Negative a
    -> PinaforeShimWit baseupdate 'Negative b
    -> PinaforeShimWit baseupdate 'Negative (MeetType a b)
meetPinaforeShimWit (MkShimWit ta conva) (MkShimWit tb convb) =
    cfmap (meetBimap conva convb) $ meetPinaforeTypes ta tb $ \tab conva' convb' -> MkShimWit tab $ meetf conva' convb'

instance Is PolarityType polarity => Semigroup (AnyW (PinaforeType baseupdate polarity)) where
    MkAnyW ta <> MkAnyW tb =
        case representative @_ @_ @polarity of
            PositiveType -> joinPinaforeTypes ta tb $ \tab _ _ -> MkAnyW tab
            NegativeType -> meetPinaforeTypes ta tb $ \tab _ _ -> MkAnyW tab

instance Is PolarityType polarity => Monoid (AnyW (PinaforeType baseupdate polarity)) where
    mappend = (<>)
    mempty = MkAnyW NilPinaforeType

instance Is PolarityType polarity => Semigroup (AnyInKind (RangeType (PinaforeType baseupdate) polarity)) where
    MkAnyInKind (MkRangeType tp1 tq1) <> MkAnyInKind (MkRangeType tp2 tq2) =
        invertPolarity @polarity $
        case (MkAnyW tp1 <> MkAnyW tp2, MkAnyW tq1 <> MkAnyW tq2) of
            (MkAnyW tp12, MkAnyW tq12) -> MkAnyInKind (MkRangeType tp12 tq12)

instance Is PolarityType polarity => Monoid (AnyInKind (RangeType (PinaforeType baseupdate) polarity)) where
    mappend = (<>)
    mempty = MkAnyInKind (MkRangeType NilPinaforeType NilPinaforeType)

instance Is PolarityType polarity => ExprShow (PinaforeSingularType baseupdate polarity t) where
    exprShowPrec (VarPinaforeSingularType namewit) = (pack $ show namewit, 0)
    exprShowPrec (GroundPinaforeSingularType gt args) = pinaforeGroundTypeShowPrec gt args

instance Is PolarityType polarity => ExprShow (PinaforeType baseupdate polarity t) where
    exprShowPrec NilPinaforeType =
        case representative @_ @_ @polarity of
            PositiveType -> ("None", 0)
            NegativeType -> ("Any", 0)
    exprShowPrec (ConsPinaforeType ta NilPinaforeType) = exprShowPrec ta
    exprShowPrec (ConsPinaforeType ta tb) = let
        jmConnector =
            case representative @_ @_ @polarity of
                PositiveType -> " | "
                NegativeType -> " & "
        in (exprPrecShow 2 ta <> jmConnector <> exprPrecShow 2 tb, 3)

instance Is PolarityType polarity => Show (PinaforeType baseupdate polarity t) where
    show v = unpack $ exprShow v

instance Is PolarityType polarity => AllWitnessConstraint Show (PinaforeType baseupdate polarity) where
    allWitnessConstraint = Dict

instance Is PolarityType polarity => ExprShow (PinaforeRangeType baseupdate polarity a) where
    exprShowPrec (MkRangeType t1 t2) = let
        getpieces ::
               forall pol t. Is PolarityType pol
            => PinaforeType baseupdate pol t
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

pinaforeToEntityArgs ::
       forall baseupdate dv f polarity t. Is PolarityType polarity
    => CovaryType dv
    -> CovaryMap JMIsoShim f
    -> DolanArguments dv (PinaforeType baseupdate) f polarity t
    -> Maybe (ShimWit JMIsoShim (Arguments EntityType f) polarity t)
pinaforeToEntityArgs = dolanArgumentsToArgumentsM pinaforeToEntityType

pinaforeEntityToEntityType ::
       forall baseupdate dv f polarity a. Is PolarityType polarity
    => CovaryType dv
    -> EntityGroundType f
    -> DolanArguments dv (PinaforeType baseupdate) f polarity a
    -> Maybe (ShimWit JMIsoShim EntityType polarity a)
pinaforeEntityToEntityType lc gt args = do
    MkShimWit eargs conv <- pinaforeToEntityArgs lc (bijectCovaryMap $ entityGroundTypeCovaryMap gt) args
    return $ MkShimWit (MkEntityType gt eargs) conv

pinaforeToEntityType ::
       forall baseupdate polarity a. Is PolarityType polarity
    => PinaforeType baseupdate polarity a
    -> Maybe (ShimWit JMIsoShim EntityType polarity a)
pinaforeToEntityType (ConsPinaforeType (GroundPinaforeSingularType (EntityPinaforeGroundType lc gt) args) NilPinaforeType) = do
    MkShimWit et conv <- pinaforeEntityToEntityType lc gt args
    return $
        MkShimWit et $
        case representative @_ @_ @polarity of
            PositiveType -> conv <.> MkJMIsoShim bijoin1
            NegativeType -> MkJMIsoShim bimeet1 <.> conv
pinaforeToEntityType NilPinaforeType
    | PositiveType <- representative @_ @_ @polarity = Just $ MkShimWit NoneEntityType id
pinaforeToEntityType _ = Nothing

entityToNegativePinaforeType ::
       forall baseupdate m t. MonadError ErrorType m
    => EntityType t
    -> m (PinaforeShimWit baseupdate 'Negative t)
entityToNegativePinaforeType (MkEntityType gt args) =
    entityGroundTypeCovaryType gt $ \ct -> do
        MkShimWit dargs conv <-
            argumentsToDolanArgumentsM entityToNegativePinaforeType ct (entityGroundTypeCovaryMap gt) args
        return $
            singlePinaforeShimWit $ MkShimWit (GroundPinaforeSingularType (EntityPinaforeGroundType ct gt) dargs) conv
entityToNegativePinaforeType NoneEntityType = throwError InterpretTypeNoneNotNegativeEntityError

entityToPositivePinaforeType :: forall baseupdate t. EntityType t -> PinaforeShimWit baseupdate 'Positive t
entityToPositivePinaforeType (MkEntityType gt args) =
    entityGroundTypeCovaryType gt $ \ct ->
        case argumentsToDolanArguments entityToPositivePinaforeType ct (entityGroundTypeCovaryMap gt) args of
            MkShimWit dargs conv ->
                singlePinaforeShimWit $
                MkShimWit (GroundPinaforeSingularType (EntityPinaforeGroundType ct gt) dargs) conv
entityToPositivePinaforeType NoneEntityType = mkShimWit NilPinaforeType

type PinaforeExpression baseupdate
     = SealedExpression Name (PinaforeShimWit baseupdate 'Negative) (PinaforeShimWit baseupdate 'Positive)

type PinaforePatternConstructor baseupdate
     = PatternConstructor Name (PinaforeShimWit baseupdate 'Positive) (PinaforeShimWit baseupdate 'Negative)

type PinaforePattern baseupdate
     = SealedPattern Name (PinaforeShimWit baseupdate 'Positive) (PinaforeShimWit baseupdate 'Negative)

data PinaforeTypeSystem (baseupdate :: Type)

type PinaforeScoped baseupdate
     = Scoped (PinaforeExpression baseupdate) (PinaforePatternConstructor baseupdate) (AnyW ClosedEntityType)

type PinaforeSourceScoped baseupdate
     = SourceScoped (PinaforeExpression baseupdate) (PinaforePatternConstructor baseupdate) (AnyW ClosedEntityType)

type PinaforeTypeCheck baseupdate = VarRenamerT (PinaforeTypeSystem baseupdate) (PinaforeSourceScoped baseupdate)
