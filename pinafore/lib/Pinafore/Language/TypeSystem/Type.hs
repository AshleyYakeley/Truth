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

type PinaforeRangeType = RangeType PinaforeType

data PinaforeType (polarity :: Polarity) (t :: Type) where
    NilPinaforeType :: PinaforeType polarity (LimitType polarity)
    ConsPinaforeType
        :: PinaforeSingularType polarity t
        -> PinaforeType polarity tr
        -> PinaforeType polarity (JoinMeetType polarity t tr)

-- | This is \"soft\" typing: it mostly represents types, but relies on unsafe coercing to and from a raw type ('UVar') for type variables.
data PinaforeSingularType (polarity :: Polarity) (t :: Type) where
    GroundPinaforeSingularType
        :: PinaforeGroundType dv t -> DolanArguments dv PinaforeType t polarity ta -> PinaforeSingularType polarity ta
    VarPinaforeSingularType :: SymbolType name -> PinaforeSingularType polarity (UVar name)

type PinaforeShim = JMShim

type PinaforeShimWit polarity = PJMShimWit PinaforeType polarity

singlePinaforeShimWit ::
       forall polarity t. Is PolarityType polarity
    => PJMShimWit PinaforeSingularType polarity t
    -> PinaforeShimWit polarity t
singlePinaforeShimWit (MkShimWit st conv) =
    case representative @_ @_ @polarity of
        PositiveType -> ccontramap conv $ MkShimWit (singlePinaforeType st) join1
        NegativeType -> cfmap conv $ MkShimWit (singlePinaforeType st) meet1

singlePinaforeType ::
       PinaforeSingularType polarity t -> PinaforeType polarity (JoinMeetType polarity t (LimitType polarity))
singlePinaforeType st = ConsPinaforeType st NilPinaforeType

joinPinaforeTypes ::
       forall (a :: Type) (b :: Type) r.
       PinaforeType 'Positive a
    -> PinaforeType 'Positive b
    -> (forall ab. PinaforeType 'Positive ab -> JMShim a ab -> JMShim b ab -> r)
    -> r
joinPinaforeTypes NilPinaforeType tb cont = cont tb initf id
joinPinaforeTypes (ConsPinaforeType ta tr) tb cont =
    joinPinaforeTypes tr tb $ \trb conva convb -> cont (ConsPinaforeType ta trb) (joinBimap id conva) (join2 . convb)

joinPinaforeShimWit ::
       forall (a :: Type) (b :: Type).
       PinaforeShimWit 'Positive a
    -> PinaforeShimWit 'Positive b
    -> PinaforeShimWit 'Positive (JoinType a b)
joinPinaforeShimWit (MkShimWit ta conva) (MkShimWit tb convb) =
    ccontramap (joinBimap conva convb) $
    joinPinaforeTypes ta tb $ \tab conva' convb' -> MkShimWit tab $ joinf conva' convb'

meetPinaforeTypes ::
       forall (a :: Type) (b :: Type) r.
       PinaforeType 'Negative a
    -> PinaforeType 'Negative b
    -> (forall ab. PinaforeType 'Negative ab -> JMShim ab a -> JMShim ab b -> r)
    -> r
meetPinaforeTypes NilPinaforeType tb cont = cont tb termf id
meetPinaforeTypes (ConsPinaforeType ta tr) tb cont =
    meetPinaforeTypes tr tb $ \trb conva convb -> cont (ConsPinaforeType ta trb) (meetBimap id conva) (convb . meet2)

meetPinaforeShimWit ::
       forall (a :: Type) (b :: Type).
       PinaforeShimWit 'Negative a
    -> PinaforeShimWit 'Negative b
    -> PinaforeShimWit 'Negative (MeetType a b)
meetPinaforeShimWit (MkShimWit ta conva) (MkShimWit tb convb) =
    cfmap (meetBimap conva convb) $ meetPinaforeTypes ta tb $ \tab conva' convb' -> MkShimWit tab $ meetf conva' convb'

instance Is PolarityType polarity => Semigroup (AnyW (PinaforeType polarity)) where
    MkAnyW ta <> MkAnyW tb =
        case representative @_ @_ @polarity of
            PositiveType -> joinPinaforeTypes ta tb $ \tab _ _ -> MkAnyW tab
            NegativeType -> meetPinaforeTypes ta tb $ \tab _ _ -> MkAnyW tab

instance Is PolarityType polarity => Monoid (AnyW (PinaforeType polarity)) where
    mappend = (<>)
    mempty = MkAnyW NilPinaforeType

instance Is PolarityType polarity => Semigroup (AnyInKind (RangeType PinaforeType polarity)) where
    MkAnyInKind (MkRangeType tp1 tq1) <> MkAnyInKind (MkRangeType tp2 tq2) =
        invertPolarity @polarity $
        case (MkAnyW tp1 <> MkAnyW tp2, MkAnyW tq1 <> MkAnyW tq2) of
            (MkAnyW tp12, MkAnyW tq12) -> MkAnyInKind (MkRangeType tp12 tq12)

instance Is PolarityType polarity => Monoid (AnyInKind (RangeType PinaforeType polarity)) where
    mappend = (<>)
    mempty = MkAnyInKind (MkRangeType NilPinaforeType NilPinaforeType)

instance Is PolarityType polarity => ExprShow (PinaforeSingularType polarity t) where
    exprShowPrec (VarPinaforeSingularType namewit) = (pack $ show namewit, 0)
    exprShowPrec (GroundPinaforeSingularType gt args) = pinaforeGroundTypeShowPrec gt args

instance Is PolarityType polarity => ExprShow (PinaforeType polarity t) where
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

instance Is PolarityType polarity => Show (PinaforeType polarity t) where
    show v = unpack $ exprShow v

instance Is PolarityType polarity => AllWitnessConstraint Show (PinaforeType polarity) where
    allWitnessConstraint = Dict

instance Is PolarityType polarity => ExprShow (PinaforeRangeType polarity a) where
    exprShowPrec (MkRangeType t1 t2) = let
        getpieces ::
               forall pol t. Is PolarityType pol
            => PinaforeType pol t
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

pinaforeToConcreteEntityArgs ::
       forall dv f polarity t. Is PolarityType polarity
    => CovaryType dv
    -> CovaryMap f
    -> DolanArguments dv PinaforeType f polarity t
    -> Maybe (ShimWit JMIsoShim (Arguments ConcreteEntityType f) polarity t)
pinaforeToConcreteEntityArgs = dolanArgumentsToArgumentsM pinaforeToConcreteEntityType

pinaforeEntityToConcreteEntityType ::
       forall dv f polarity a. Is PolarityType polarity
    => CovaryType dv
    -> EntityGroundType f
    -> DolanArguments dv PinaforeType f polarity a
    -> Maybe (ShimWit JMIsoShim ConcreteEntityType polarity a)
pinaforeEntityToConcreteEntityType lc gt args = do
    MkShimWit eargs conv <- pinaforeToConcreteEntityArgs lc (entityGroundTypeCovaryMap gt) args
    return $ MkShimWit (MkConcreteType gt eargs) conv

pinaforeSingularToConcreteEntityType ::
       forall polarity a. Is PolarityType polarity
    => PinaforeSingularType polarity a
    -> Maybe (ShimWit JMIsoShim ConcreteEntityType polarity a)
pinaforeSingularToConcreteEntityType (GroundPinaforeSingularType (EntityPinaforeGroundType lc gt) args) =
    pinaforeEntityToConcreteEntityType lc gt args
pinaforeSingularToConcreteEntityType _ = Nothing

pinaforeToConcreteEntityType ::
       forall polarity a. Is PolarityType polarity
    => PinaforeType polarity a
    -> Maybe (ShimWit JMIsoShim ConcreteEntityType polarity a)
pinaforeToConcreteEntityType (ConsPinaforeType t NilPinaforeType) = do
    MkShimWit et conv <- pinaforeSingularToConcreteEntityType t
    return $
        MkShimWit et $
        case representative @_ @_ @polarity of
            PositiveType -> conv <.> MkJMIsoShim bijoin1
            NegativeType -> MkJMIsoShim bimeet1 <.> conv
pinaforeToConcreteEntityType _ = Nothing

concreteEntityToMaybeNegativePinaforeType :: forall t. ConcreteEntityType t -> Maybe (PinaforeShimWit 'Negative t)
concreteEntityToMaybeNegativePinaforeType (MkConcreteType gt args) =
    entityGroundTypeCovaryType gt $ \ct -> do
        MkShimWit dargs conv <-
            argumentsToDolanArgumentsM concreteEntityToMaybeNegativePinaforeType ct (entityGroundTypeCovaryMap gt) args
        return $
            singlePinaforeShimWit $ MkShimWit (GroundPinaforeSingularType (EntityPinaforeGroundType ct gt) dargs) conv

concreteEntityToNegativePinaforeType ::
       forall m t. MonadThrow ErrorType m
    => ConcreteEntityType t
    -> m (PinaforeShimWit 'Negative t)
concreteEntityToNegativePinaforeType et =
    case concreteEntityToMaybeNegativePinaforeType et of
        Just wit -> return wit
        Nothing -> throw InterpretTypeNoneNotNegativeEntityError

concreteEntityToPositivePinaforeType :: forall t. ConcreteEntityType t -> PinaforeShimWit 'Positive t
concreteEntityToPositivePinaforeType (MkConcreteType gt args) =
    entityGroundTypeCovaryType gt $ \ct ->
        case argumentsToDolanArguments concreteEntityToPositivePinaforeType ct (entityGroundTypeCovaryMap gt) args of
            MkShimWit dargs conv ->
                singlePinaforeShimWit $
                MkShimWit (GroundPinaforeSingularType (EntityPinaforeGroundType ct gt) dargs) conv

concreteEntityToPinaforeType ::
       forall polarity t. Is PolarityType polarity
    => ConcreteEntityType t
    -> Maybe (PinaforeShimWit polarity t)
concreteEntityToPinaforeType et =
    case representative @_ @_ @polarity of
        PositiveType -> return $ concreteEntityToPositivePinaforeType et
        NegativeType -> concreteEntityToMaybeNegativePinaforeType et

type PinaforeExpression = SealedExpression Name (PinaforeShimWit 'Negative) (PinaforeShimWit 'Positive)

type PinaforePatternConstructor = PatternConstructor Name (PinaforeShimWit 'Positive) (PinaforeShimWit 'Negative)

type PinaforePattern = SealedPattern Name (PinaforeShimWit 'Positive) (PinaforeShimWit 'Negative)

data PinaforeTypeSystem

type instance ScopeExpression PinaforeTypeSystem =
     PinaforeExpression

type instance ScopePatternConstructor PinaforeTypeSystem =
     PinaforePatternConstructor

type instance ScopeProvidedType PinaforeTypeSystem = ProvidedType

type instance ScopeClosedEntityType PinaforeTypeSystem =
     ClosedEntityType

type PinaforeNamedType = NamedType PinaforeTypeSystem

type PinaforeTypeBox = TypeBox PinaforeTypeSystem

type PinaforeScoped = Scoped PinaforeTypeSystem

type PinaforeSourceScoped = SourceScoped PinaforeTypeSystem

type PinaforeTypeCheck = VarRenamerT PinaforeTypeSystem (PinaforeSourceScoped)
