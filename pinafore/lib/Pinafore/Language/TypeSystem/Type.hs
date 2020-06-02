module Pinafore.Language.TypeSystem.Type where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Language.Error
import Pinafore.Language.Name
import Pinafore.Language.Scope
import Pinafore.Language.Shim
import Pinafore.Language.Type.Entity
import Pinafore.Language.Type.Ground
import Shapes

type PinaforeSingularType :: Polarity -> Type -> Type
type PinaforeSingularType = DolanSingularType PinaforeGroundType

type PinaforeType :: Polarity -> Type -> Type
type PinaforeType = DolanType PinaforeGroundType

type PinaforeTypeShimWit :: Polarity -> Type -> Type
type PinaforeTypeShimWit polarity = PShimWit (PinaforeShim Type) PinaforeType polarity

pinaforeToConcreteEntityArgs ::
       forall (polarity :: Polarity) dv f t. (Is PolarityType polarity)
    => CovaryType dv
    -> CovaryMap f
    -> DolanArguments dv PinaforeType f polarity t
    -> Maybe (ShimWit (PolyIso PinaforeShim Type) (Arguments ConcreteEntityType f) polarity t)
pinaforeToConcreteEntityArgs = dolanArgumentsToArgumentsM pinaforeToConcreteEntityType

pinaforeEntityToConcreteEntityType ::
       forall (polarity :: Polarity) dv f a. Is PolarityType polarity
    => CovaryType dv
    -> EntityGroundType f
    -> DolanArguments dv PinaforeType f polarity a
    -> Maybe (ShimWit (PolyIso PinaforeShim Type) ConcreteEntityType polarity a)
pinaforeEntityToConcreteEntityType lc gt args = do
    MkShimWit eargs conv <- pinaforeToConcreteEntityArgs lc (entityGroundTypeCovaryMap gt) args
    return $ MkShimWit (MkConcreteType gt eargs) conv

pinaforeSingularToConcreteEntityType ::
       forall polarity a. Is PolarityType polarity
    => PinaforeSingularType polarity a
    -> Maybe (ShimWit (PolyIso PinaforeShim Type) ConcreteEntityType polarity a)
pinaforeSingularToConcreteEntityType (GroundDolanSingularType (EntityPinaforeGroundType lc gt) args) =
    pinaforeEntityToConcreteEntityType lc gt args
pinaforeSingularToConcreteEntityType _ = Nothing

pinaforeToConcreteEntityType ::
       forall polarity a. Is PolarityType polarity
    => PinaforeType polarity a
    -> Maybe (ShimWit (PolyIso PinaforeShim Type) ConcreteEntityType polarity a)
pinaforeToConcreteEntityType (ConsDolanType t NilDolanType) = do
    MkShimWit et conv <- pinaforeSingularToConcreteEntityType t
    return $ MkShimWit et $ conv <.> polarPolyIsoPolar1
pinaforeToConcreteEntityType _ = Nothing

concreteEntityToMaybeNegativePinaforeType :: forall t. ConcreteEntityType t -> Maybe (PinaforeTypeShimWit 'Negative t)
concreteEntityToMaybeNegativePinaforeType (MkConcreteType gt args) =
    entityGroundTypeCovaryType gt $ \ct -> do
        MkShimWit dargs conv <-
            argumentsToDolanArgumentsM concreteEntityToMaybeNegativePinaforeType ct (entityGroundTypeCovaryMap gt) args
        return $ singleDolanShimWit $ MkShimWit (GroundDolanSingularType (EntityPinaforeGroundType ct gt) dargs) conv

concreteEntityToNegativePinaforeType ::
       forall m t. MonadThrow ErrorType m
    => ConcreteEntityType t
    -> m (PinaforeTypeShimWit 'Negative t)
concreteEntityToNegativePinaforeType et =
    case concreteEntityToMaybeNegativePinaforeType et of
        Just wit -> return wit
        Nothing -> throw InterpretTypeNoneNotNegativeEntityError

concreteEntityToPositivePinaforeType :: forall t. ConcreteEntityType t -> PinaforeTypeShimWit 'Positive t
concreteEntityToPositivePinaforeType (MkConcreteType gt args) =
    entityGroundTypeCovaryType gt $ \ct ->
        case argumentsToDolanArguments concreteEntityToPositivePinaforeType ct (entityGroundTypeCovaryMap gt) args of
            MkShimWit dargs conv ->
                singleDolanShimWit $ MkShimWit (GroundDolanSingularType (EntityPinaforeGroundType ct gt) dargs) conv

concreteEntityToPinaforeType ::
       forall polarity t. Is PolarityType polarity
    => ConcreteEntityType t
    -> Maybe (PinaforeTypeShimWit polarity t)
concreteEntityToPinaforeType et =
    case polarityType @polarity of
        PositiveType -> return $ concreteEntityToPositivePinaforeType et
        NegativeType -> concreteEntityToMaybeNegativePinaforeType et

type PinaforeExpression = SealedExpression Name (PinaforeTypeShimWit 'Negative) (PinaforeTypeShimWit 'Positive)

type PinaforePatternConstructor
     = PatternConstructor Name (PinaforeTypeShimWit 'Positive) (PinaforeTypeShimWit 'Negative)

type PinaforePattern = SealedPattern Name (PinaforeTypeShimWit 'Positive) (PinaforeTypeShimWit 'Negative)

type PinaforeTypeSystem = DolanTypeSystem PinaforeGroundType

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

type PinaforeTypeCheck = VarRenamerT PinaforeTypeSystem PinaforeSourceScoped
