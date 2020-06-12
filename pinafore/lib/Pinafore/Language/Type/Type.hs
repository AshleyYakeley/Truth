module Pinafore.Language.Type.Type where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Language.Name
import Pinafore.Language.Scope
import Pinafore.Language.Shim
import Pinafore.Language.Type.Entity
import Pinafore.Language.Type.Ground
import Shapes

type PinaforeSingularType :: Polarity -> Type -> Type
type PinaforeSingularType = DolanSingularType PinaforeGroundType

type PinaforePlainType :: Polarity -> Type -> Type
type PinaforePlainType = DolanPlainType PinaforeGroundType

type PinaforePlainShimWit :: Polarity -> Type -> Type
type PinaforePlainShimWit polarity = PShimWit (PinaforePolyShim Type) PinaforePlainType polarity

type PinaforeType :: Polarity -> Type -> Type
type PinaforeType = DolanType PinaforeGroundType

type PinaforeShimWit :: Polarity -> Type -> Type
type PinaforeShimWit polarity = PShimWit (PinaforePolyShim Type) PinaforeType polarity

type PinaforeExpression = SealedExpression Name (PinaforeShimWit 'Negative) (PinaforeShimWit 'Positive)

type PinaforePatternConstructor = PatternConstructor Name (PinaforeShimWit 'Positive) (PinaforeShimWit 'Negative)

type PinaforePattern = SealedPattern Name (PinaforeShimWit 'Positive) (PinaforeShimWit 'Negative)

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
