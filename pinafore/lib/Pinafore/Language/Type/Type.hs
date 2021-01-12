module Pinafore.Language.Type.Type where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Language.Interpret.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type.Entity
import Pinafore.Language.Type.Ground
import Shapes

type PinaforeSingularType :: Polarity -> Type -> Type
type PinaforeSingularType = DolanSingularType PinaforeGroundType

type PinaforeSingularShimWit :: Polarity -> Type -> Type
type PinaforeSingularShimWit polarity = PShimWit (PinaforePolyShim Type) PinaforeSingularType polarity

type PinaforeType :: Polarity -> Type -> Type
type PinaforeType = DolanType PinaforeGroundType

type PinaforeShimWit :: Polarity -> Type -> Type
type PinaforeShimWit polarity = PShimWit (PinaforePolyShim Type) PinaforeType polarity

type PinaforeExpression = SealedExpression Name (PinaforeShimWit 'Negative) (PinaforeShimWit 'Positive)

type PinaforePatternConstructor = PatternConstructor Name (PinaforeShimWit 'Positive) (PinaforeShimWit 'Negative)

type PinaforePattern = SealedPattern Name (PinaforeShimWit 'Positive) (PinaforeShimWit 'Negative)

type instance InterpreterProvidedType PinaforeTypeSystem =
     ProvidedType

type instance InterpreterClosedEntityType PinaforeTypeSystem =
     ClosedEntityType

type PinaforeSpecialVals = SpecialVals PinaforeTypeSystem

type PinaforeBoundType = BoundType PinaforeTypeSystem

type PinaforeTypeBox = TypeBox PinaforeTypeSystem

type PinaforeScope = Scope PinaforeTypeSystem

type PinaforeBinding = InterpreterBinding PinaforeTypeSystem

type PinaforeInterpreter = Interpreter PinaforeTypeSystem

type PinaforeSourceInterpreter = SourceInterpreter PinaforeTypeSystem

type PinaforeAnnotation = Annotation PinaforeTypeSystem

type PinaforeSpecialForm = SpecialForm PinaforeTypeSystem PinaforeSourceInterpreter
