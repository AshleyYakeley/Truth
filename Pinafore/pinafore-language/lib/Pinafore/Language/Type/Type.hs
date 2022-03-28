module Pinafore.Language.Type.Type where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Subtype ()
import Shapes

type PinaforeSingularType :: Polarity -> Type -> Type
type PinaforeSingularType = DolanSingularType PinaforeGroundType

type PinaforeSingularShimWit :: Polarity -> Type -> Type
type PinaforeSingularShimWit polarity = DolanSingularShimWit PinaforeGroundType polarity

type PinaforeType :: Polarity -> Type -> Type
type PinaforeType = DolanType PinaforeGroundType

type PinaforeShimWit :: Polarity -> Type -> Type
type PinaforeShimWit polarity = DolanShimWit PinaforeGroundType polarity

type PinaforeArgumentsShimWit :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Polarity -> Type -> Type
type PinaforeArgumentsShimWit dv gt polarity = DolanArgumentsShimWit PinaforePolyShim dv PinaforeType gt polarity

type PinaforeExpression = SealedExpression Name (PinaforeShimWit 'Negative) (PinaforeShimWit 'Positive)

type PinaforePatternConstructor = PatternConstructor Name (PinaforeShimWit 'Positive) (PinaforeShimWit 'Negative)

type PinaforePattern = SealedPattern Name (PinaforeShimWit 'Positive) (PinaforeShimWit 'Negative)

type instance InterpreterFamilyType PinaforeTypeSystem =
     FamilialType

type PinaforeSpecialVals = SpecialVals PinaforeTypeSystem

type PinaforeBoundType = BoundType PinaforeTypeSystem

type PinaforeScope = Scope PinaforeTypeSystem

type PinaforeModule = Module PinaforeTypeSystem

type PinaforeBinding = InterpreterBinding PinaforeTypeSystem

type PinaforeInterpreter = Interpreter PinaforeTypeSystem

type PinaforeAnnotation = Annotation PinaforeTypeSystem

type PinaforeSpecialForm = SpecialForm PinaforeTypeSystem PinaforeInterpreter
