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
type PinaforeSingularShimWit polarity = PShimWit (PinaforePolyShim Type) PinaforeSingularType polarity

type PinaforeType :: Polarity -> Type -> Type
type PinaforeType = DolanType PinaforeGroundType

type PinaforeShimWit :: Polarity -> Type -> Type
type PinaforeShimWit polarity = PShimWit (PinaforePolyShim Type) PinaforeType polarity

type PinaforeExpression = SealedExpression Name (PinaforeShimWit 'Negative) (PinaforeShimWit 'Positive)

type PinaforePatternConstructor = PatternConstructor Name (PinaforeShimWit 'Positive) (PinaforeShimWit 'Negative)

type PinaforePattern = SealedPattern Name (PinaforeShimWit 'Positive) (PinaforeShimWit 'Negative)

type instance InterpreterFamilyType PinaforeTypeSystem = FamilyType

type PinaforeSpecialVals = SpecialVals PinaforeTypeSystem

type PinaforeBoundType = BoundType PinaforeTypeSystem

type PinaforeScope = Scope PinaforeTypeSystem

type PinaforeModule = Module PinaforeTypeSystem

type PinaforeBinding = InterpreterBinding PinaforeTypeSystem

type PinaforeInterpreter = Interpreter PinaforeTypeSystem

type PinaforeAnnotation = Annotation PinaforeTypeSystem

type PinaforeSpecialForm = SpecialForm PinaforeTypeSystem PinaforeInterpreter

getGreatestDynamicSupertype :: PinaforeType 'Positive t -> PinaforeInterpreter (PinaforeGreatestDynamicSupertype t)
getGreatestDynamicSupertype (ConsDolanType (GroundedDolanSingularType gt args) NilDolanType)
    | Just ds <- pgtGreatestDynamicSupertype gt args =
        return $ mapPolarShimWit (MkPolarMap $ applyCoPolyShim ccrVariation ccrVariation id iJoinR1) ds
getGreatestDynamicSupertype t = do
    t' <- invertType t
    return $ mapPolarShimWit (MkPolarMap $ functionToShim "Just" Just) t'
