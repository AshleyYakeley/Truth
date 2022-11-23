module Pinafore.Language.Type.Type where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Language.Interpreter
import Pinafore.Language.Shim
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Subtype ()
import Shapes

type QSingularType :: Polarity -> Type -> Type
type QSingularType = DolanSingularType QGroundType

type QSingularShimWit :: Polarity -> Type -> Type
type QSingularShimWit polarity = DolanSingularShimWit QGroundType polarity

type QGroundedType :: Polarity -> Type -> Type
type QGroundedType = DolanGroundedType QGroundType

type QGroundedShimWit :: Polarity -> Type -> Type
type QGroundedShimWit polarity = DolanGroundedShimWit QGroundType polarity

type QType :: Polarity -> Type -> Type
type QType = DolanType QGroundType

type QShimWit :: Polarity -> Type -> Type
type QShimWit polarity = DolanShimWit QGroundType polarity

type QIsoShimWit :: Polarity -> Type -> Type
type QIsoShimWit polarity = DolanIsoShimWit QGroundType polarity

type QArgumentsShimWit :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Polarity -> Type -> Type
type QArgumentsShimWit dv gt polarity = DolanArgumentsShimWit QPolyShim dv QType gt polarity

type QValue = TSValue QTypeSystem

type QValueF f = TSValueF QTypeSystem f

type QOpenExpression = TSOpenExpression QTypeSystem

type QExpression = TSSealedExpression QTypeSystem

type QPatternConstructor = TSExpressionPatternConstructor QTypeSystem

type QRecordConstructor = RecordConstructor QTypeSystem

type QSignature = Signature QTypeSystem

type QRecordPattern = RecordPattern QTypeSystem

type QOpenPattern = TSOpenPattern QTypeSystem

type QPattern = TSSealedExpressionPattern QTypeSystem

type instance InterpreterFamilyType QTypeSystem = FamilialType

type QSpecialVals = SpecialVals QTypeSystem

type QBoundType = InterpreterBoundType QTypeSystem

type QScope = Scope QTypeSystem

type QModule = Module QTypeSystem

type QBinding = TSBinding QTypeSystem

type QInterpreterBinding = InterpreterBinding QTypeSystem

type QBindingInfo = BindingInfo QTypeSystem

type QInterpreter = Interpreter QTypeSystem

type QScopeInterpreter = ScopeInterpreter QTypeSystem

type QAnnotation = Annotation QTypeSystem

type QSpecialForm = SpecialForm QTypeSystem QInterpreter

type QFixBox = ScopeFixBox QTypeSystem
