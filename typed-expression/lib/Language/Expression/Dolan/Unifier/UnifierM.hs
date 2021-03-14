module Language.Expression.Dolan.Unifier.UnifierM where

import Data.Shim
import Language.Expression.Dolan.Invert
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type UnifierError :: GroundTypeKind -> Type
data UnifierError ground where
    UninvertibleError
        :: forall (ground :: GroundTypeKind) polarity t. Is PolarityType polarity
        => DolanType ground polarity t
        -> UnifierError ground

type UnifierM :: GroundTypeKind -> Type -> Type
type UnifierM ground = Result (UnifierError ground)

runUnifierM ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => UnifierM ground a
    -> DolanM ground a
runUnifierM (SuccessResult a) = return a
runUnifierM (FailureResult (UninvertibleError t)) = throwTypeNotInvertible t

invertTypeM ::
       forall (ground :: GroundTypeKind) polarity a. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity a
    -> UnifierM ground (DolanShimWit ground (InvertPolarity polarity) a)
invertTypeM t =
    case invertTypeMaybe t of
        Just r -> return r
        Nothing -> FailureResult (UninvertibleError t)

invertType ::
       forall (ground :: GroundTypeKind) polarity a. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity a
    -> DolanM ground (DolanShimWit ground (InvertPolarity polarity) a)
invertType t = runUnifierM $ invertTypeM t
