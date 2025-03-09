module Language.Expression.Dolan.TypeResult where

import Data.Shim
import Shapes

import Language.Expression.Common
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem

type TypeError :: GroundTypeKind -> Type
data TypeError ground where
    PatternTypeError :: forall (ground :: GroundTypeKind). PatternError -> TypeError ground
    ExpressionTypeError :: forall (ground :: GroundTypeKind). ExpressionError (DolanVarWit ground) -> TypeError ground
    InternalTypeError :: forall (ground :: GroundTypeKind). Text -> TypeError ground
    InternalSafetyTypeError ::
        forall (ground :: GroundTypeKind) polarity t.
        Is PolarityType polarity =>
        Text ->
        RecursiveTypeError ->
        DolanType ground polarity t ->
        TypeError ground
    UninvertibleTypeError ::
        forall (ground :: GroundTypeKind) polarity t.
        Is PolarityType polarity =>
        DolanType ground polarity t ->
        TypeError ground
    NoGroundConvertTypeError ::
        forall (ground :: GroundTypeKind) dva ga dvb gb. ground dva ga -> ground dvb gb -> TypeError ground
    IncoherentGroundConvertTypeError ::
        forall (ground :: GroundTypeKind) dva ga dvb gb. ground dva ga -> ground dvb gb -> TypeError ground
    ConvertTypeError ::
        forall (ground :: GroundTypeKind) ta tb.
        FlipType ground 'Positive ta ->
        FlipType ground 'Negative tb ->
        TypeError ground

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => Show (TypeError ground) where
    show (PatternTypeError _) = "pattern"
    show (ExpressionTypeError _) = "expression"
    show (InternalTypeError _) = "INTERNAL"
    show (InternalSafetyTypeError _ _ _) = "INTERNAL safety"
    show (UninvertibleTypeError _) = "uninvertible"
    show (NoGroundConvertTypeError _ _) = "no ground conversion"
    show (IncoherentGroundConvertTypeError _ _) = "incoherent ground conversions"
    show (ConvertTypeError _ _) = "no conversion"

type TypeResult :: GroundTypeKind -> Type -> Type
type TypeResult ground = Result (TypeError ground)

joinFirstResult :: Result err a -> Result err a -> Result err a
joinFirstResult (FailureResult _) r = r
joinFirstResult (SuccessResult a) _ = SuccessResult a

instance forall (ground :: GroundTypeKind). MonadThrow (TypeError ground) (TypeResult ground) where
    throw = throwExc

instance forall (ground :: GroundTypeKind). MonadThrow PatternError (TypeResult ground) where
    throw err = throwExc $ PatternTypeError err

instance forall (ground :: GroundTypeKind) vw. vw ~ DolanVarWit ground => MonadThrow (ExpressionError vw) (TypeResult ground) where
    throw err = throwExc $ ExpressionTypeError err
