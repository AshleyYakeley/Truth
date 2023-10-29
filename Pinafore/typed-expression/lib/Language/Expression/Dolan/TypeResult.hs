module Language.Expression.Dolan.TypeResult where

import Data.Shim
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type TypeError :: GroundTypeKind -> Type
data TypeError ground where
    InternalTypeError :: forall (ground :: GroundTypeKind). Text -> TypeError ground
    InternalSafetyError
        :: forall (ground :: GroundTypeKind) polarity t. Is PolarityType polarity
        => Text
        -> RecursiveTypeError
        -> DolanType ground polarity t
        -> TypeError ground
    UninvertibleTypeError
        :: forall (ground :: GroundTypeKind) polarity t. Is PolarityType polarity
        => DolanType ground polarity t
        -> TypeError ground
    NoGroundConvertTypeError
        :: forall (ground :: GroundTypeKind) dva ga dvb gb. ground dva ga -> ground dvb gb -> TypeError ground
    IncoherentGroundConvertTypeError
        :: forall (ground :: GroundTypeKind) dva ga dvb gb. ground dva ga -> ground dvb gb -> TypeError ground
    ConvertTypeError
        :: forall (ground :: GroundTypeKind) ta tb.
           FlipType ground 'Positive ta
        -> FlipType ground 'Negative tb
        -> TypeError ground

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => Show (TypeError ground) where
    show (InternalTypeError _) = "INTERNAL"
    show (InternalSafetyError _ _ _) = "INTERNAL safety"
    show (UninvertibleTypeError _) = "uninvertible"
    show (NoGroundConvertTypeError _ _) = "no ground conversion"
    show (IncoherentGroundConvertTypeError _ _) = "incoherent ground conversions"
    show (ConvertTypeError _ _) = "no conversion"

type TypeResult :: GroundTypeKind -> Type -> Type
type TypeResult ground = Result (TypeError ground)

joinFirstResult :: Result err a -> Result err a -> Result err a
joinFirstResult (FailureResult _) r = r
joinFirstResult (SuccessResult a) _ = SuccessResult a

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground =>
             MonadThrow (TypeError ground) (TypeResult ground) where
    throw = throwExc
