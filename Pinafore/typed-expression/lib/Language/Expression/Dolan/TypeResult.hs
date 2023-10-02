module Language.Expression.Dolan.TypeResult where

import Data.Shim
import Language.Expression.Dolan.FlipType
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type TypeError :: GroundTypeKind -> Type
data TypeError ground where
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

type TypeResult :: GroundTypeKind -> Type -> Type
type TypeResult ground = Result (TypeError ground)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground =>
             MonadThrow (TypeError ground) (TypeResult ground) where
    throw = throwExc

newtype CrumbleM (ground :: GroundTypeKind) a = MkCrumbleM
    { unCrumbleM :: ComposeInner (TypeResult ground) (DolanTypeCheckM ground) a
    }

deriving newtype instance
         forall (ground :: GroundTypeKind) . IsDolanGroundType ground =>
                                             Functor (CrumbleM ground)

deriving newtype instance
         forall (ground :: GroundTypeKind) . IsDolanGroundType ground =>
                                             Applicative (CrumbleM ground)

deriving newtype instance
         forall (ground :: GroundTypeKind) . IsDolanGroundType ground =>
                                             Monad (CrumbleM ground)

deriving newtype instance
         forall (ground :: GroundTypeKind) . (IsDolanGroundType ground,
                                              MonadIO (DolanM ground)) =>
                                             MonadIO (CrumbleM ground)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => MonadThrow (TypeError ground) (CrumbleM ground) where
    throw err = liftResultToCrumbleM $ throw err

liftToCrumbleM ::
       forall (ground :: GroundTypeKind). IsDolanGroundType ground
    => DolanTypeCheckM ground --> CrumbleM ground
liftToCrumbleM tca = MkCrumbleM $ MkComposeInner $ fmap return tca

liftResultToCrumbleM ::
       forall (ground :: GroundTypeKind). IsDolanGroundType ground
    => TypeResult ground --> CrumbleM ground
liftResultToCrumbleM rea = MkCrumbleM $ MkComposeInner $ return rea

runCrumbleMResult ::
       forall (ground :: GroundTypeKind) a. CrumbleM ground a -> DolanTypeCheckM ground (TypeResult ground a)
runCrumbleMResult (MkCrumbleM (MkComposeInner mra)) = mra

crumbleMGetResult ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => CrumbleM ground a
    -> CrumbleM ground (TypeResult ground a)
crumbleMGetResult ca = liftToCrumbleM $ runCrumbleMResult ca
