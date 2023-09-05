{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Unifier.Substitution where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier.FlipType
import Language.Expression.Dolan.Unifier.UnifierM
import Shapes

type Substitution :: GroundTypeKind -> Type
data Substitution ground where
    MkSubstitution
        :: forall (ground :: GroundTypeKind) polarity nv t.
           PolarityType polarity
        -> TypeVarT (JoinMeetType polarity nv t)
        -> TypeVarT nv
        -> UnifierM ground (DolanShimWit ground polarity (JoinMeetType polarity nv t))
        -> Maybe (DolanType ground (InvertPolarity polarity) t)
        -> Substitution ground

substBisubstitution ::
       forall (ground :: GroundTypeKind). IsDolanGroundType ground
    => Substitution ground
    -> UnifierBisubstitution ground
substBisubstitution (MkSubstitution (pol :: _ polarity) oldvar newvar mt _) =
    withRepresentative pol $
    withInvertPolarity @polarity $ let
        newVarWit = shimWitToDolan $ MkShimWit (VarDolanSingularType newvar) $ invertPolarMap polar1
        in mkPolarBisubstitution oldvar mt $ return newVarWit
