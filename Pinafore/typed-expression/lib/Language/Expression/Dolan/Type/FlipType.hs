module Language.Expression.Dolan.Type.FlipType where

import Data.Shim
import Shapes

import Language.Expression.Dolan.FreeVars
import Language.Expression.Dolan.Type.DolanType
import Language.Expression.Dolan.Type.Equality ()
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

type FlipType :: GroundTypeKind -> Polarity -> Type -> Type
data FlipType ground polarity t
    = NormalFlipType (DolanType ground polarity t)
    | InvertFlipType (DolanType ground (InvertPolarity polarity) t)

instance
    forall (ground :: GroundTypeKind) polarity.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    TestEquality (FlipType ground polarity)
    where
    testEquality (NormalFlipType t1) (NormalFlipType t2) = do
        Refl <- testEquality t1 t2
        return Refl
    testEquality (InvertFlipType t1) (InvertFlipType t2) =
        withInvertPolarity @polarity $ do
            Refl <- testEquality t1 t2
            return Refl
    testEquality _ _ = Nothing

instance forall (ground :: GroundTypeKind) polarity t. FreeTypeVariables (FlipType ground polarity t) where
    freeTypeVariables (NormalFlipType t) = freeTypeVariables t
    freeTypeVariables (InvertFlipType t) = freeTypeVariables t

instance
    forall (ground :: GroundTypeKind) polarity t.
    (ShowGroundType ground, Is PolarityType polarity) =>
    Show (FlipType ground polarity t)
    where
    show (NormalFlipType t) = allShow t
    show (InvertFlipType t) = withInvertPolarity @polarity $ allShow t <> " [inv]"

instance
    forall (ground :: GroundTypeKind) polarity.
    (ShowGroundType ground, Is PolarityType polarity) =>
    AllConstraint Show (FlipType ground polarity)
    where
    allConstraint = Dict

toFlipType ::
    forall (ground :: GroundTypeKind) pola polb t.
    (Is PolarityType pola, Is PolarityType polb) =>
    DolanType ground pola t ->
    FlipType ground polb t
toFlipType =
    case samePolarityType (polarityType @pola) (polarityType @polb) of
        Left Refl -> NormalFlipType
        Right Refl -> InvertFlipType

flipToType ::
    forall (ground :: GroundTypeKind) pola t r.
    Is PolarityType pola =>
    FlipType ground pola t ->
    (forall polb. Is PolarityType polb => DolanType ground polb t -> r) ->
    r
flipToType (NormalFlipType t) call = call t
flipToType (InvertFlipType t) call = withInvertPolarity @pola $ call t

occursInFlipType :: forall (ground :: GroundTypeKind) polarity tv a. TypeVarT tv -> FlipType ground polarity a -> Bool
occursInFlipType v (NormalFlipType t) = variableOccursIn v t
occursInFlipType v (InvertFlipType t) = variableOccursIn v t
