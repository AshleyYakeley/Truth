module Language.Expression.Dolan.Unifier.Piece where

import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier.Substitution
import Language.Expression.Dolan.Unifier.WholeConstraint
import Shapes

type Piece :: GroundTypeKind -> Type -> Type
data Piece ground t where
    MkPiece :: forall (ground :: GroundTypeKind) t. [Substitution ground] -> WholeConstraint ground t -> Piece ground t

instance forall (ground :: GroundTypeKind) t. IsDolanGroundType ground => Show (Piece ground t) where
    show (MkPiece substs wc) = show wc <> " (+" <> show (length substs) <> ")"

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => AllConstraint Show (Piece ground) where
    allConstraint = Dict

applySubstsToPiece :: forall (ground :: GroundTypeKind) t. [Substitution ground] -> Piece ground t -> Piece ground t
applySubstsToPiece newsubsts (MkPiece substs wc) = MkPiece (substs <> newsubsts) wc
