module Language.Expression.Dolan.Unifier.Piece where

import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier.AtomicConstraint
import Language.Expression.Dolan.Unifier.Substitution
import Language.Expression.Dolan.Unifier.WholeConstraint
import Shapes

type Piece :: GroundTypeKind -> Type -> Type
data Piece ground t where
    WholePiece
        :: forall (ground :: GroundTypeKind) t. [Substitution ground] -> WholeConstraint ground t -> Piece ground t
    AtomicPiece :: forall (ground :: GroundTypeKind) t. AtomicConstraint ground t -> Piece ground t

instance forall (ground :: GroundTypeKind) t. IsDolanGroundType ground => Show (Piece ground t) where
    show (WholePiece substs wc) = "whole: " <> show wc <> " (+" <> show (length substs) <> ")"
    show (AtomicPiece ac) = "atomic: " <> show ac

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => AllConstraint Show (Piece ground) where
    allConstraint = Dict
