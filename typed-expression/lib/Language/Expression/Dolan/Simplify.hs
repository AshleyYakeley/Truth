{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Simplify
    (
    ) where

import Language.Expression.Common
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Simplify.DuplicateGroundTypes
import Language.Expression.Dolan.Simplify.DuplicateTypeVars
import Language.Expression.Dolan.Simplify.FullyConstrainedTypeVars
import Language.Expression.Dolan.Simplify.OneSidedTypeVars
import Language.Expression.Dolan.Simplify.RollUpRecursion
import Language.Expression.Dolan.Simplify.SharedTypeVars
import Language.Expression.Dolan.Simplify.UnusedRecursion
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type Simplifier :: GroundTypeKind -> Type
newtype Simplifier ground = MkSimplifier
    { runSimplifier :: forall a.
                           PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a =>
                                   a -> DolanTypeCheckM ground a
    }

pureSimplifier ::
       forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground
    => (forall a. PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a => a -> a)
    -> Simplifier ground
pureSimplifier aa = MkSimplifier $ \a -> return $ aa a

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground => Semigroup (Simplifier ground) where
    MkSimplifier p <> MkSimplifier q =
        MkSimplifier $ \a -> do
            a' <- p a
            q a'

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground => Monoid (Simplifier ground) where
    mempty = MkSimplifier return

-- Simplification:
--
-- eliminateUnusedRecursion: remove unused recursion & eliminate immediate recursion
-- e.g. "rec a. Integer" => "Integer"
-- e.g. "rec a. a" => "Any"/"None"
--
-- mergeDuplicateGroundTypes: merge duplicate ground types in join/meet (on each type)
-- e.g. "[a]|[b]" => "[a|b]"
--
-- eliminateOneSidedTypeVars: eliminate one-sided type vars (on whole expression)
-- Type vars are one-sided if they appear only in positive position, or only in negative position.
-- e.g. "a -> [b]" => "Any -> [None]"
-- This is usually switched off since fullyConstrainedTypeVars does this.
--
-- fullyConstrainedTypeVars: eliminate fully-constrained vars (on whole expression)
-- Type vars are fully constrained if their positive and negative constraints overlap.
-- e.g. "(a & Integer) -> (a | Number)" => "Integer -> Number"
--
-- mergeSharedTypeVars: merge shared type vars (on whole expression)
-- Two type variables are shared, if they always appear together (join/meet) in the positive positions, or in the negative positions.
-- e.g. "a -> b -> a|b" => "a -> a -> a|a"
--
-- mergeDuplicateTypeVars: merge duplicate type vars in join/meet (on each type)
-- e.g. "a|a" => "a"
--
-- rollUpRecursiveTypes: roll up recursive types
-- e.g. "F (rec a. F a)" => "rec a. F a"
dolanSimplifyTypes ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanSubtypeGroundType ground, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => a
    -> DolanTypeCheckM ground a
dolanSimplifyTypes =
    runSimplifier $
    mif True $
    mconcat
        [ mif True $ pureSimplifier $ eliminateUnusedRecursion @ground
        , mif True $ MkSimplifier $ mergeDuplicateGroundTypes @ground
        , mif False $ pureSimplifier $ eliminateOneSidedTypeVars @ground
        , mif True $ MkSimplifier $ fullyConstrainedTypeVars @ground
        , mif True $ pureSimplifier $ mergeSharedTypeVars @ground
        , mif True $ pureSimplifier $ mergeDuplicateTypeVars @ground
        , mif True $ pureSimplifier $ rollUpRecursiveTypes @ground
        ]

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground =>
             SimplifyTypeSystem (DolanTypeSystem ground) where
    simplify = dolanSimplifyTypes @ground
