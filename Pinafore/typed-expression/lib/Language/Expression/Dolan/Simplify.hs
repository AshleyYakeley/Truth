{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Simplify
    ( SimplifierSettings(..)
    , defaultSimplifierSettings
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

data SimplifierSettings = MkSimplifierSettings
    { simplifyAny :: Bool
    , simplifyEliminateUnusedRecursion :: Bool
    , simplifyMergeDuplicateGroundTypes :: Bool
    , simplifyEliminateOneSidedTypeVars :: Bool
    , simplifyFullyConstrainedTypeVars :: Bool
    , simplifyMergeSharedTypeVars :: Bool
    , simplifyMergeDuplicateTypeVars :: Bool
    , simplifyRollUpRecursiveTypes :: Bool
    }

defaultSimplifierSettings :: SimplifierSettings
defaultSimplifierSettings =
    MkSimplifierSettings
        { simplifyAny = True
        , simplifyEliminateUnusedRecursion = True
        , simplifyMergeDuplicateGroundTypes = True
        , simplifyEliminateOneSidedTypeVars = False
        , simplifyFullyConstrainedTypeVars = True
        , simplifyMergeSharedTypeVars = True
        , simplifyMergeDuplicateTypeVars = True
        , simplifyRollUpRecursiveTypes = True
        }

simplifierSettings :: SimplifierSettings
simplifierSettings = defaultSimplifierSettings

-- Simplification:
--
-- eliminateUnusedRecursion: remove unused recursion & eliminate immediate recursion
-- e.g. "rec a, Integer" => "Integer"
-- e.g. "rec a, a" => "Any"/"None"
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
-- e.g. "F (rec a, F a)" => "rec a, F a"
dolanSimplifyTypes ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanSubtypeGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a)
    => EndoM (DolanTypeCheckM ground) a
dolanSimplifyTypes =
    case simplifierSettings of
        MkSimplifierSettings {..} ->
            mif simplifyAny $
            mconcat
                [ mif simplifyEliminateUnusedRecursion $ endoToEndoM $ eliminateUnusedRecursion @ground
                , mif simplifyMergeDuplicateGroundTypes $ mergeDuplicateGroundTypes @ground
                , mif simplifyEliminateOneSidedTypeVars $ endoToEndoM $ eliminateOneSidedTypeVars @ground
                , mif simplifyFullyConstrainedTypeVars $ fullyConstrainedTypeVars @ground
                , mif simplifyMergeSharedTypeVars $ endoToEndoM $ mergeSharedTypeVars @ground
                , mif simplifyMergeDuplicateTypeVars $ endoToEndoM $ mergeDuplicateTypeVars @ground
                , mif simplifyRollUpRecursiveTypes $ endoToEndoM $ rollUpRecursiveTypes @ground
                ]

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground =>
             SimplifyTypeSystem (DolanTypeSystem ground) where
    simplify = dolanSimplifyTypes @ground
