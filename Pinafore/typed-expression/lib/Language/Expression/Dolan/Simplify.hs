{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Simplify
    ( SimplifierSettings(..)
    , defaultSimplifierSettings
    ) where

import Language.Expression.Dolan.Simplify.AutomateRecursion
import Language.Expression.Dolan.Simplify.DuplicateGroundTypes
import Language.Expression.Dolan.Simplify.DuplicateTypeVars
import Language.Expression.Dolan.Simplify.FullyConstrainedTypeVars
import Language.Expression.Dolan.Simplify.OneSidedTypeVars
import Language.Expression.Dolan.Simplify.RollUpRecursion
import Language.Expression.Dolan.Simplify.Safety
import Language.Expression.Dolan.Simplify.SharedTypeVars
import Language.Expression.Dolan.Simplify.UnusedRecursion
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem
import Shapes

data SimplifierSettings = MkSimplifierSettings
    { simplifyAny :: Bool
    , simplifyCheckSafetyBefore :: Bool
    , simplifyAutomateRecursion :: Bool
    , simplifyEliminateUnusedRecursion :: Bool
    , simplifyMergeDuplicateGroundTypes :: Bool
    , simplifyEliminateOneSidedTypeVars :: Bool
    , simplifyFullyConstrainedTypeVars :: Bool
    , simplifyMergeSharedTypeVars :: Bool
    , simplifyMergeDuplicateTypeVars :: Bool
    , simplifyRollUpRecursiveTypes :: Bool
    , simplifyCheckSafetyAfter :: Bool
    }

defaultSimplifierSettings :: SimplifierSettings
defaultSimplifierSettings =
    MkSimplifierSettings
        { simplifyAny = True
        , simplifyCheckSafetyBefore = False
        , simplifyAutomateRecursion = True
        , simplifyEliminateUnusedRecursion = False
        , simplifyMergeDuplicateGroundTypes = True
        , simplifyEliminateOneSidedTypeVars = False
        , simplifyFullyConstrainedTypeVars = True
        , simplifyMergeSharedTypeVars = True
        , simplifyMergeDuplicateTypeVars = True
        , simplifyRollUpRecursiveTypes = True
        , simplifyCheckSafetyAfter = False
        }

simplifierSettingsINTERNAL :: SimplifierSettings
simplifierSettingsINTERNAL = defaultSimplifierSettings

-- Simplification:
--
-- automateRecursion: simplify complex recursive types using automata
-- e.g. "rec a, Maybe. (rec b, a | Maybe. b)" => "rec a, Maybe a"
--
-- eliminateUnusedRecursion: remove unused recursion & eliminate immediate recursion
-- e.g. "rec a, Integer" => "Integer"
-- This is usually switched off since automateRecursion does this.
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
    case simplifierSettingsINTERNAL of
        MkSimplifierSettings {..} ->
            mif simplifyAny $
            mconcat
                [ mif simplifyCheckSafetyBefore $ checkSafetyMappable @ground "before"
                , mif simplifyAutomateRecursion $ automateRecursion @ground
                , mif simplifyEliminateUnusedRecursion $ endoToEndoM $ eliminateUnusedRecursion @ground
                , mif simplifyMergeDuplicateGroundTypes $ mergeDuplicateGroundTypes @ground
                , mif simplifyEliminateOneSidedTypeVars $ endoToEndoM $ eliminateOneSidedTypeVars @ground
                , mif simplifyFullyConstrainedTypeVars $ fullyConstrainedTypeVars @ground
                , mif simplifyMergeSharedTypeVars $ endoToEndoM $ mergeSharedTypeVars @ground
                , mif simplifyMergeDuplicateTypeVars $ endoToEndoM $ mergeDuplicateTypeVars @ground
                , mif simplifyCheckSafetyBefore $ checkSafetyMappable @ground "middle"
                , mif simplifyRollUpRecursiveTypes $ endoToEndoM $ rollUpRecursiveTypes @ground
                , mif simplifyCheckSafetyAfter $ checkSafetyMappable @ground "after"
                ]

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground =>
             SimplifyTypeSystem (DolanTypeSystem ground) where
    simplify = dolanSimplifyTypes @ground
