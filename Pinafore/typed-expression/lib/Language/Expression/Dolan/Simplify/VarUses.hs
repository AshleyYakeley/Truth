module Language.Expression.Dolan.Simplify.VarUses
    ( mappableGetVarUses
    , mappableGetVars
    , Appearance
    , mappableGetAppearances
    , appearanceMatchingTypes
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Argument
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Occur
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type Appearance :: GroundTypeKind -> Polarity -> Type
newtype Appearance ground polarity =
    MkAppearance [AnyW (DolanSingularType ground polarity)]

appearanceMatchingType ::
       forall (ground :: GroundTypeKind) polarity.
       AnyW SymbolType
    -> Appearance ground polarity
    -> Maybe (AnyW (DolanType ground polarity))
appearanceMatchingType (MkAnyW var) (MkAppearance appr) = let
    matchRemove :: [AnyW (DolanSingularType ground polarity)] -> (Bool, [AnyW (DolanSingularType ground polarity)])
    matchRemove [] = (False, [])
    matchRemove (MkAnyW (VarDolanSingularType var'):rr)
        | Just Refl <- testEquality var var' = let
            (_, tt) = matchRemove rr
            in (True, tt)
    matchRemove (r:rr) = let
        (m, tt) = matchRemove rr
        in (m, r : tt)
    (m', tt') = matchRemove appr
    in if m'
           then Just $ singularsToAnyType tt'
           else Nothing

appearanceMatchingTypes ::
       forall (ground :: GroundTypeKind) polarity.
       AnyW SymbolType
    -> [Appearance ground polarity]
    -> [AnyW (DolanType ground polarity)]
appearanceMatchingTypes var = mapMaybe (appearanceMatchingType var)

removeAppearanceVar ::
       forall (ground :: GroundTypeKind) polarity. IsDolanGroundType ground
    => AnyW SymbolType
    -> Appearance ground polarity
    -> Appearance ground polarity
removeAppearanceVar (MkAnyW var) (MkAppearance appr) = let
    notVar :: AnyW (DolanSingularType ground polarity) -> Bool
    notVar (MkAnyW t) = not $ occursInSingularType var t
    in MkAppearance $ filter notVar appr

typeToAppearance ::
       forall (ground :: GroundTypeKind) polarity t. Is PolarityType polarity
    => DolanType ground polarity t
    -> Appearance ground polarity
typeToAppearance t = MkAppearance $ typeToAnySingulars t

checkVar ::
       forall (ground :: GroundTypeKind) polarity. AnyW (DolanSingularType ground polarity) -> Maybe (AnyW SymbolType)
checkVar (MkAnyW (VarDolanSingularType var)) = Just (MkAnyW var)
checkVar _ = Nothing

appearanceVars :: forall (ground :: GroundTypeKind) polarity. Appearance ground polarity -> [AnyW SymbolType]
appearanceVars (MkAppearance appr) = mapMaybe checkVar appr

type GetVarUses :: GroundTypeKind -> (k -> Type) -> Constraint
class GetVarUses ground f | f -> ground where
    getVarAppearances :: forall t. f t -> ([Appearance ground 'Positive], [Appearance ground 'Negative])

{-
type VarReplacements :: GroundTypeKind -> Symbol -> Type
data VarReplacements ground name = MkVarReplacements
    {
        vrPositive :: [DolanShimWit ground 'Positive (UVarT name)],
        vrNegative :: [DolanShimWit ground 'Negative (UVarT name)]
    }

runSVR ::forall (ground :: GroundTypeKind) name a.
    ([DolanShimWit ground 'Positive (UVarT name)],[DolanShimWit ground 'Negative (UVarT name)]) -> State (VarReplacements ground name) a -> a
runSVR (vrPositive,vrNegative) svr = case runState svr MkVarReplacements{..} of
    (a,MkVarReplacements [] []) -> a
    _ -> error "simplifier:  leftover types"

svrDrawPositiveType :: forall (ground :: GroundTypeKind) name. State (VarReplacements ground name) (DolanShimWit ground 'Positive (UVarT name))
svrDrawPositiveType = do
    vr <- get
    case vrPositive vr of
        t : tt -> do
            put vr {vrPositive = tt}
            return t
        [] -> error "simplifier: missing positive type"

svrDrawNegativeType :: forall (ground :: GroundTypeKind) name. State (VarReplacements ground name) (DolanShimWit ground 'Negative (UVarT name))
svrDrawNegativeType = do
    vr <- get
    case vrNegative vr of
        t : tt -> do
            put vr {vrNegative = tt}
            return t
        [] -> error "simplifier: missing negative type"

type ReplaceVarUses :: GroundTypeKind -> (Polarity -> Type -> Type) -> Constraint
class ReplaceVarUses ground f | f -> ground where
    replaceVarByUses ::
        forall polarity name t. PolarityType polarity ->
            SymbolType name -> f polarity t -> State (VarReplacements ground name) (DolanShimWit ground polarity t)
-}
getCCRVarAppearances ::
       forall (ground :: GroundTypeKind) polarity sv t. (IsDolanGroundType ground, Is PolarityType polarity)
    => CCRPolarArgument (DolanType ground) polarity sv t
    -> ([Appearance ground 'Positive], [Appearance ground 'Negative])
getCCRVarAppearances (CoCCRPolarArgument t) = getVarAppearances t
getCCRVarAppearances (ContraCCRPolarArgument t) = invertPolarity @polarity $ getVarAppearances t
getCCRVarAppearances (RangeCCRPolarArgument tp tq) =
    invertPolarity @polarity $ getVarAppearances tp <> getVarAppearances tq

instance forall (ground :: GroundTypeKind) polarity cat wit. GetVarUses ground wit =>
             GetVarUses ground (PolarShimWit cat wit polarity) where
    getVarAppearances (MkShimWit w _) = getVarAppearances w

instance forall (ground :: GroundTypeKind) polarity dv gt. (IsDolanGroundType ground, Is PolarityType polarity) =>
             GetVarUses ground (DolanArguments dv (DolanType ground) gt polarity) where
    getVarAppearances NilCCRArguments = mempty
    getVarAppearances (ConsCCRArguments arg args) =
        getCCRVarAppearances @ground arg <> getVarAppearances @_ @ground args

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             GetVarUses ground (DolanSingularType ground polarity) where
    getVarAppearances (GroundedDolanSingularType _ args) = getVarAppearances args
    getVarAppearances (VarDolanSingularType _) = mempty
    getVarAppearances (RecursiveDolanSingularType vn st) = let
        (pvarss, nvarss) = getVarAppearances st
        in case polarityType @polarity of
               PositiveType -> (fmap (removeAppearanceVar $ MkAnyW vn) pvarss, nvarss)
               NegativeType -> (pvarss, fmap (removeAppearanceVar $ MkAnyW vn) nvarss)

{-
instance forall (ground :: GroundTypeKind). IsDolanGroundType ground =>
             ReplaceVarUses ground (DolanSingularType ground) where
    replaceVarByUses polw var (VarDolanSingularType v) | Just Refl <- testEquality var v = do
        case polw of
               PositiveType -> svrDrawPositiveType
               NegativeType -> svrDrawNegativeType
-}
getVarAppearances' ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> ([Appearance ground 'Positive], [Appearance ground 'Negative])
getVarAppearances' NilDolanType = mempty
getVarAppearances' (ConsDolanType t1 tr) = getVarAppearances t1 <> getVarAppearances' tr

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             GetVarUses ground (DolanType ground polarity) where
    getVarAppearances t = let
        appr = typeToAppearance t
        in (case polarityType @polarity of
                PositiveType -> ([appr], [])
                NegativeType -> ([], [appr])) <>
           getVarAppearances' @ground t

mappableGetAppearances ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => PShimWitMappable (DolanShim ground) (DolanType ground) a =>
               a -> ([Appearance ground 'Positive], [Appearance ground 'Negative])
mappableGetAppearances a =
    mconcat $
    fmap
        (\case
             Left (MkAnyW t) -> getVarAppearances t
             Right (MkAnyW t) -> getVarAppearances t) $
    mappableGetWitnesses @_ @(DolanShimWit ground 'Positive) @(DolanShimWit ground 'Negative) a

-- | (positive, negative)
-- to be used after merging duplicate ground types
-- used to find shared type vars
-- example: "a -> (b|c,d)" ==> ([[b,c],[d]],[[a]])
mappableGetVarUses ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => PShimWitMappable (DolanShim ground) (DolanType ground) a => a -> ([[AnyW SymbolType]], [[AnyW SymbolType]])
mappableGetVarUses a = let
    (posapprs, negapprs) = mappableGetAppearances @ground a
    in (fmap appearanceVars posapprs, fmap appearanceVars negapprs)

class GetExpressionVars f where
    -- | (positive, negative)
    getExpressionVars :: forall t. f t -> ([AnyW SymbolType], [AnyW SymbolType])

instance GetExpressionVars wit => GetExpressionVars (PolarShimWit cat wit polarity) where
    getExpressionVars (MkShimWit w _) = getExpressionVars w

getArgExpressionVars ::
       forall (ground :: GroundTypeKind) polarity sv a. (IsDolanGroundType ground, Is PolarityType polarity)
    => CCRPolarArgument (DolanType ground) polarity sv a
    -> ([AnyW SymbolType], [AnyW SymbolType])
getArgExpressionVars (CoCCRPolarArgument t) = getExpressionVars t
getArgExpressionVars (ContraCCRPolarArgument t) = invertPolarity @polarity $ getExpressionVars t
getArgExpressionVars (RangeCCRPolarArgument tp tq) =
    invertPolarity @polarity $ getExpressionVars tp <> getExpressionVars tq

getArgsExpressionVars ::
       forall (ground :: GroundTypeKind) polarity dv gt t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanArguments dv (DolanType ground) gt polarity t
    -> ([AnyW SymbolType], [AnyW SymbolType])
getArgsExpressionVars NilCCRArguments = mempty
getArgsExpressionVars (ConsCCRArguments arg args) =
    getArgExpressionVars @ground @polarity arg <> getArgsExpressionVars args

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             GetExpressionVars (DolanSingularType ground polarity) where
    getExpressionVars (GroundedDolanSingularType _ args) = getArgsExpressionVars args
    getExpressionVars (VarDolanSingularType vn) =
        case polarityType @polarity of
            PositiveType -> ([MkAnyW vn], [])
            NegativeType -> ([], [MkAnyW vn])
    getExpressionVars (RecursiveDolanSingularType vn st) = let
        (pvars, nvars) = getExpressionVars st
        removeVar :: [AnyW SymbolType] -> [AnyW SymbolType]
        removeVar = filter $ (/=) $ MkAnyW vn
        in case polarityType @polarity of
               PositiveType -> (removeVar pvars, nvars)
               NegativeType -> (pvars, removeVar nvars)

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             GetExpressionVars ((DolanType ground) polarity) where
    getExpressionVars NilDolanType = mempty
    getExpressionVars (ConsDolanType t1 tr) = getExpressionVars t1 <> getExpressionVars tr

-- | (positive, negative)
-- to be used after merging duplicate ground types
-- used to find one-sided type variables
-- example: "a -> (b|c,d)" ==> ([b,c,d]],[a])
mappableGetVars ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => PShimWitMappable (DolanShim ground) (DolanType ground) a => a -> ([AnyW SymbolType], [AnyW SymbolType])
mappableGetVars a =
    mconcat $
    fmap
        (\case
             Left (MkAnyW t) -> getExpressionVars t
             Right (MkAnyW t) -> getExpressionVars t) $
    mappableGetWitnesses @_ @(DolanShimWit ground 'Positive) @(DolanShimWit ground 'Negative) a
