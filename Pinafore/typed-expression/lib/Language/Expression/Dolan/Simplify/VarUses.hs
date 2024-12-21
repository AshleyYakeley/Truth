module Language.Expression.Dolan.Simplify.VarUses
    ( mappableGetVarUses
    , mappableGetVars
    , Appearance
    , mappableGetAppearances
    , appearanceMatchingTypes
    ) where

import Data.Shim
import Language.Expression.Dolan.FreeVars
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem
import Shapes

type Appearance :: GroundTypeKind -> Polarity -> Type
newtype Appearance ground polarity =
    MkAppearance [Some (DolanSingularType ground polarity)]

appearanceMatchingType ::
       forall (ground :: GroundTypeKind) polarity.
       SomeTypeVarT
    -> Appearance ground polarity
    -> Maybe (Some (DolanType ground polarity))
appearanceMatchingType (MkSomeTypeVarT var) (MkAppearance appr) = let
    matchRemove :: [Some (DolanSingularType ground polarity)] -> (Bool, [Some (DolanSingularType ground polarity)])
    matchRemove [] = (False, [])
    matchRemove (MkSome (VarDolanSingularType var'):rr)
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
       SomeTypeVarT
    -> [Appearance ground polarity]
    -> [Some (DolanType ground polarity)]
appearanceMatchingTypes var = mapMaybe (appearanceMatchingType var)

removeAppearanceVar ::
       forall (ground :: GroundTypeKind) polarity.
       SomeTypeVarT
    -> Appearance ground polarity
    -> Appearance ground polarity
removeAppearanceVar (MkSomeTypeVarT var) (MkAppearance appr) = let
    notVar :: Some (DolanSingularType ground polarity) -> Bool
    notVar (MkSome t) = not $ variableOccursIn var t
    in MkAppearance $ filter notVar appr

typeToAppearance ::
       forall (ground :: GroundTypeKind) polarity t. DolanType ground polarity t -> Appearance ground polarity
typeToAppearance t = MkAppearance $ typeToAnySingulars t

checkVar :: forall (ground :: GroundTypeKind) polarity. Some (DolanSingularType ground polarity) -> Maybe (SomeTypeVarT)
checkVar (MkSome (VarDolanSingularType var)) = Just (MkSomeTypeVarT var)
checkVar _ = Nothing

appearanceVars :: forall (ground :: GroundTypeKind) polarity. Appearance ground polarity -> [SomeTypeVarT]
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
            TypeVarT name -> f polarity t -> State (VarReplacements ground name) (DolanShimWit ground polarity t)
-}
getCCRVarAppearances ::
       forall (ground :: GroundTypeKind) polarity sv t. (IsDolanGroundType ground, Is PolarityType polarity)
    => CCRPolarArgument (DolanType ground) polarity sv t
    -> ([Appearance ground 'Positive], [Appearance ground 'Negative])
getCCRVarAppearances (CoCCRPolarArgument t) = getVarAppearances t
getCCRVarAppearances (ContraCCRPolarArgument t) = withInvertPolarity @polarity $ getVarAppearances t
getCCRVarAppearances (RangeCCRPolarArgument tp tq) =
    withInvertPolarity @polarity $ getVarAppearances tp <> getVarAppearances tq

instance forall (ground :: GroundTypeKind) polarity cat wit. GetVarUses ground wit =>
             GetVarUses ground (PolarShimWit cat wit polarity) where
    getVarAppearances (MkShimWit w _) = getVarAppearances w

instance forall (ground :: GroundTypeKind) polarity dv gt. (IsDolanGroundType ground, Is PolarityType polarity) =>
             GetVarUses ground (CCRPolarArguments dv (DolanType ground) gt polarity) where
    getVarAppearances NilCCRArguments = mempty
    getVarAppearances (ConsCCRArguments arg args) =
        getCCRVarAppearances @ground arg <> getVarAppearances @_ @ground args

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             GetVarUses ground (DolanGroundedType ground polarity) where
    getVarAppearances (MkDolanGroundedType _ args) = getVarAppearances args

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             GetVarUses ground (DolanSingularType ground polarity) where
    getVarAppearances (GroundedDolanSingularType t) = getVarAppearances t
    getVarAppearances (VarDolanSingularType _) = mempty
    getVarAppearances (RecursiveDolanSingularType vn st) = let
        (pvarss, nvarss) = getVarAppearances st
        in case polarityType @polarity of
               PositiveType -> (fmap (removeAppearanceVar $ MkSomeTypeVarT vn) pvarss, nvarss)
               NegativeType -> (pvarss, fmap (removeAppearanceVar $ MkSomeTypeVarT vn) nvarss)

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
             Left (MkSome t) -> getVarAppearances t
             Right (MkSome t) -> getVarAppearances t) $
    mappableGetWitnesses @_ @(DolanShimWit ground 'Positive) @(DolanShimWit ground 'Negative) a

-- | (positive, negative)
-- to be used after merging duplicate ground types
-- used to find shared type vars
-- example: "a -> (b|c,d)" ==> ([[b,c],[d]],[[a]])
mappableGetVarUses ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => PShimWitMappable (DolanShim ground) (DolanType ground) a => a -> ([[SomeTypeVarT]], [[SomeTypeVarT]])
mappableGetVarUses a = let
    (posapprs, negapprs) = mappableGetAppearances @ground a
    in (fmap appearanceVars posapprs, fmap appearanceVars negapprs)

class GetExpressionVars (f :: Type -> Type) where
    -- | (positive, negative)
    getExpressionVars :: forall (t :: Type). f t -> ([SomeTypeVarT], [SomeTypeVarT])

instance GetExpressionVars wit => GetExpressionVars (PolarShimWit cat wit polarity) where
    getExpressionVars (MkShimWit w _) = getExpressionVars w

getArgExpressionVars ::
       forall (ground :: GroundTypeKind) polarity sv a. (IsDolanGroundType ground, Is PolarityType polarity)
    => CCRPolarArgument (DolanType ground) polarity sv a
    -> ([SomeTypeVarT], [SomeTypeVarT])
getArgExpressionVars (CoCCRPolarArgument t) = getExpressionVars t
getArgExpressionVars (ContraCCRPolarArgument t) = withInvertPolarity @polarity $ getExpressionVars t
getArgExpressionVars (RangeCCRPolarArgument tp tq) =
    withInvertPolarity @polarity $ getExpressionVars tp <> getExpressionVars tq

getArgsExpressionVars ::
       forall (ground :: GroundTypeKind) polarity dv gt t. (IsDolanGroundType ground, Is PolarityType polarity)
    => CCRPolarArguments dv (DolanType ground) gt polarity t
    -> ([SomeTypeVarT], [SomeTypeVarT])
getArgsExpressionVars NilCCRArguments = mempty
getArgsExpressionVars (ConsCCRArguments arg args) =
    getArgExpressionVars @ground @polarity arg <> getArgsExpressionVars args

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             GetExpressionVars (DolanGroundedType ground polarity) where
    getExpressionVars (MkDolanGroundedType _ args) = getArgsExpressionVars args

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             GetExpressionVars (DolanSingularType ground polarity) where
    getExpressionVars (GroundedDolanSingularType t) = getExpressionVars t
    getExpressionVars (VarDolanSingularType vn) =
        case polarityType @polarity of
            PositiveType -> ([MkSomeTypeVarT vn], [])
            NegativeType -> ([], [MkSomeTypeVarT vn])
    getExpressionVars (RecursiveDolanSingularType vn st) = let
        (pvars, nvars) = getExpressionVars st
        removeVar :: [SomeTypeVarT] -> [SomeTypeVarT]
        removeVar = filter $ (/=) $ MkSomeTypeVarT vn
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
    => PShimWitMappable (DolanShim ground) (DolanType ground) a => a -> ([SomeTypeVarT], [SomeTypeVarT])
mappableGetVars a =
    mconcat $
    fmap
        (\case
             Left (MkSome t) -> getExpressionVars t
             Right (MkSome t) -> getExpressionVars t) $
    mappableGetWitnesses @_ @(DolanShimWit ground 'Positive) @(DolanShimWit ground 'Negative) a
