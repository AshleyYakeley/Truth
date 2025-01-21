{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.TypeSystem.Bindings
    ( TSBindingData
    , TSBinding
    , singleBinding
    , bindingsNames
    , bindingSequentialLetSealedExpression
    , bindingsRecursiveLetSealedExpression
    )
where

import Shapes

import Language.Expression.Common
import Language.Expression.TypeSystem.Abstract
import Language.Expression.TypeSystem.Rename
import Language.Expression.TypeSystem.SolverExpression
import Language.Expression.TypeSystem.Subsume
import Language.Expression.TypeSystem.TypeSystem
import Language.Expression.TypeSystem.Unify

type TSBindingData :: Type -> Type
type family TSBindingData ts

data TSBinding (ts :: Type) where
    MkTSBinding ::
        TSVarID ts ->
        (TSSealedExpression ts -> TSBindingData ts) ->
        Bool ->
        TSOuter ts (SealedSubsumerExpression ts) ->
        TSBinding ts

singleBinding ::
    TSVarID ts ->
    (TSSealedExpression ts -> TSBindingData ts) ->
    Bool ->
    TSOuter ts (SealedSubsumerExpression ts) ->
    TSBinding ts
singleBinding = MkTSBinding

type BindMap ts = Map (TSVarID ts) (TSBindingData ts, TSSealedExpression ts)

data Bound ts
    = forall tdecl. MkBound
        (forall a. TSOpenExpression ts a -> TSOuter ts (UnifierExpression ts (tdecl -> a)))
        (OpenSubsumerExpression ts tdecl)
        (UnifierSubstitutions ts -> SubsumerSubstitutions ts -> TSOpenExpression ts tdecl -> TSOuter ts (BindMap ts))

instance (AbstractTypeSystem ts, SubsumeTypeSystem ts) => Semigroup (Bound ts) where
    MkBound abstractNamesA (exprsA :: _ adecl) getbindsA <> MkBound abstractNamesB (exprsB :: _ bdecl) getbindsB = let
        abstractNamesAB :: forall a. TSOpenExpression ts a -> TSOuter ts (UnifierExpression ts ((adecl, bdecl) -> a))
        abstractNamesAB expr = do
            MkSolverExpression uconvA exprA <- abstractNamesA expr
            MkSolverExpression uconvB exprAB <- abstractNamesB exprA
            let uconvAB = liftA2 (,) uconvA uconvB
            return $ MkSolverExpression uconvAB $ fmap (\ff (ta, tb) ~(va, vb) -> ff tb vb ta va) exprAB
        exprsAB :: OpenSubsumerExpression ts (adecl, bdecl)
        exprsAB = exprsA <***> exprsB
        getbindsAB ::
            UnifierSubstitutions ts ->
            SubsumerSubstitutions ts ->
            TSOpenExpression ts (adecl, bdecl) ->
            TSOuter ts (BindMap ts)
        getbindsAB usubs ssubs fexprAB = do
            bindsA <- getbindsA usubs ssubs $ fmap fst fexprAB
            bindsB <- getbindsB usubs ssubs $ fmap snd fexprAB
            return $ bindsA <> bindsB
        in MkBound abstractNamesAB exprsAB getbindsAB

instance (AbstractTypeSystem ts, SubsumeTypeSystem ts) => Monoid (Bound ts) where
    mempty = let
        abstractNames :: forall a. TSOpenExpression ts a -> TSOuter ts (UnifierExpression ts (() -> a))
        abstractNames expr = return $ MkSolverExpression (pure ()) $ fmap (\a () ~() -> a) expr
        exprs :: OpenSubsumerExpression ts ()
        exprs = rUnit
        getbinds ::
            UnifierSubstitutions ts -> SubsumerSubstitutions ts -> TSOpenExpression ts () -> TSOuter ts (BindMap ts)
        getbinds _ _ _ = return mempty
        in MkBound abstractNames exprs getbinds

singleBound ::
    forall ts.
    (AbstractTypeSystem ts, SubsumeTypeSystem ts) =>
    TSBinding ts ->
    TSOuter ts (Bound ts)
singleBound (MkTSBinding name bdf hasTypeSig mexpr) = do
    MkSealedSubsumerExpression (decltype :: _ tdecl) subsexpr <- mexpr
    decltype' <-
        if hasTypeSig
            then renameMappable @ts [] FreeName decltype
            else return decltype
    let
        abstractNames :: forall a. TSOpenExpression ts a -> TSOuter ts (UnifierExpression ts (tdecl -> a))
        abstractNames = abstractOpenExpression @ts name decltype'
        getbinds ::
            UnifierSubstitutions ts ->
            SubsumerSubstitutions ts ->
            TSOpenExpression ts tdecl ->
            TSOuter ts (BindMap ts)
        getbinds usubs ssubs fexpr = do
            fexpr' <- unEndoM (subsumerSubstitute @ts ssubs) fexpr
            expr <- unEndoM (unifierSubstituteSimplifyFinalRename @ts usubs) $ MkSealedExpression decltype fexpr'
            return $ singletonMap name (bdf expr, expr)
    return $ MkBound abstractNames subsexpr getbinds

boundToMapRecursive ::
    forall ts.
    SubsumeTypeSystem ts =>
    Bound ts ->
    TSOuter ts (BindMap ts)
boundToMapRecursive (MkBound abstractNames (MkSolverExpression subsumer exprs) getbinds) = do
    MkSolverExpression uexprtt uexprvv <- abstractNames exprs -- abstract
    (fexpr, usubs) <- solveUnifier @ts uexprtt -- unify
    (ssexpr, ssubs) <- usubSolveSubsumer @ts usubs subsumer
    getbinds usubs ssubs $ (\t1 t2 tdi -> fix $ \tdecl -> tdi t2 tdecl t1) <$> ssexpr <*> fexpr <*> uexprvv

-- for a recursive component
bindingsRecursiveLetSealedExpression ::
    forall ts.
    (AbstractTypeSystem ts, SubsumeTypeSystem ts) =>
    [TSBinding ts] ->
    TSInner ts (Map (TSVarID ts) (TSBindingData ts, TSSealedExpression ts))
bindingsRecursiveLetSealedExpression bindings =
    runRenamer @ts [] [] $ do
        bounds <- for bindings singleBound
        boundToMapRecursive $ mconcat bounds

bindingSequentialLetSealedExpression ::
    forall ts.
    (AbstractTypeSystem ts, SubsumeTypeSystem ts) =>
    TSBinding ts ->
    TSInner ts (Map (TSVarID ts) (TSBindingData ts, TSSealedExpression ts))
bindingSequentialLetSealedExpression (MkTSBinding name bdf _ mexpr) =
    runRenamer @ts [] [] $ do
        MkSealedSubsumerExpression tdecl sexpr <- mexpr
        (oexpr, ssubs) <- solveSubsumerExpression @ts sexpr
        oexpr' <- unEndoM (subsumerSubstitute @ts ssubs) oexpr
        expr <- unEndoM (simplifyFinalRename @ts) $ MkSealedExpression tdecl oexpr'
        return $ singletonMap name (bdf expr, expr)

bindingsNames :: [TSBinding ts] -> [TSVarID ts]
bindingsNames bindings = fmap (\(MkTSBinding name _ _ _) -> name) bindings
