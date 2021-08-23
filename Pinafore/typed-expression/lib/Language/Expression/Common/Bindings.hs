{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Bindings
    ( TSBindingData
    , Binding
    , singleBinding
    , bindingsNames
    , bindingSequentialLetSealedExpression
    , bindingsRecursiveLetSealedExpression
    ) where

import Data.Shim
import Language.Expression.Common.Abstract
import Language.Expression.Common.Rename
import Language.Expression.Common.Sealed
import Language.Expression.Common.Subsumer
import Language.Expression.Common.TypeSystem
import Language.Expression.Common.Unifier
import Shapes

type TSBindingData :: Type -> Type
type family TSBindingData ts

data Binding (ts :: Type) where
    MkBinding :: TSName ts -> TSBindingData ts -> TSOuter ts (SubsumerExpression ts) -> Binding ts

singleBinding :: TSName ts -> TSBindingData ts -> TSOuter ts (SubsumerExpression ts) -> Binding ts
singleBinding name bd expr = MkBinding name bd expr

data UnifyExpression ts a =
    forall t. MkUnifyExpression (Unifier ts t)
                                (TSOpenExpression ts (t -> a))

unifierExpression :: Functor (Unifier ts) => UnifyExpression ts a -> Unifier ts (TSOpenExpression ts a)
unifierExpression (MkUnifyExpression uconv expr) = fmap (\conv -> fmap (\conva -> conva conv) expr) uconv

type BindMap ts = Map (TSName ts) (TSBindingData ts, TSSealedExpression ts)

data Bound ts =
    forall tdecl. MkBound (forall a. TSOpenExpression ts a -> TSOuter ts (UnifyExpression ts (tdecl -> a)))
                          (SubsumerOpenExpression ts tdecl)
                          (UnifierSubstitutions ts -> SubsumerSubstitutions ts -> TSOpenExpression ts tdecl -> TSOuter ts (BindMap ts))

instance (AbstractTypeSystem ts, SubsumeTypeSystem ts) => Semigroup (Bound ts) where
    MkBound abstractNamesA (exprsA :: _ adecl) getbindsA <> MkBound abstractNamesB (exprsB :: _ bdecl) getbindsB = let
        abstractNamesAB :: forall a. TSOpenExpression ts a -> TSOuter ts (UnifyExpression ts ((adecl, bdecl) -> a))
        abstractNamesAB expr = do
            MkUnifyExpression uconvA exprA <- abstractNamesA expr
            MkUnifyExpression uconvB exprAB <- abstractNamesB exprA
            let uconvAB = liftA2 (,) uconvA uconvB
            return $ MkUnifyExpression uconvAB $ fmap (\ff (ta, tb) ~(va, vb) -> ff tb vb ta va) exprAB
        exprsAB :: SubsumerOpenExpression ts (adecl, bdecl)
        exprsAB = exprsA <***> exprsB
        getbindsAB ::
               UnifierSubstitutions ts
            -> SubsumerSubstitutions ts
            -> TSOpenExpression ts (adecl, bdecl)
            -> TSOuter ts (BindMap ts)
        getbindsAB usubs ssubs fexprAB = do
            bindsA <- getbindsA usubs ssubs $ fmap fst fexprAB
            bindsB <- getbindsB usubs ssubs $ fmap snd fexprAB
            return $ bindsA <> bindsB
        in MkBound abstractNamesAB exprsAB getbindsAB

instance (AbstractTypeSystem ts, SubsumeTypeSystem ts) => Monoid (Bound ts) where
    mempty = let
        abstractNames :: forall a. TSOpenExpression ts a -> TSOuter ts (UnifyExpression ts (() -> a))
        abstractNames expr = return $ MkUnifyExpression (pure ()) $ fmap (\a () ~() -> a) expr
        exprs :: SubsumerOpenExpression ts ()
        exprs = pUnit
        getbinds ::
               UnifierSubstitutions ts -> SubsumerSubstitutions ts -> TSOpenExpression ts () -> TSOuter ts (BindMap ts)
        getbinds _ _ _ = return mempty
        in MkBound abstractNames exprs getbinds

singleBound ::
       forall ts. (AbstractTypeSystem ts, SubsumeTypeSystem ts)
    => Binding ts
    -> TSOuter ts (Bound ts)
singleBound (MkBinding name bd mexpr) = do
    MkSubsumerExpression (decltype :: _ tdecl) subsexpr <- mexpr
    let
        abstractNames :: forall a. TSOpenExpression ts a -> TSOuter ts (UnifyExpression ts (tdecl -> a))
        abstractNames expr = do
            MkAbstractResult vwt expr' <- abstractNamedExpression @ts name expr
            uconv <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit decltype) vwt
            return $ MkUnifyExpression (uuGetShim uconv) $ fmap (\ta conv -> ta . shimToFunction conv) expr'
        getbinds ::
               UnifierSubstitutions ts
            -> SubsumerSubstitutions ts
            -> TSOpenExpression ts tdecl
            -> TSOuter ts (BindMap ts)
        getbinds usubs ssubs fexpr = do
            fexpr' <- subsumerExpressionSubstitute @ts ssubs fexpr
            expr <- unifierSubstituteAndSimplify @ts usubs $ MkSealedExpression decltype fexpr'
            return $ singletonMap name (bd, expr)
    return $ MkBound abstractNames subsexpr getbinds

boundToMapRecursive ::
       forall ts. SubsumeTypeSystem ts
    => Bound ts
    -> TSOuter ts (BindMap ts)
boundToMapRecursive (MkBound abstractNames (MkSubsumerOpenExpression (subsumer :: _ (tinf -> tdecl)) exprs) getbinds) = do
    uexprvv <- abstractNames exprs -- abstract
    (fexpr, usubs) <- solveUnifier @ts $ unifierExpression uexprvv -- unify
    (subconv, ssubs) <- usubSolveSubsumer @ts usubs subsumer
    getbinds usubs ssubs $ fmap (\tdi -> fix $ subconv . tdi) fexpr

-- for a recursive component
bindingsRecursiveLetSealedExpression ::
       forall ts. (AbstractTypeSystem ts, SubsumeTypeSystem ts)
    => [Binding ts]
    -> TSInner ts (Map (TSName ts) (TSBindingData ts, TSSealedExpression ts))
bindingsRecursiveLetSealedExpression bindings =
    runRenamer @ts $ do
        bounds <- for bindings singleBound
        boundToMapRecursive $ mconcat bounds

bindingSequentialLetSealedExpression ::
       forall ts. (AbstractTypeSystem ts, SubsumeTypeSystem ts)
    => Binding ts
    -> TSInner ts (Map (TSName ts) (TSBindingData ts, TSSealedExpression ts))
bindingSequentialLetSealedExpression (MkBinding name bd mexpr) =
    runRenamer @ts $ do
        MkSubsumerExpression tdecl (MkSubsumerOpenExpression subsumer expr) <- mexpr
        (subconv, ssubs) <- solveSubsumer @ts subsumer
        expr' <- subsumerExpressionSubstitute @ts ssubs expr
        return $ singletonMap name (bd, MkSealedExpression tdecl $ fmap subconv expr')

bindingsNames :: [Binding ts] -> [TSName ts]
bindingsNames bindings = fmap (\(MkBinding name _ _) -> name) bindings
