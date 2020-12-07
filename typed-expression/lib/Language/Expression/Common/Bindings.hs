{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Bindings
    ( Bindings
    , singleBinding
    , bindingsNames
    , bindingsComponentLetSealedExpression
    ) where

import Data.Shim
import Language.Expression.Common.Abstract
import Language.Expression.Common.Rename.RenameTypeSystem
import Language.Expression.Common.Sealed
import Language.Expression.Common.Subsumer
import Language.Expression.Common.TypeSystem
import Language.Expression.Common.Unifier
import Shapes

data Binding (ts :: Type) where
    MkBinding :: TSName ts -> TSOuter ts (SubsumerExpression ts) -> Binding ts

newtype Bindings ts =
    MkBindings [Binding ts]
    deriving (Semigroup, Monoid)

singleBinding :: TSName ts -> TSOuter ts (SubsumerExpression ts) -> Bindings ts
singleBinding name expr = MkBindings $ pure $ MkBinding name expr

data UnifyExpression ts a =
    forall t. MkUnifyExpression (Unifier ts t)
                                (TSOpenExpression ts (t -> a))

unifierExpression :: Functor (Unifier ts) => UnifyExpression ts a -> Unifier ts (TSOpenExpression ts a)
unifierExpression (MkUnifyExpression uconv expr) = fmap (\conv -> fmap (\conva -> conva conv) expr) uconv

type BindMap ts = Map (TSName ts) (TSSealedExpression ts)

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
        abstractNames expr = return $ MkUnifyExpression (pure ()) $ fmap (\a _ _ -> a) expr
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
singleBound (MkBinding name mexpr) = do
    MkSubsumerExpression (decltype :: _ tdecl) subsexpr <- mexpr
    let
        abstractNames :: forall a. TSOpenExpression ts a -> TSOuter ts (UnifyExpression ts (tdecl -> a))
        abstractNames e = do
            MkAbstractResult vwt e' <- abstractNamedExpression @ts name e
            uconv <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit decltype) vwt
            return $ MkUnifyExpression (uuGetShim uconv) $ fmap (\ta conv -> ta . shimToFunction conv) e'
        getbinds ::
               UnifierSubstitutions ts
            -> SubsumerSubstitutions ts
            -> TSOpenExpression ts tdecl
            -> TSOuter ts (BindMap ts)
        getbinds usubs ssubs fexpr = do
            fexpr' <- subsumerExpressionSubstitute @ts ssubs fexpr
            expr <- unifierSubstituteAndSimplify @ts usubs $ MkSealedExpression decltype fexpr'
            return $ singletonMap name expr
    return $ MkBound abstractNames subsexpr getbinds

boundToMap ::
       forall ts. (UnifyTypeSystem ts, SubsumeTypeSystem ts)
    => Bound ts
    -> TSOuter ts (BindMap ts)
boundToMap (MkBound abstractNames (MkSubsumerOpenExpression subsumer exprs) getbinds) = do
    uexprvv <- abstractNames exprs -- abstract
    (fexpr, usubs) <- solveUnifier @ts $ unifierExpression uexprvv -- unify
    (subconv, ssubs) <- solveSubsumer @ts subsumer
    getbinds usubs ssubs $ fmap (\tdi -> fix $ subconv . tdi) fexpr

-- for a recursive component
bindingsComponentLetSealedExpression ::
       forall ts. (AbstractTypeSystem ts, SubsumeTypeSystem ts)
    => Bindings ts
    -> TSInner ts (Map (TSName ts) (TSSealedExpression ts))
bindingsComponentLetSealedExpression (MkBindings bindings) =
    runRenamer @ts $ do
        bounds <- for bindings singleBound
        boundToMap $ mconcat bounds

bindingsNames :: Bindings ts -> [TSName ts]
bindingsNames (MkBindings bb) = fmap (\(MkBinding name _) -> name) bb
