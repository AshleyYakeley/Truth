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
import Language.Expression.Common.Simplifier
import Language.Expression.Common.Subsumer
import Language.Expression.Common.TypeSystem
import Language.Expression.Common.Unifier
import Shapes

data Binding (ts :: Type) where
    MkBinding :: TSName ts -> Maybe (AnyW (TSPosWitness ts)) -> TSSealedExpression ts -> Binding ts

newtype Bindings ts =
    MkBindings [Binding ts]
    deriving (Semigroup, Monoid)

singleBinding :: TSName ts -> Maybe (AnyW (TSPosWitness ts)) -> TSSealedExpression ts -> Bindings ts
singleBinding name msig expr = MkBindings $ pure $ MkBinding name msig expr

bindingsMap :: Ord (TSName ts) => Bindings ts -> Map (TSName ts) (TSSealedExpression ts)
bindingsMap (MkBindings bb) = mapFromList $ fmap (\(MkBinding n _ e) -> (n, e)) bb

data UnifyExpression ts a =
    forall t. MkUnifyExpression (Unifier ts t)
                                (TSOpenExpression ts (t -> a))

unifierExpression :: Functor (Unifier ts) => UnifyExpression ts a -> Unifier ts (TSOpenExpression ts a)
unifierExpression (MkUnifyExpression uconv expr) = fmap (\conv -> fmap (\conva -> conva conv) expr) uconv

data Bound ts =
    forall tdecl tinf. MkBound (forall a. TSOpenExpression ts a -> TSOuter ts (UnifyExpression ts (tdecl -> a)))
                               (TSOpenExpression ts tinf)
                               (Subsumer ts (tinf -> tdecl))
                               (UnifierSubstitutions ts -> SubsumerSubstitutions ts -> TSOpenExpression ts tdecl -> TSOuter ts (Bindings ts))

instance (AbstractTypeSystem ts, SubsumeTypeSystem ts) => Semigroup (Bound ts) where
    MkBound abstractNamesA (exprsA :: _ ainf) subsumerA (getbindsA :: _ -> _ -> _ adecl -> _) <> MkBound abstractNamesB (exprsB :: _ binf) subsumerB (getbindsB :: _ -> _ -> _ bdecl -> _) = let
        abstractNamesAB :: forall a. TSOpenExpression ts a -> TSOuter ts (UnifyExpression ts ((adecl, bdecl) -> a))
        abstractNamesAB expr = do
            MkUnifyExpression uconvA exprA <- abstractNamesA expr
            MkUnifyExpression uconvB exprAB <- abstractNamesB exprA
            let uconvAB = liftA2 (,) uconvA uconvB
            return $ MkUnifyExpression uconvAB $ fmap (\ff (ta, tb) ~(va, vb) -> ff tb vb ta va) exprAB
        exprsAB :: TSOpenExpression ts (ainf, binf)
        exprsAB = liftA2 (,) exprsA exprsB
        subsumerAB :: Subsumer ts ((ainf, binf) -> (adecl, bdecl))
        subsumerAB = liftA2 (\fa fb (a, b) -> (fa a, fb b)) subsumerA subsumerB
        getbindsAB ::
               UnifierSubstitutions ts
            -> SubsumerSubstitutions ts
            -> TSOpenExpression ts (adecl, bdecl)
            -> TSOuter ts (Bindings ts)
        getbindsAB usubs ssubs fexprAB = do
            bindsA <- getbindsA usubs ssubs $ fmap fst fexprAB
            bindsB <- getbindsB usubs ssubs $ fmap snd fexprAB
            return $ bindsA <> bindsB
        in MkBound abstractNamesAB exprsAB subsumerAB getbindsAB

instance (AbstractTypeSystem ts, SubsumeTypeSystem ts) => Monoid (Bound ts) where
    mempty = let
        abstractNames :: forall a. TSOpenExpression ts a -> TSOuter ts (UnifyExpression ts (() -> a))
        abstractNames expr = return $ MkUnifyExpression (pure ()) $ fmap (\a _ _ -> a) expr
        exprs :: TSOpenExpression ts ()
        exprs = pure ()
        subsumer :: Subsumer ts (() -> ())
        subsumer = pure id
        getbinds ::
               UnifierSubstitutions ts -> SubsumerSubstitutions ts -> TSOpenExpression ts () -> TSOuter ts (Bindings ts)
        getbinds _ _ _ = return mempty
        in MkBound abstractNames exprs subsumer getbinds

data SubsumerWit ts tinf =
    forall tdecl. MkSubsumerWit (TSPosShimWit ts tdecl)
                                (Subsumer ts (TSShim ts tinf tdecl))

singleBound ::
       forall ts. (AbstractTypeSystem ts, SubsumeTypeSystem ts)
    => Binding ts
    -> TSOuter ts (Bound ts)
singleBound (MkBinding name mtdecl sexpr) =
    withTransConstraintTM @Monad $ do
        MkSealedExpression (twtinf :: _ tinf) bexpr <- rename @ts sexpr
        subswit :: SubsumerWit ts tinf <-
            case mtdecl of
                Nothing -> return $ MkSubsumerWit twtinf $ pure id
                Just (MkAnyW rawdecltype) -> do
                    MkShimWit decltype _ <- simplify @ts $ mkShimWit @Type @(TSShim ts) @_ @'Positive rawdecltype
                    twtdecl' <- namespace @ts $ renamePosWitness @ts decltype
                    subsumer <- subsumePosShimWit @ts twtinf twtdecl'
                    return $ MkSubsumerWit (mkShimWit twtdecl') subsumer
        return $
            case subswit of
                MkSubsumerWit (wdecl :: _ tdecl) subsumer -> let
                    abstractNames :: forall a. TSOpenExpression ts a -> TSOuter ts (UnifyExpression ts (tdecl -> a))
                    abstractNames e = do
                        MkAbstractResult vwt e' <- abstractNamedExpression @ts name e
                        uconv <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit wdecl) vwt
                        return $ MkUnifyExpression (uuGetShim uconv) $ fmap (\ta conv -> ta . shimToFunction conv) e'
                    getbinds ::
                           UnifierSubstitutions ts
                        -> SubsumerSubstitutions ts
                        -> TSOpenExpression ts tdecl
                        -> TSOuter ts (Bindings ts)
                    getbinds usubs ssubs fexpr = do
                        fexpr' <- subsumerExpressionSubstitute @ts ssubs fexpr
                        expr <- unifierSubstituteAndSimplify @ts usubs $ MkSealedExpression wdecl fexpr'
                        return $ singleBinding name Nothing expr
                    in MkBound abstractNames bexpr (fmap shimToFunction subsumer) getbinds

boundToBindings ::
       forall ts. (UnifyTypeSystem ts, SubsumeTypeSystem ts)
    => Bound ts
    -> TSOuter ts (Bindings ts)
boundToBindings (MkBound abstractNames exprs subsumer getbinds) = do
    uexprvv <- abstractNames exprs -- abstract
    (fexpr, usubs) <- solveUnifier @ts $ unifierExpression uexprvv -- unify
    (subconv, ssubs) <- solveSubsumer @ts subsumer
    getbinds usubs ssubs $ fmap (\tdi -> fix $ subconv . tdi) fexpr

-- for a recursive component
bindingsComponentLetSealedExpression ::
       forall ts. (Ord (TSName ts), AbstractTypeSystem ts, SubsumeTypeSystem ts)
    => Bindings ts
    -> TSInner ts (Map (TSName ts) (TSSealedExpression ts))
bindingsComponentLetSealedExpression (MkBindings bindings) =
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        bounds <- for bindings singleBound
        bindings' <- boundToBindings $ mconcat bounds
        return $ bindingsMap bindings'

bindingsNames :: Bindings ts -> [TSName ts]
bindingsNames (MkBindings bb) = fmap (\(MkBinding name _ _) -> name) bb
