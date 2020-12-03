{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Bindings
    ( Bindings
    , singleBinding
    , bindingsNames
    , bindingsComponentLetSealedExpression
    ) where

import Data.Shim.JoinMeet
import Language.Expression.Common.Abstract
import Language.Expression.Common.Rename.RenameTypeSystem
import Language.Expression.Common.Sealed
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
    forall vals. MkBound (forall a. TSOpenExpression ts a -> TSOuter ts (UnifyExpression ts (vals -> a)))
                         (TSOpenExpression ts vals)
                         (UnifierSubstitutions ts -> TSOpenExpression ts vals -> TSOuter ts (Bindings ts))

instance AbstractTypeSystem ts => Semigroup (Bound ts) where
    MkBound abstractNamesA (exprsA :: _ valsA) getbindsA <> MkBound abstractNamesB (exprsB :: _ valsB) getbindsB = let
        abstractNamesAB :: forall a. TSOpenExpression ts a -> TSOuter ts (UnifyExpression ts ((valsA, valsB) -> a))
        abstractNamesAB expr = do
            MkUnifyExpression uconvA exprA <- abstractNamesA expr
            MkUnifyExpression uconvB exprAB <- abstractNamesB exprA
            let uconvAB = (,) <$> uconvA <*> uconvB
            return $ MkUnifyExpression uconvAB $ fmap (\ff (ta, tb) ~(va, vb) -> ff tb vb ta va) exprAB
        exprsAB :: TSOpenExpression ts (valsA, valsB)
        exprsAB = (,) <$> exprsA <*> exprsB
        getbindsAB :: UnifierSubstitutions ts -> TSOpenExpression ts (valsA, valsB) -> TSOuter ts (Bindings ts)
        getbindsAB usubs fexprAB = do
            bindsA <- getbindsA usubs $ fmap fst fexprAB
            bindsB <- getbindsB usubs $ fmap snd fexprAB
            return $ bindsA <> bindsB
        in MkBound abstractNamesAB exprsAB getbindsAB

instance AbstractTypeSystem ts => Monoid (Bound ts) where
    mempty = let
        abstractNames :: forall a. TSOpenExpression ts a -> TSOuter ts (UnifyExpression ts (() -> a))
        abstractNames expr = return $ MkUnifyExpression (pure ()) $ fmap (\a _ _ -> a) expr
        exprs :: TSOpenExpression ts ()
        exprs = pure ()
        getbinds :: UnifierSubstitutions ts -> TSOpenExpression ts () -> TSOuter ts (Bindings ts)
        getbinds _ _ = return mempty
        in MkBound abstractNames exprs getbinds

singleBound ::
       forall ts. AbstractTypeSystem ts
    => Binding ts
    -> TSOuter ts (Bound ts)
singleBound (MkBinding name _ sexpr) =
    withTransConstraintTM @Monad $ do
        MkSealedExpression (twt :: _ t) expr <- rename @ts sexpr
        return $ let
            abstractNames :: forall a. TSOpenExpression ts a -> TSOuter ts (UnifyExpression ts (t -> a))
            abstractNames e = do
                MkAbstractResult vwt e' <- abstractNamedExpression @ts name e
                uconv <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit twt) vwt
                return $ MkUnifyExpression (uuGetShim uconv) $ fmap (\ta conv -> ta . shimToFunction conv) e'
            getbinds :: UnifierSubstitutions ts -> TSOpenExpression ts t -> TSOuter ts (Bindings ts)
            getbinds subs fexpr = do
                e <- unifierSubstituteAndSimplify @ts subs $ MkSealedExpression twt fexpr
                return $ singleBinding name Nothing e
            in MkBound abstractNames expr getbinds

boundToBindings ::
       forall ts. UnifyTypeSystem ts
    => Bound ts
    -> TSOuter ts (Bindings ts)
boundToBindings (MkBound abstractNames exprs getbinds) = do
    uexprvv <- abstractNames exprs
    (fexpr, subs) <- solveUnifier @ts $ unifierExpression uexprvv
    getbinds subs $ fmap fix fexpr

-- for a recursive component
bindingsComponentLetSealedExpression ::
       forall ts. (Ord (TSName ts), AbstractTypeSystem ts)
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
