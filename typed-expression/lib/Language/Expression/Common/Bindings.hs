{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Bindings
    ( Bindings
    , singleBinding
    , bindingsNames
    , bindingsComponentLetSealedExpression
    , valuesLetSealedExpression
    ) where

import Data.Shim.JoinMeet
import Language.Expression.Common.Abstract
import Language.Expression.Common.Rename.RenameTypeSystem
import Language.Expression.Common.Sealed
import Language.Expression.Common.TypeSystem
import Language.Expression.Common.Unifier
import Shapes

data Binding (ts :: Type) where
    MkBinding :: TSName ts -> TSSealedExpression ts -> Binding ts

newtype Bindings ts =
    MkBindings [Binding ts]
    deriving (Semigroup, Monoid)

singleBinding :: TSName ts -> TSSealedExpression ts -> Bindings ts
singleBinding name expr = MkBindings $ pure $ MkBinding name expr

bindingsMap :: Ord (TSName ts) => Bindings ts -> Map (TSName ts) (TSSealedExpression ts)
bindingsMap (MkBindings bb) = mapFromList $ fmap (\(MkBinding n e) -> (n, e)) bb

data UnifyExpression ts a =
    forall t. MkUnifyExpression (Unifier ts t)
                                (TSOpenExpression ts (t -> a))

exprUnifyExpression :: UnifyTypeSystem ts => TSOpenExpression ts a -> UnifyExpression ts a
exprUnifyExpression expr = MkUnifyExpression (pure ()) $ fmap (\a _ -> a) expr

unifierExpression :: Functor (Unifier ts) => UnifyExpression ts a -> Unifier ts (TSOpenExpression ts a)
unifierExpression (MkUnifyExpression uconv expr) = fmap (\conv -> fmap (\conva -> conva conv) expr) uconv

data Bound ts =
    forall vals. MkBound (forall a. TSOpenExpression ts a -> TSOuter ts (UnifyExpression ts (vals -> a)))
                         (TSOpenExpression ts vals)
                         (UnifierSubstitutions ts -> TSOpenExpression ts vals -> TSOuter ts (Bindings ts))

mkBound ::
       forall ts. AbstractTypeSystem ts
    => [Binding ts]
    -> TSOuter ts (Bound ts)
mkBound [] =
    withTransConstraintTM @Monad $
    return $ MkBound (\e -> return $ exprUnifyExpression $ fmap (\a _ -> a) e) (pure ()) (\_ _ -> return mempty)
mkBound ((MkBinding name sexpr):bb) =
    withTransConstraintTM @Monad $ do
        MkSealedExpression twt expr <- rename @ts sexpr
        MkBound abstractNames exprs getbinds <- mkBound bb
        return $ let
            abstractNames' :: forall a. TSOpenExpression ts a -> TSOuter ts (UnifyExpression ts (_ -> a))
            abstractNames' e = do
                MkUnifyExpression uconvRest e' <- abstractNames e
                MkAbstractResult vwt e'' <- abstractNamedExpression @ts name e'
                uconvFirst <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit twt) vwt
                let
                    uresult = do
                        convFirst <- uuGetShim uconvFirst
                        convRest <- uconvRest
                        pure (convFirst, convRest)
                return $
                    MkUnifyExpression uresult $
                    fmap (\ff (convFirst, convRest) ~(t, vals) -> ff (shimToFunction convFirst t) convRest vals) e''
            exprs' = (,) <$> expr <*> exprs
            getbinds' :: UnifierSubstitutions ts -> TSOpenExpression ts _ -> TSOuter ts (Bindings ts)
            getbinds' subs fexpr = do
                b1 <- getbinds subs (fmap snd fexpr)
                e <- unifierSubstituteAndSimplify @ts subs $ MkSealedExpression twt $ fmap fst fexpr
                return $ b1 <> singleBinding name e
            in MkBound abstractNames' exprs' getbinds'

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
        bound <- mkBound bindings
        bindings' <- boundToBindings bound
        return $ bindingsMap bindings'

valuesLetSealedExpression ::
       forall ts. UnifyTypeSystem ts
    => Map (TSName ts) (AnyValue (TSPosShimWit ts))
    -> Map (TSName ts) (TSSealedExpression ts)
valuesLetSealedExpression = fmap constSealedExpression

bindingsNames :: Bindings ts -> [TSName ts]
bindingsNames (MkBindings bb) = fmap (\(MkBinding name _) -> name) bb
