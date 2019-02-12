module Language.Expression.Bindings
    ( Bindings
    , singleBinding
    , bindingsNames
    , bindingsComponentLetSealedExpression
    , valuesLetSealedExpression
    ) where

import Language.Expression.Abstract
import Language.Expression.Renamer
import Language.Expression.Sealed
import Language.Expression.Unifier
import Shapes

data Binding (unifier :: Type -> Type) where
    MkBinding :: UnifierName unifier -> UnifierSealedExpression unifier -> Binding unifier

newtype Bindings unifier =
    MkBindings [Binding unifier]
    deriving (Semigroup, Monoid)

singleBinding :: UnifierName unifier -> UnifierSealedExpression unifier -> Bindings unifier
singleBinding name expr = MkBindings $ pure $ MkBinding name expr

bindingsMap ::
       Ord (UnifierName unifier)
    => Bindings unifier
    -> StrictMap (UnifierName unifier) (UnifierSealedExpression unifier)
bindingsMap (MkBindings bb) = mapFromList $ fmap (\(MkBinding n e) -> (n, e)) bb

data Bound unifier =
    forall vals. MkBound (forall a.
                                  UnifierOpenExpression unifier a -> UnifierMonad unifier (UnifyExpression unifier (vals -> a)))
                         (UnifierOpenExpression unifier vals)
                         (UnifierSubstitutions unifier -> UnifierOpenExpression unifier vals -> UnifierMonad unifier (Bindings unifier))

mkBound ::
       forall renamer unifier m.
       ( Monad m
       , Renamer renamer
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       , UnifierMonad unifier ~ renamer m
       )
    => [Binding unifier]
    -> renamer m (Bound unifier)
mkBound [] =
    withTransConstraintTM @Monad $
    return $ MkBound (\e -> return $ exprUnifyExpression $ fmap (\a _ -> a) e) (pure ()) (\_ _ -> return mempty)
mkBound ((MkBinding name sexpr):bb) =
    withTransConstraintTM @Monad $ do
        MkSealedExpression twt expr <- rename sexpr
        MkBound abstractNames exprs getbinds <- mkBound bb
        return $ let
            abstractNames' ::
                   forall a. UnifierOpenExpression unifier a -> UnifierMonad unifier (UnifyExpression unifier (_ -> a))
            abstractNames' e = do
                MkUnifyExpression uconvRest e' <- abstractNames e
                MkAbstractResult vwt (MkUnifyExpression uconvFirst e'') <- abstractNamedExpression @unifier name e'
                uconvVar <- unifyPosNegWitnesses @unifier twt vwt
                let uresult = (,,) <$> uconvFirst <*> uconvRest <*> uconvVar
                return $
                    MkUnifyExpression uresult $
                    fmap (\ff (convFirst, convRest, convVar) ~(t, vals) -> ff convFirst (convVar t) convRest vals) e''
            exprs' = (,) <$> expr <*> exprs
            getbinds' ::
                   UnifierSubstitutions unifier
                -> UnifierOpenExpression unifier _
                -> UnifierMonad unifier (Bindings unifier)
            getbinds' subs fexpr = do
                b1 <- getbinds subs (fmap snd fexpr)
                e <- unifierSubstituteAndSimplify @unifier subs $ MkSealedExpression twt $ fmap fst fexpr
                return $ b1 <> singleBinding name e
            in MkBound abstractNames' exprs' getbinds'

boundToBindings ::
       forall unifier. Unifier unifier
    => Bound unifier
    -> UnifierMonad unifier (Bindings unifier)
boundToBindings (MkBound abstractNames exprs getbinds) = do
    uexprvv <- abstractNames exprs
    (fexpr, subs) <- solveUnifier @unifier $ unifierExpression uexprvv
    getbinds subs $ fmap fix fexpr

-- for a recursive component
bindingsComponentLetSealedExpression ::
       forall renamer unifier m.
       ( Monad m
       , Ord (UnifierName unifier)
       , Renamer renamer
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       , UnifierMonad unifier ~ renamer m
       )
    => Bindings unifier
    -> m (StrictMap (UnifierName unifier) (UnifierSealedExpression unifier))
bindingsComponentLetSealedExpression (MkBindings bindings) =
    runRenamer @renamer $
    withTransConstraintTM @Monad $ do
        bound <- mkBound bindings
        bindings' <- boundToBindings bound
        return $ bindingsMap bindings'

valuesLetSealedExpression ::
       forall unifier.
       StrictMap (UnifierName unifier) (AnyValue (UnifierPosWitness unifier))
    -> StrictMap (UnifierName unifier) (UnifierSealedExpression unifier)
valuesLetSealedExpression = fmap constSealedExpression

bindingsNames :: Bindings unifier -> [UnifierName unifier]
bindingsNames (MkBindings bb) = fmap (\(MkBinding name _) -> name) bb
