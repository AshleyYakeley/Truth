{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Bindings
    ( Bindings
    , singleBinding
    , bindingsNames
    , bindingsCheckDuplicates
    , bindingsLetSealedExpression
    , valuesLetSealedExpression
    ) where

import Data.Graph
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
    return $ MkBound (\e -> return $ unifyExpression $ fmap (\a _ -> a) e) (pure ()) (\_ _ -> return mempty)
mkBound ((MkBinding name sexpr):bb) =
    withTransConstraintTM @Monad $ do
        MkSealedExpression twt expr <- renameSealedExpression sexpr
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
                e <- unifierExpressionSubstituteAndSimplify @unifier subs twt $ fmap fst fexpr
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

getBindingNode :: Binding unifier -> (Binding unifier, UnifierName unifier, [UnifierName unifier])
getBindingNode b@(MkBinding n expr) = (b, n, sealedExpressionFreeNames expr)

-- | Group bindings into a topologically-sorted list of strongly-connected components
clumpBindings :: Ord (UnifierName unifier) => Bindings unifier -> [Bindings unifier]
clumpBindings (MkBindings bb) = fmap (MkBindings . flattenSCC) $ stronglyConnComp $ fmap getBindingNode bb

bindingsLetSealedExpression ::
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
bindingsLetSealedExpression bindings = let
    doClumps [] = return mempty
    doClumps (b:bb) = do
        m1 <- bindingsComponentLetSealedExpression @renamer @unifier b
        bb' <-
            for bb $ \(MkBindings binds) -> do
                binds' <-
                    for binds $ \(MkBinding n sexprb) -> do
                        runRenamer @renamer $
                            withTransConstraintTM @Monad $ do
                                MkSealedExpression tb exprb <- renameSealedExpression sexprb
                                uexprb' <- letBindNamedExpression @unifier (\name -> lookup name m1) exprb
                                (exprb', subs) <- solveUnifier @unifier $ unifierExpression uexprb'
                                sexprb' <- unifierExpressionSubstituteAndSimplify @unifier subs tb exprb'
                                return $ MkBinding n sexprb'
                return $ MkBindings binds'
        m2 <- doClumps bb'
        return $ m1 <> m2
    in doClumps $ clumpBindings bindings

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

duplicates ::
       forall a. Eq a
    => [a]
    -> [a]
duplicates [] = []
duplicates (a:aa)
    | elem a aa = a : duplicates aa
duplicates (_:aa) = duplicates aa

bindingsNames :: Bindings unifier -> [UnifierName unifier]
bindingsNames (MkBindings bb) = fmap (\(MkBinding name _) -> name) bb

checkDuplicates :: (Show name, Eq name, MonadFail m) => [name] -> m ()
checkDuplicates nn =
    case nub $ duplicates nn of
        [] -> return ()
        b -> fail $ "duplicate bindings: " <> (intercalate ", " $ fmap show b)

bindingsCheckDuplicates ::
       (Show (UnifierName unifier), Eq (UnifierName unifier), MonadFail m) => Bindings unifier -> m ()
bindingsCheckDuplicates bindings = checkDuplicates $ bindingsNames bindings
