{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Bindings
    ( Bindings
    , singleBinding
    , bindingsNames
    , checkDuplicates
    , bindingsCheckDuplicates
    , bindingsLetSealedExpression
    , valuesLetSealedExpression
    ) where

import Data.Graph
import Language.Expression.Abstract
import Language.Expression.Named
import Language.Expression.Renamer
import Language.Expression.Sealed
import Language.Expression.Unifier
import Shapes

data Binding (name :: Type) (unifier :: Type -> Type) where
    MkBinding
        :: name -> SealedExpression name (UnifierNegWitness unifier) (UnifierPosWitness unifier) -> Binding name unifier

newtype Bindings name unifier =
    MkBindings [Binding name unifier]
    deriving (Semigroup, Monoid)

singleBinding ::
       name -> SealedExpression name (UnifierNegWitness unifier) (UnifierPosWitness unifier) -> Bindings name unifier
singleBinding name expr = MkBindings $ pure $ MkBinding name expr

bindingsMap ::
       Ord name
    => Bindings name unifier
    -> StrictMap name (SealedExpression name (UnifierNegWitness unifier) (UnifierPosWitness unifier))
bindingsMap (MkBindings bb) = mapFromList $ fmap (\(MkBinding n e) -> (n, e)) bb

data Bound name unifier =
    forall vals. MkBound (forall a.
                                  NamedExpression name (UnifierNegWitness unifier) a -> UnifierMonad unifier (UnifyExpression name unifier (vals -> a)))
                         (NamedExpression name (UnifierNegWitness unifier) vals)
                         (UnifierSubstitutions unifier -> NamedExpression name (UnifierNegWitness unifier) vals -> UnifierMonad unifier (Bindings name unifier))

mkBound ::
       forall name renamer unifier m.
       ( Eq name
       , Monad m
       , Renamer renamer
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       , UnifierMonad unifier ~ renamer m
       )
    => [Binding name unifier]
    -> renamer m (Bound name unifier)
mkBound [] =
    withTransConstraintTM @Monad $
    return $ MkBound (\e -> return $ unifyExpression $ fmap (\a _ -> a) e) (pure ()) (\_ _ -> return mempty)
mkBound ((MkBinding name sexpr):bb) =
    withTransConstraintTM @Monad $ do
        MkSealedExpression twt expr <- renameSealedExpression sexpr
        MkBound abstractNames exprs getbinds <- mkBound bb
        return $ let
            abstractNames' ::
                   forall a.
                   NamedExpression name (UnifierNegWitness unifier) a
                -> UnifierMonad unifier (UnifyExpression name unifier (_ -> a))
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
                -> NamedExpression name (UnifierNegWitness unifier) _
                -> UnifierMonad unifier (Bindings name unifier)
            getbinds' subs fexpr = do
                b1 <- getbinds subs (fmap snd fexpr)
                e <- unifierExpressionSubstituteAndSimplify @unifier subs twt $ fmap fst fexpr
                return $ b1 <> singleBinding name e
            in MkBound abstractNames' exprs' getbinds'

boundToBindings ::
       forall name unifier. (Eq name, Unifier unifier)
    => Bound name unifier
    -> UnifierMonad unifier (Bindings name unifier)
boundToBindings (MkBound abstractNames exprs getbinds) = do
    uexprvv <- abstractNames exprs
    (fexpr, subs) <- solveUnifier @unifier $ unifierExpression uexprvv
    getbinds subs $ fmap fix fexpr

getBindingNode :: Binding name unifier -> (Binding name unifier, name, [name])
getBindingNode b@(MkBinding n expr) = (b, n, sealedExpressionFreeNames expr)

-- | Group bindings into a topologically-sorted list of strongly-connected components
clumpBindings :: Ord name => Bindings name unifier -> [Bindings name unifier]
clumpBindings (MkBindings bb) = fmap (MkBindings . flattenSCC) $ stronglyConnComp $ fmap getBindingNode bb

bindingsLetSealedExpression ::
       forall renamer unifier m name.
       ( Monad m
       , Ord name
       , Renamer renamer
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       , UnifierMonad unifier ~ renamer m
       )
    => Bindings name unifier
    -> SealedExpression name (RenamerNegWitness renamer) (RenamerPosWitness renamer)
    -> m (SealedExpression name (RenamerNegWitness renamer) (RenamerPosWitness renamer))
bindingsLetSealedExpression bindings = let
    doClumps [] expr = return expr
    doClumps (b:bb) expr = do
        expr' <- doClumps bb expr
        bindingsComponentLetSealedExpression b expr'
    in doClumps $ clumpBindings bindings

bindingsComponentLetSealedExpression ::
       forall renamer unifier m name.
       ( Monad m
       , Ord name
       , Renamer renamer
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       , UnifierMonad unifier ~ renamer m
       )
    => Bindings name unifier
    -> SealedExpression name (RenamerNegWitness renamer) (RenamerPosWitness renamer)
    -> m (SealedExpression name (RenamerNegWitness renamer) (RenamerPosWitness renamer))
bindingsComponentLetSealedExpression (MkBindings bindings) sexprb =
    runRenamer @renamer $
    withTransConstraintTM @Monad $ do
        bound <- mkBound bindings
        bindings' <- boundToBindings bound
        MkSealedExpression tb exprb <- renameSealedExpression sexprb
        uexprb' <- letBindNamedExpression @unifier (\name -> lookup name $ bindingsMap bindings') exprb
        (exprb', subs) <- solveUnifier @unifier $ unifierExpression uexprb'
        unifierExpressionSubstituteAndSimplify @unifier subs tb exprb'

valuesLetSealedExpression ::
       forall renamer unifier m name.
       ( Monad m
       , Ord name
       , Renamer renamer
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       , UnifierMonad unifier ~ renamer m
       )
    => (name -> Maybe (AnyValue (UnifierPosWitness unifier)))
    -> SealedExpression name (RenamerNegWitness renamer) (RenamerPosWitness renamer)
    -> m (SealedExpression name (RenamerNegWitness renamer) (RenamerPosWitness renamer))
valuesLetSealedExpression valbind sexprb =
    runRenamer @renamer $
    withTransConstraintTM @Monad $ do
        MkSealedExpression tb exprb <- renameSealedExpression sexprb
        uexprb' <- letBindNamedExpression @unifier (\name -> fmap constSealedExpression $ valbind name) exprb
        (exprb', subs) <- solveUnifier @unifier $ unifierExpression uexprb'
        unifierExpressionSubstituteAndSimplify @unifier subs tb exprb'

duplicates ::
       forall a. Eq a
    => [a]
    -> [a]
duplicates [] = []
duplicates (a:aa)
    | elem a aa = a : duplicates aa
duplicates (_:aa) = duplicates aa

bindingsNames :: Bindings name unifier -> [name]
bindingsNames (MkBindings bb) = fmap (\(MkBinding name _) -> name) bb

checkDuplicates :: (Show name, Eq name, MonadFail m) => [name] -> m ()
checkDuplicates nn =
    case nub $ duplicates nn of
        [] -> return ()
        b -> fail $ "duplicate bindings: " <> (intercalate ", " $ fmap show b)

bindingsCheckDuplicates :: (Show name, Eq name, MonadFail m) => Bindings name unifier -> m ()
bindingsCheckDuplicates bindings = checkDuplicates $ bindingsNames bindings
