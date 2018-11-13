{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Bindings
    ( Bindings
    , singleBinding
    , bindingsNames
    , checkDuplicates
    , bindingsCheckDuplicates
    , bindingsLetSealedExpression
    ) where

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

data Bound name unifier =
    forall vals. MkBound (forall a.
                                  NamedExpression name (UnifierNegWitness unifier) a -> UnifierMonad unifier (UnifyExpression name unifier (vals -> a)))
                         (NamedExpression name (UnifierNegWitness unifier) vals)

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
    withTransConstraintTM @Monad $ return $ MkBound (\e -> return $ unifyExpression $ fmap (\a _ -> a) e) (pure ())
mkBound ((MkBinding name sexpr):bb) =
    withTransConstraintTM @Monad $ do
        MkSealedExpression twt expr <- renameSealedExpression sexpr
        MkBound abstractNames exprs <- mkBound bb
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
            in MkBound abstractNames' exprs'

bindingsLetSealedExpression ::
       forall renamer unifier m name.
       ( Monad m
       , Eq name
       , Renamer renamer
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       , UnifierMonad unifier ~ renamer m
       )
    => Bindings name unifier
    -> SealedExpression name (RenamerNegWitness renamer) (RenamerPosWitness renamer)
    -> m (SealedExpression name (RenamerNegWitness renamer) (RenamerPosWitness renamer))
bindingsLetSealedExpression (MkBindings bindings) sexprb =
    runRenamer @renamer $
    withTransConstraintTM @Monad $ do
        MkBound abstractNames exprs <- mkBound bindings
        MkSealedExpression tb exprb <- renameSealedExpression sexprb
        ues <- abstractNames exprs
        ueb <- abstractNames exprb
        ((es, eb), subs) <- solveUnifier @unifier $ (,) <$> unifierExpression ues <*> unifierExpression ueb
        unifierPosSubstitute @unifier subs tb $ \tb' tconv ->
            return $ unifierExpressionSubstituteAndSimplify @unifier subs tb' $ fmap tconv $ eb <*> fmap fix es

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
