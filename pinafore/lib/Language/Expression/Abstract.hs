{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Abstract where

import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Renamer
import Language.Expression.Sealed
import Language.Expression.Unifier
import Shapes

abstractNamedExpressionUnifier ::
       forall unifier name t a r. (Eq name, Unifier unifier)
    => name
    -> UnifierNegWitness unifier t
    -> NamedExpression name (UnifierNegWitness unifier) a
    -> (forall tu t'.
                UnifierNegWitness unifier t' -> unifier (t' -> (t, tu)) -> NamedExpression name (UnifierNegWitness unifier) (tu -> a) -> UnifierMonad unifier r)
    -> UnifierMonad unifier r
abstractNamedExpressionUnifier _name vwt (ClosedExpression a) cont =
    cont vwt (pure $ \t -> (t, t)) $ ClosedExpression $ \_ -> a
abstractNamedExpressionUnifier name vwt (OpenExpression (MkNameWitness name' vwt') expr) cont
    | name == name' =
        abstractNamedExpressionUnifier name vwt expr $ \vwt1 uconv' expr' ->
            unifyNegWitnesses vwt1 vwt' $ \vwtt uconv -> let
                convconv (conva, convb) conv' ab = let
                    t' = conva ab
                    t1 = convb ab
                    (t, tu) = conv' t'
                    in (t, (tu, t1))
                in cont vwtt (convconv <$> uconv <*> uconv') $ fmap (\tta (ta, tb) -> tta ta tb) expr'
abstractNamedExpressionUnifier name vwt (OpenExpression (MkNameWitness name' vwt') expr) cont =
    abstractNamedExpressionUnifier name vwt expr $ \vwt1 uconv' expr' ->
        cont vwt1 uconv' $ OpenExpression (MkNameWitness name' vwt') $ fmap (\vva v1 v2 -> vva v2 v1) expr'

data AbstractResult name unifier a =
    forall t. MkAbstractResult (UnifierNegWitness unifier t)
                               (UnifyExpression name unifier (t -> a))

newtype Abstracter unifier =
    MkAbstracter (forall name a.
                      Eq name =>
                              name -> NamedExpression name (UnifierNegWitness unifier) a -> UnifierMonad unifier (AbstractResult name unifier a))

abstractNamedExpression ::
       forall unifier renamer m.
       ( Renamer renamer
       , Monad m
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       )
    => renamer m (Abstracter unifier)
abstractNamedExpression =
    renameNewVar $ \vwt0 _ _ ->
        withTransConstraintTM @Monad $
        return $
        MkAbstracter $ \name expr ->
            abstractNamedExpressionUnifier @unifier name vwt0 expr $ \vwt uconv expr' ->
                return $ MkAbstractResult vwt $ MkUnifyExpression uconv $ fmap (\tua t'ttu -> tua . snd . t'ttu) expr'

type FunctionPosWitness vw tw = forall r a b. vw a -> tw b -> (forall f. tw f -> ((a -> b) -> f) -> r) -> r

type FunctionNegWitness vw tw = forall r a b. tw a -> vw b -> (forall f. vw f -> (f -> (a -> b)) -> r) -> r

abstractSealedExpression ::
       forall renamer unifier name.
       ( Eq name
       , Renamer renamer
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       )
    => FunctionPosWitness (RenamerNegWitness renamer) (RenamerPosWitness renamer)
    -> name
    -> SealedExpression name (RenamerNegWitness renamer) (RenamerPosWitness renamer)
    -> UnifierMonad unifier (SealedExpression name (RenamerNegWitness renamer) (RenamerPosWitness renamer))
abstractSealedExpression absw name sexpr =
    runRenamer @renamer $
    withTransConstraintTM @Monad $ do
        MkSealedExpression twt expr <- renameSealedExpression sexpr
        MkAbstracter abstract <- abstractNamedExpression @unifier
        lift $ do
            MkAbstractResult vwt (unifierExpression -> uexpr') <- abstract name expr
            (expr', subs) <- solveUnifier @unifier uexpr'
            unifierPosSubstitute @unifier subs twt $ \twt' tconv ->
                absw vwt twt' $ \twf abconv ->
                    return $ unifierExpressionSubstituteAndSimplify @unifier subs twf $ fmap (abconv . fmap tconv) expr'

applySealedExpression ::
       forall renamer unifier name.
       ( Renamer renamer
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       )
    => FunctionNegWitness (RenamerNegWitness renamer) (RenamerPosWitness renamer)
    -> SealedExpression name (RenamerNegWitness renamer) (RenamerPosWitness renamer)
    -> SealedExpression name (RenamerNegWitness renamer) (RenamerPosWitness renamer)
    -> UnifierMonad unifier (SealedExpression name (RenamerNegWitness renamer) (RenamerPosWitness renamer))
applySealedExpression appw sexprf sexpra =
    runRenamer @renamer $
    withTransConstraintTM @Monad $ do
        MkSealedExpression tf exprf <- renameSealedExpression sexprf
        MkSealedExpression ta expra <- renameSealedExpression sexpra
        renameNewVar $ \vx tx convvar ->
            lift $
            appw ta vx $ \vax convf -> do
                uconv <- getCompose $ unifyPosNegWitnesses tf vax
                (convu, subs) <- solveUnifier @unifier uconv
                unifierPosSubstitute @unifier subs tx $ \tx' tconv ->
                    return $
                    unifierExpressionSubstituteAndSimplify @unifier subs tx' $
                    (\t t1 -> tconv $ convvar $ convf (convu t) t1) <$> exprf <*> expra

-- | not recursive
letSealedExpression ::
       forall renamer unifier name.
       ( Eq name
       , Renamer renamer
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       )
    => name
    -> SealedExpression name (RenamerNegWitness renamer) (RenamerPosWitness renamer)
    -> SealedExpression name (RenamerNegWitness renamer) (RenamerPosWitness renamer)
    -> UnifierMonad unifier (SealedExpression name (RenamerNegWitness renamer) (RenamerPosWitness renamer))
letSealedExpression name sexpre sexprb =
    runRenamer @renamer $
    withTransConstraintTM @Monad $ do
        MkSealedExpression te expre <- renameSealedExpression sexpre
        MkSealedExpression tb exprb <- renameSealedExpression sexprb
        MkAbstracter abstract <- abstractNamedExpression @unifier
        lift $ do
            MkAbstractResult vt (unifierExpression -> uexprf) <- abstract name exprb
            uconvet <- getCompose $ unifyPosNegWitnesses te vt
            (exprf', subs) <-
                solveUnifier @unifier $ (\exprf convet -> fmap (\tt2 -> tt2 . convet) exprf) <$> uexprf <*> uconvet
            unifierPosSubstitute @unifier subs tb $ \tb' tconv ->
                return $ unifierExpressionSubstituteAndSimplify @unifier subs tb' $ (fmap tconv) <$> exprf' <*> expre