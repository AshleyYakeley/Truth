module Language.Expression.Abstract where

import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Pattern
import Language.Expression.Renamer
import Language.Expression.Sealed
import Language.Expression.Unifier
import Shapes

abstractNamedExpressionUnifier ::
       forall unifier t a r. Unifier unifier
    => UnifierName unifier
    -> UnifierNegWitness unifier t
    -> UnifierOpenExpression unifier a
    -> (forall tu t'.
                UnifierNegWitness unifier t' -> unifier (t' -> (t, tu)) -> UnifierOpenExpression unifier (tu -> a) -> UnifierMonad unifier r)
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

data AbstractResult unifier a =
    forall t. MkAbstractResult (UnifierNegWitness unifier t)
                               (UnifyExpression unifier (t -> a))

instance Functor (AbstractResult unifier) where
    fmap ab (MkAbstractResult vwt uexpr) = MkAbstractResult vwt $ fmap (fmap ab) uexpr

abstractNamedExpression ::
       forall unifier renamer m a.
       ( Renamer renamer
       , Monad m
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       , UnifierMonad unifier ~ renamer m
       )
    => UnifierName unifier
    -> UnifierOpenExpression unifier a
    -> UnifierMonad unifier (AbstractResult unifier a)
abstractNamedExpression name expr =
    renameNewVar $ \vwt0 _ _ ->
        abstractNamedExpressionUnifier @unifier name vwt0 expr $ \vwt uconv expr' ->
            return $ MkAbstractResult vwt $ MkUnifyExpression uconv $ fmap (\tua t'ttu -> tua . snd . t'ttu) expr'

abstractUnifyExpression ::
       forall unifier renamer m a.
       ( Renamer renamer
       , Monad m
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       , UnifierMonad unifier ~ renamer m
       )
    => UnifierName unifier
    -> UnifyExpression unifier a
    -> UnifierMonad unifier (AbstractResult unifier a)
abstractUnifyExpression name (MkUnifyExpression uconv expr) = do
    MkAbstractResult vwt (MkUnifyExpression uabsconv expr') <- abstractNamedExpression @unifier name expr
    return $
        MkAbstractResult vwt $
        MkUnifyExpression ((,) <$> uabsconv <*> uconv) $ fmap (\f (absconv, conv) t -> f absconv t conv) expr'

patternAbstractUnifyExpression ::
       forall unifier renamer m q a t.
       ( Renamer renamer
       , Monad m
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       , UnifierMonad unifier ~ renamer m
       )
    => UnifierOpenPattern unifier q t
    -> UnifyExpression unifier a
    -> UnifierMonad unifier (UnifyExpression unifier (q -> Maybe (a, t)))
patternAbstractUnifyExpression (ClosedPattern qmt) expr = return $ fmap (\a q -> qmt q >>= \t -> Just (a, t)) expr
patternAbstractUnifyExpression (OpenPattern (MkNameWitness name vwt) pat) expr = do
    MkAbstractResult absvwt absexpr <- abstractUnifyExpression @unifier name expr
    uabsconv <- unifyPosNegWitnesses vwt absvwt
    MkUnifyExpression uconv rexpr <- patternAbstractUnifyExpression @unifier pat absexpr
    return $
        MkUnifyExpression ((,) <$> uabsconv <*> uconv) $
        fmap (\f (absconv, conv) q -> fmap (\(t2a, (t1, t)) -> (t2a $ absconv t1, t)) $ f conv q) rexpr

patternAbstractExpression ::
       forall unifier renamer m a.
       ( Renamer renamer
       , Monad m
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       , UnifierMonad unifier ~ renamer m
       )
    => UnifierSealedPattern unifier
    -> UnifyExpression unifier a
    -> UnifierMonad unifier (AbstractResult unifier (Maybe a))
patternAbstractExpression (MkSealedPattern vwt pat) uexpr = do
    uexpr' <- patternAbstractUnifyExpression @unifier pat uexpr
    return $ fmap (fmap fst) $ MkAbstractResult vwt uexpr'

letBindNamedExpression ::
       forall unifier renamer m a.
       ( Renamer renamer
       , Monad m
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       , UnifierMonad unifier ~ renamer m
       )
    => (UnifierName unifier -> Maybe (UnifierSealedExpression unifier))
    -> UnifierOpenExpression unifier a
    -> UnifierMonad unifier (UnifyExpression unifier a)
letBindNamedExpression _ (ClosedExpression a) = return $ pure a
letBindNamedExpression bindmap (OpenExpression (MkNameWitness name vwt) expr) = do
    uerest <- letBindNamedExpression @unifier bindmap expr
    case bindmap name of
        Just bindexpr -> do
            MkSealedExpression twt bindexpr' <- renameSealedExpression bindexpr
            ubindconv <- unifyPosNegWitnesses @unifier twt vwt
            let uebind = MkUnifyExpression ubindconv $ fmap (\t1 tt -> tt t1) bindexpr'
            return $ uerest <*> uebind
        Nothing ->
            return $
            case uerest of
                MkUnifyExpression uconv expr' ->
                    MkUnifyExpression uconv $ OpenExpression (MkNameWitness name vwt) (fmap (\cta t c -> cta c t) expr')

type FunctionPosWitness vw tw = forall r a b. vw a -> tw b -> (forall f. tw f -> ((a -> b) -> f) -> r) -> r

type FunctionNegWitness vw tw = forall r a b. tw a -> vw b -> (forall f. vw f -> (f -> (a -> b)) -> r) -> r

abstractSealedExpression ::
       forall renamer unifier m.
       ( Monad m
       , Renamer renamer
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       , UnifierMonad unifier ~ renamer m
       )
    => FunctionPosWitness (RenamerNegWitness renamer) (RenamerPosWitness renamer)
    -> UnifierName unifier
    -> UnifierSealedExpression unifier
    -> m (UnifierSealedExpression unifier)
abstractSealedExpression absw name sexpr =
    runRenamer @renamer $
    withTransConstraintTM @Monad $ do
        MkSealedExpression twt expr <- renameSealedExpression sexpr
        MkAbstractResult vwt (unifierExpression -> uexpr') <- abstractNamedExpression @unifier name expr
        (expr', subs) <- solveUnifier @unifier uexpr'
        absw vwt twt $ \twf abconv -> unifierExpressionSubstituteAndSimplify @unifier subs twf $ fmap abconv expr'

applySealedExpression ::
       forall renamer unifier m.
       ( Monad m
       , Renamer renamer
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       , UnifierMonad unifier ~ renamer m
       )
    => FunctionNegWitness (RenamerNegWitness renamer) (RenamerPosWitness renamer)
    -> UnifierSealedExpression unifier
    -> UnifierSealedExpression unifier
    -> m (UnifierSealedExpression unifier)
applySealedExpression appw sexprf sexpra =
    runRenamer @renamer $
    withTransConstraintTM @Monad $ do
        MkSealedExpression tf exprf <- renameSealedExpression sexprf
        MkSealedExpression ta expra <- renameSealedExpression sexpra
        renameNewVar $ \vx tx convvar ->
            appw ta vx $ \vax convf -> do
                uconv <- unifyPosNegWitnesses tf vax
                (convu, subs) <- solveUnifier @unifier uconv
                unifierExpressionSubstituteAndSimplify @unifier subs tx $
                    (\t t1 -> convvar $ convf (convu t) t1) <$> exprf <*> expra

-- | not recursive
letSealedExpression ::
       forall renamer unifier m.
       ( Monad m
       , Renamer renamer
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       , UnifierMonad unifier ~ renamer m
       )
    => UnifierName unifier
    -> UnifierSealedExpression unifier
    -> UnifierSealedExpression unifier
    -> m (UnifierSealedExpression unifier)
letSealedExpression name sexpre sexprb =
    runRenamer @renamer $
    withTransConstraintTM @Monad $ do
        MkSealedExpression te expre <- renameSealedExpression sexpre
        MkSealedExpression tb exprb <- renameSealedExpression sexprb
        MkAbstractResult vt (unifierExpression -> uexprf) <- abstractNamedExpression @unifier name exprb
        uconvet <- unifyPosNegWitnesses te vt
        (exprf', subs) <-
            solveUnifier @unifier $ (\exprf convet -> fmap (\tt2 -> tt2 . convet) exprf) <$> uexprf <*> uconvet
        unifierExpressionSubstituteAndSimplify @unifier subs tb $ exprf' <*> expre
