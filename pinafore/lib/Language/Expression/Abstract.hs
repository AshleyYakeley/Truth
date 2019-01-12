module Language.Expression.Abstract where

import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Pattern
import Language.Expression.Renamer
import Language.Expression.Sealed
import Language.Expression.Unifier
import Shapes

type UnifierRenamerConstraint unifier renamer m
     = ( Renamer renamer
       , Monad m
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       , UnifierMonad unifier ~ renamer m)

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
       forall unifier renamer m a. UnifierRenamerConstraint unifier renamer m
    => UnifierName unifier
    -> UnifierOpenExpression unifier a
    -> UnifierMonad unifier (AbstractResult unifier a)
abstractNamedExpression name expr = do
    MkNewVar vwt0 _ _ <- renameNewVar
    abstractNamedExpressionUnifier @unifier name vwt0 expr $ \vwt uconv expr' ->
        return $ MkAbstractResult vwt $ MkUnifyExpression uconv $ fmap (\tua t'ttu -> tua . snd . t'ttu) expr'

abstractUnifyExpression ::
       forall unifier renamer m a. UnifierRenamerConstraint unifier renamer m
    => UnifierName unifier
    -> UnifyExpression unifier a
    -> UnifierMonad unifier (AbstractResult unifier a)
abstractUnifyExpression name (MkUnifyExpression uconv expr) = do
    MkAbstractResult vwt (MkUnifyExpression uabsconv expr') <- abstractNamedExpression @unifier name expr
    return $
        MkAbstractResult vwt $
        MkUnifyExpression ((,) <$> uabsconv <*> uconv) $ fmap (\f (absconv, conv) t -> f absconv t conv) expr'

patternAbstractUnifyExpression ::
       forall unifier renamer m q a t. UnifierRenamerConstraint unifier renamer m
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

data PatternResult unifier f =
    forall t a. MkPatternResult (UnifierNegWitness unifier t)
                                (UnifierPosWitness unifier a)
                                (UnifyExpression unifier (t -> f a))

joinPatternResult ::
       forall unifier. Unifier unifier
    => PatternResult unifier Maybe
    -> PatternResult unifier Identity
    -> UnifierMonad unifier (PatternResult unifier Identity)
joinPatternResult (MkPatternResult vwtp twtp exprp) (MkPatternResult vwtc twtc exprc) =
    unifyNegWitnesses @unifier vwtp vwtc $ \vwtpc uvwt ->
        unifyPosWitnesses @unifier twtp twtc $ \twtpc utwt -> let
            ff :: (ab -> t, ab -> t1)
               -> (a -> ab1, a1 -> ab1)
               -> (t -> Maybe a)
               -> (t1 -> Identity a1)
               -> ab
               -> Identity ab1
            ff (abt, abt1) (aab1, a1ab1) tma t1a1 ab =
                case tma $ abt ab of
                    Just a -> Identity $ aab1 a
                    Nothing -> fmap a1ab1 $ t1a1 $ abt1 ab
            in return $
               MkPatternResult vwtpc twtpc $
               ff <$> unifierUnifyExpression uvwt <*> unifierUnifyExpression utwt <*> exprp <*> exprc

joinPatternResults ::
       UnifierRenamerConstraint unifier renamer m
    => [PatternResult unifier Maybe]
    -> UnifierMonad unifier (PatternResult unifier Identity)
joinPatternResults [] = do
    MkNewVar tt _ _ <- renameNewVar
    MkNewVar _ ta _ <- renameNewVar
    return $ MkPatternResult tt ta $ pure $ \_ -> Identity $ error "missing case"
joinPatternResults (p:pp) = do
    c <- joinPatternResults pp
    joinPatternResult p c

patternAbstractSealedExpression ::
       forall unifier renamer m. UnifierRenamerConstraint unifier renamer m
    => UnifierSealedPattern unifier
    -> UnifierSealedExpression unifier
    -> UnifierMonad unifier (PatternResult unifier Maybe)
patternAbstractSealedExpression (MkSealedPattern vwt pat) (MkSealedExpression twt expr) = do
    uexpr' <- patternAbstractUnifyExpression @unifier pat $ exprUnifyExpression expr
    return $ MkPatternResult vwt twt $ fmap (fmap (fmap fst)) uexpr'

letBindNamedExpression ::
       forall unifier renamer m a. UnifierRenamerConstraint unifier renamer m
    => (UnifierName unifier -> Maybe (UnifierSealedExpression unifier))
    -> UnifierOpenExpression unifier a
    -> UnifierMonad unifier (UnifyExpression unifier a)
letBindNamedExpression _ (ClosedExpression a) = return $ pure a
letBindNamedExpression bindmap (OpenExpression (MkNameWitness name vwt) expr) = do
    uerest <- letBindNamedExpression @unifier bindmap expr
    case bindmap name of
        Just bindexpr -> do
            MkSealedExpression twt bindexpr' <- rename bindexpr
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
       forall renamer unifier m. UnifierRenamerConstraint unifier renamer m
    => FunctionPosWitness (RenamerNegWitness renamer) (RenamerPosWitness renamer)
    -> UnifierName unifier
    -> UnifierSealedExpression unifier
    -> m (UnifierSealedExpression unifier)
abstractSealedExpression absw name sexpr =
    runRenamer @renamer $
    withTransConstraintTM @Monad $ do
        MkSealedExpression twt expr <- rename sexpr
        MkAbstractResult vwt (unifierExpression -> uexpr') <- abstractNamedExpression @unifier name expr
        absw vwt twt $ \twf abconv -> do
            unifierSolve @unifier $ fmap (\expr' -> MkSealedExpression twf $ fmap abconv expr') uexpr'

applySealedExpression ::
       forall renamer unifier m. UnifierRenamerConstraint unifier renamer m
    => FunctionNegWitness (RenamerNegWitness renamer) (RenamerPosWitness renamer)
    -> UnifierSealedExpression unifier
    -> UnifierSealedExpression unifier
    -> m (UnifierSealedExpression unifier)
applySealedExpression appw sexprf sexpra =
    runRenamer @renamer $
    withTransConstraintTM @Monad $ do
        MkSealedExpression tf exprf <- rename sexprf
        MkSealedExpression ta expra <- rename sexpra
        MkNewVar vx tx convvar <- renameNewVar
        appw ta vx $ \vax convf -> do
            uconv <- unifyPosNegWitnesses @unifier tf vax
            unifierSolve @unifier $
                fmap
                    (\convu -> MkSealedExpression tx $ (\t t1 -> convvar $ convf (convu t) t1) <$> exprf <*> expra)
                    uconv

-- | not recursive
letSealedExpression ::
       forall renamer unifier m. UnifierRenamerConstraint unifier renamer m
    => UnifierName unifier
    -> UnifierSealedExpression unifier
    -> UnifierSealedExpression unifier
    -> m (UnifierSealedExpression unifier)
letSealedExpression name sexpre sexprb =
    runRenamer @renamer $
    withTransConstraintTM @Monad $ do
        MkSealedExpression te expre <- rename sexpre
        MkSealedExpression tb exprb <- rename sexprb
        MkAbstractResult vt (unifierExpression -> uexprf) <- abstractNamedExpression @unifier name exprb
        uconvet <- unifyPosNegWitnesses @unifier te vt
        unifierSolve @unifier $
            (\exprf convet -> MkSealedExpression tb $ fmap (\tt2 -> tt2 . convet) exprf <*> expre) <$> uexprf <*>
            uconvet

bothSealedPattern ::
       forall renamer unifier m. UnifierRenamerConstraint unifier renamer m
    => UnifierSealedPattern unifier
    -> UnifierSealedPattern unifier
    -> m (UnifierSealedPattern unifier)
bothSealedPattern spat1 spat2 =
    runRenamer @renamer $
    withTransConstraintTM @Monad $ do
        MkSealedPattern tw1 pat1 <- rename spat1
        MkSealedPattern tw2 pat2 <- rename spat2
        unifyNegWitnesses @unifier tw1 tw2 $ \twr uconv -> do
            unifierSolve @unifier $
                fmap
                    (\(abt1, abt2) ->
                         MkSealedPattern twr $
                         proc ab -> do
                             pat1 -< abt1 ab
                             pat2 -< abt2 ab)
                    uconv

caseSealedExpression ::
       forall renamer unifier m. UnifierRenamerConstraint unifier renamer m
    => UnifierSealedExpression unifier
    -> [(UnifierSealedPattern unifier, UnifierSealedExpression unifier)]
    -> m (UnifierSealedExpression unifier)
caseSealedExpression sbexpr rawcases =
    runRenamer @renamer $
    withTransConstraintTM @Monad $ do
        patrs <-
            for rawcases $ \(rawpat, rawexpr) -> do
                pat <- rename rawpat
                expr <- rename rawexpr
                patternAbstractSealedExpression @unifier pat expr
        MkPatternResult rvwt rtwt ruexpr <- joinPatternResults patrs
        MkSealedExpression btwt bexpr <- rename sbexpr
        uconv <- unifyPosNegWitnesses @unifier btwt rvwt
        unifierSolve $
            (\conv rexpr -> MkSealedExpression rtwt $ (\t1a t -> runIdentity $ t1a $ conv t) <$> rexpr <*> bexpr) <$>
            uconv <*>
            unifierExpression ruexpr

caseAbstractSealedExpression ::
       forall renamer unifier m. UnifierRenamerConstraint unifier renamer m
    => FunctionPosWitness (RenamerNegWitness renamer) (RenamerPosWitness renamer)
    -> [(UnifierSealedPattern unifier, UnifierSealedExpression unifier)]
    -> m (UnifierSealedExpression unifier)
caseAbstractSealedExpression absw rawcases =
    runRenamer @renamer $
    withTransConstraintTM @Monad $ do
        patrs <-
            for rawcases $ \(rawpat, rawexpr) -> do
                pat <- rename rawpat
                expr <- rename rawexpr
                patternAbstractSealedExpression @unifier pat expr
        MkPatternResult rvwt rtwt ruexpr <- joinPatternResults patrs
        absw rvwt rtwt $ \rfwt fconv ->
            unifierSolve $
            (\rexpr -> MkSealedExpression rfwt $ fmap (\tia -> fconv $ \t -> runIdentity $ tia t) rexpr) <$>
            unifierExpression ruexpr
