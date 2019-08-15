{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Abstract where

import Data.Shim.JoinMeet
import Data.Shim.ShimWit
import Language.Expression.Error
import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Pattern
import Language.Expression.Renamer
import Language.Expression.Sealed
import Language.Expression.Unifier
import Shapes

type UnifierRenamerConstraint unifier renamer m
     = ( Monad m
       , Renamer renamer
       , Unifier unifier
       , UnifierShim unifier ~ RenamerShim renamer
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       , UnifierMonad unifier ~ renamer m)

abstractNamedExpressionUnifier ::
       forall unifier t a r. Unifier unifier
    => UnifierName unifier
    -> UnifierNegShimWit unifier t
    -> UnifierOpenExpression unifier a
    -> (forall tu.
                UUNegShimWit unifier (MeetType t tu) -> UnifierOpenExpression unifier (tu -> a) -> UnifierMonad unifier r)
    -> UnifierMonad unifier r
abstractNamedExpressionUnifier _name vwt (ClosedExpression a) cont =
    cont (uuLiftNegShimWit $ mapShimWit (meetf cid termf) vwt) $ pure $ \_ -> a
abstractNamedExpressionUnifier name vwt (OpenExpression (MkNameWitness name' vwt') expr) cont
    | name == name' =
        abstractNamedExpressionUnifier @unifier name vwt expr $ \vwt1 expr' -> do
            vwtt <- unifyUUNegShimWit @unifier vwt1 (uuLiftNegShimWit vwt')
            cont (mapShimWit swapMeetRight vwtt) $ fmap (\tta (BothMeetType ta tb) -> tta ta tb) expr'
abstractNamedExpressionUnifier name vwt (OpenExpression (MkNameWitness name' vwt') expr) cont =
    abstractNamedExpressionUnifier @unifier name vwt expr $ \vwt1 expr' ->
        cont vwt1 $ OpenExpression (MkNameWitness name' vwt') $ fmap (\vva v1 v2 -> vva v2 v1) expr'

data AbstractResult unifier a =
    forall t. MkAbstractResult (UUNegShimWit unifier t)
                               (UnifierOpenExpression unifier (t -> a))

instance Functor (AbstractResult unifier) where
    fmap ab (MkAbstractResult vwt uexpr) = MkAbstractResult vwt $ fmap (fmap ab) uexpr

joinAbstractResult ::
       forall unifier a b c. Unifier unifier
    => (a -> b -> c)
    -> AbstractResult unifier a
    -> AbstractResult unifier b
    -> UnifierMonad unifier (AbstractResult unifier c)
joinAbstractResult abc (MkAbstractResult wa expra) (MkAbstractResult wb exprb) = do
    wab <- unifyUUNegShimWit @unifier wa wb
    return $ MkAbstractResult wab $ (\aa bb (BothMeetType a b) -> abc (aa a) (bb b)) <$> expra <*> exprb

abstractNamedExpression ::
       forall unifier renamer m a. UnifierRenamerConstraint unifier renamer m
    => UnifierName unifier
    -> UnifierOpenExpression unifier a
    -> UnifierMonad unifier (AbstractResult unifier a)
abstractNamedExpression name expr = do
    MkNewVar vwt0 _ <- renameNewVar
    abstractNamedExpressionUnifier @unifier name vwt0 expr $ \vwt expr' ->
        return $ MkAbstractResult (mapShimWit meet2 vwt) expr'

patternAbstractUnifyExpression ::
       forall unifier renamer m q a t r. UnifierRenamerConstraint unifier renamer m
    => UnifierOpenPattern unifier q t
    -> UnifierOpenExpression unifier a
    -> (forall pa. UUShim unifier pa a -> UnifierOpenExpression unifier (q -> Maybe (pa, t)) -> UnifierMonad unifier r)
    -> UnifierMonad unifier r
patternAbstractUnifyExpression (ClosedPattern qmt) expr cont = cont id $ fmap (\a q -> fmap (\t -> (a, t)) $ qmt q) expr
patternAbstractUnifyExpression (OpenPattern (MkNameWitness name vwt) pat) expr cont = do
    MkAbstractResult absvwt absexpr <- abstractNamedExpression @unifier name expr
    uabsconv <- unifyUUPosNegShimWit @unifier (uuLiftPosShimWit vwt) absvwt
    patternAbstractUnifyExpression @unifier pat absexpr $ \uuconv rexpr ->
        cont (applf uuconv uabsconv) $ fmap (fmap (fmap $ \(pa, (t1, t)) -> (BothMeetType pa t1, t))) rexpr

data PatternResult unifier f =
    forall a. MkPatternResult (UUPosShimWit unifier a)
                              (AbstractResult unifier (f a))

joinPatternResult ::
       forall unifier. Unifier unifier
    => PatternResult unifier Maybe
    -> PatternResult unifier Identity
    -> UnifierMonad unifier (PatternResult unifier Identity)
joinPatternResult (MkPatternResult wa expra) (MkPatternResult wb exprb) = do
    wab <- unifyUUPosShimWit @unifier wa wb
    let
        pickpat (Just a) _ = Identity $ join1 a -- pattern matched
        pickpat Nothing (Identity b) = Identity $ join2 b -- pattern didn't match, try the rest of the patterns
    exprab <- joinAbstractResult pickpat expra exprb
    return $ MkPatternResult wab exprab

joinPatternResults ::
       UnifierRenamerConstraint unifier renamer m
    => [PatternResult unifier Maybe]
    -> UnifierMonad unifier (PatternResult unifier Identity)
joinPatternResults [] = do
    MkNewVar tt _ <- renameNewVar
    MkNewVar _ ta <- renameNewVar
    return $
        MkPatternResult (uuLiftPosShimWit ta) $
        MkAbstractResult (uuLiftNegShimWit tt) $ pure $ \_ -> error "missing case"
joinPatternResults (p:pp) = do
    c <- joinPatternResults pp
    joinPatternResult p c

patternAbstractSealedExpression ::
       forall unifier renamer m. UnifierRenamerConstraint unifier renamer m
    => UnifierSealedPattern unifier
    -> UnifierSealedExpression unifier
    -> UnifierMonad unifier (PatternResult unifier Maybe)
patternAbstractSealedExpression (MkSealedPattern vwt pat) (MkSealedExpression twt expr) =
    patternAbstractUnifyExpression @unifier pat expr $ \uconv uexpr' ->
        return $
        MkPatternResult (mapShimWit (applf cid uconv) $ uuLiftPosShimWit twt) $
        (fmap $ fmap $ \(pa, ()) -> BothMeetType id pa) $ MkAbstractResult (uuLiftNegShimWit vwt) uexpr'

type FunctionWitness vw tw = forall a b. vw a -> tw b -> tw (a -> b)

type UnifierFunctionPosWitness unifier = FunctionWitness (UnifierNegShimWit unifier) (UnifierPosShimWit unifier)

type UnifierFunctionNegWitness unifier = FunctionWitness (UnifierPosShimWit unifier) (UnifierNegShimWit unifier)

abstractSealedExpression ::
       forall renamer unifier m. UnifierRenamerConstraint unifier renamer m
    => UnifierFunctionPosWitness unifier
    -> UnifierName unifier
    -> UnifierSealedExpression unifier
    -> m (UnifierSealedExpression unifier)
abstractSealedExpression absw name sexpr =
    runRenamer @renamer $
    withTransConstraintTM @Monad $ do
        MkSealedExpression twt expr <- rename sexpr
        MkAbstractResult uvwt expr' <- abstractNamedExpression @unifier name expr
        unifierSolve @unifier $ do
            vwt <- uuGetNegShimWit uvwt
            pure $ MkSealedExpression (absw vwt twt) expr'

applySealedExpression ::
       forall renamer unifier m. UnifierRenamerConstraint unifier renamer m
    => UnifierFunctionNegWitness unifier
    -> UnifierSealedExpression unifier
    -> UnifierSealedExpression unifier
    -> m (UnifierSealedExpression unifier)
applySealedExpression appw sexprf sexpra =
    runRenamer @renamer $
    withTransConstraintTM @Monad $ do
        MkSealedExpression tf exprf <- rename sexprf
        MkSealedExpression ta expra <- rename sexpra
        MkNewVar vx tx <- renameNewVar
        let vax = appw ta vx
        uconv <- unifyUUPosNegShimWit @unifier (uuLiftPosShimWit tf) (uuLiftNegShimWit vax)
        unifierSolve @unifier $ do
            conv <- uuGetShim uconv
            pure $
                shimExtractFunction conv $ \fconv tconv ->
                    MkSealedExpression (mapShimWit tconv tx) $ fromEnhanced fconv <$> exprf <*> expra

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
        MkAbstractResult uvt exprf <- abstractNamedExpression @unifier name exprb
        uconv <- unifyUUPosNegShimWit @unifier (uuLiftPosShimWit te) uvt
        unifierSolve @unifier $ do
            conv <- uuGetShim uconv
            pure $ MkSealedExpression tb $ (\f -> f . fromEnhanced conv) <$> exprf <*> expre

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
        utwr <- unifyUUNegShimWit @unifier (uuLiftNegShimWit tw1) (uuLiftNegShimWit tw2)
        unifierSolve @unifier $ do
            twr <- uuGetNegShimWit utwr
            pure $
                MkSealedPattern twr $
                proc ab -> do
                    pat1 -< meet1 ab
                    pat2 -< meet2 ab

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
        MkPatternResult urtwt (MkAbstractResult rvwt rexpr) <- joinPatternResults patrs
        MkSealedExpression btwt bexpr <- rename sbexpr
        uconv <- unifyUUPosNegShimWit @unifier (uuLiftPosShimWit btwt) rvwt
        unifierSolve $ do
            rtwt <- uuGetPosShimWit urtwt
            conv <- uuGetShim uconv
            pure $ MkSealedExpression rtwt $ (\t1a t -> runIdentity $ t1a $ fromEnhanced conv t) <$> rexpr <*> bexpr

caseAbstractSealedExpression ::
       forall renamer unifier m. UnifierRenamerConstraint unifier renamer m
    => UnifierFunctionPosWitness unifier
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
        MkPatternResult urtwt (MkAbstractResult urvwt rexpr) <- joinPatternResults patrs
        unifierSolve $ do
            rtwt <- uuGetPosShimWit urtwt
            rvwt <- uuGetNegShimWit urvwt
            pure $ MkSealedExpression (absw rvwt rtwt) $ fmap (\t1a t -> runIdentity $ t1a t) rexpr

applyPatternConstructor ::
       forall renamer unifier m. (UnifierRenamerConstraint unifier renamer m, MonadError ExpressionError m)
    => UnifierPatternConstructor unifier
    -> UnifierSealedPattern unifier
    -> m (UnifierPatternConstructor unifier)
applyPatternConstructor patcon patarg =
    runRenamer @renamer $
    withTransConstraintTM @Monad $ do
        MkPatternConstructor pct pclt pcpat <- rename patcon
        case pclt of
            NilListType -> lift $ throwError PatternTooManyConsArgsError
            ConsListType pca pcla -> do
                MkSealedPattern ta pata <- rename patarg
                uconv <- unifyUUPosNegShimWit @unifier (uuLiftPosShimWit pca) (uuLiftNegShimWit ta)
                unifierSolve @unifier $ do
                    conv <- uuGetShim uconv
                    pure $
                        MkPatternConstructor pct pcla $
                        proc t -> do
                            (a, l) <- pcpat -< t
                            pata -< fromEnhanced conv a
                            returnA -< l
