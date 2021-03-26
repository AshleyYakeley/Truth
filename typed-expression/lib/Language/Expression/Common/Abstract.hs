{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Abstract where

import Data.Shim
import Language.Expression.Common.Error
import Language.Expression.Common.Expression
import Language.Expression.Common.Named
import Language.Expression.Common.Pattern
import Language.Expression.Common.Rename
import Language.Expression.Common.Sealed
import Language.Expression.Common.Simplifier
import Language.Expression.Common.TypeSystem
import Language.Expression.Common.Unifier
import Shapes

class ( RenameTypeSystem ts
      , UnifyTypeSystem ts
      , SimplifyTypeSystem ts
      , Monad (TSInner ts)
      , Ord (TSName ts)
      , TSOuter ts ~ RenamerT ts (TSInner ts)
      ) => AbstractTypeSystem ts where
    type TSInner ts :: Type -> Type

abstractNamedExpressionUnifier ::
       forall ts t a r. UnifyTypeSystem ts
    => TSName ts
    -> TSNegShimWit ts t
    -> TSOpenExpression ts a
    -> (forall tu. UUNegShimWit ts (MeetType t tu) -> TSOpenExpression ts (tu -> a) -> TSOuter ts r)
    -> TSOuter ts r
abstractNamedExpressionUnifier _name vwt (ClosedExpression a) cont =
    cont (uuLiftNegShimWit $ mapShimWit iPolarL1 vwt) $ pure $ \_ -> a
abstractNamedExpressionUnifier name vwt (OpenExpression (MkNameWitness name' vwt') expr) cont
    | name == name' =
        abstractNamedExpressionUnifier @ts name vwt expr $ \vwt1 expr' -> do
            vwtt <- unifyUUNegShimWit @ts vwt1 (uuLiftNegShimWit vwt')
            cont (mapNegShimWit iMeetSwapL vwtt) $ fmap (\tta ~(BothMeetType ta tb) -> tta ta tb) expr'
abstractNamedExpressionUnifier name vwt (OpenExpression (MkNameWitness name' vwt') expr) cont =
    abstractNamedExpressionUnifier @ts name vwt expr $ \vwt1 expr' ->
        cont vwt1 $ OpenExpression (MkNameWitness name' vwt') $ fmap (\vva v1 v2 -> vva v2 v1) expr'

data AbstractResult ts a =
    forall t. MkAbstractResult (UUNegShimWit ts t)
                               (TSOpenExpression ts (t -> a))

instance Functor (AbstractResult ts) where
    fmap ab (MkAbstractResult vwt uexpr) = MkAbstractResult vwt $ fmap (fmap ab) uexpr

joinAbstractResult ::
       forall ts a b c. UnifyTypeSystem ts
    => (a -> b -> c)
    -> AbstractResult ts a
    -> AbstractResult ts b
    -> TSOuter ts (AbstractResult ts c)
joinAbstractResult abc (MkAbstractResult wa expra) (MkAbstractResult wb exprb) = do
    wab <- unifyUUNegShimWit @ts wa wb
    return $ MkAbstractResult wab $ (\aa bb (BothMeetType a b) -> abc (aa a) (bb b)) <$> expra <*> exprb

abstractNamedExpression ::
       forall ts a. AbstractTypeSystem ts
    => TSName ts
    -> TSOpenExpression ts a
    -> TSOuter ts (AbstractResult ts a)
abstractNamedExpression name expr = do
    MkNewVar vwt0 _ <- renameNewFreeVar @ts
    abstractNamedExpressionUnifier @ts name vwt0 expr $ \vwt expr' ->
        return $ MkAbstractResult (mapShimWit polar2 vwt) expr'

patternAbstractUnifyExpression ::
       forall ts q a t r. AbstractTypeSystem ts
    => TSOpenPattern ts q t
    -> TSOpenExpression ts a
    -> (forall pa. UUShim ts pa a -> TSOpenExpression ts (q -> Maybe (pa, t)) -> TSOuter ts r)
    -> TSOuter ts r
patternAbstractUnifyExpression (ClosedPattern qmt) expr cont = cont id $ fmap (\a q -> fmap (\t -> (a, t)) $ qmt q) expr
patternAbstractUnifyExpression (OpenPattern (MkNameWitness name vwt) pat) expr cont = do
    MkAbstractResult absvwt absexpr <- abstractNamedExpression @ts name expr
    uabsconv <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit vwt) absvwt
    patternAbstractUnifyExpression @ts pat absexpr $ \uuconv rexpr ->
        cont (applf uuconv uabsconv) $ fmap (fmap (fmap $ \(pa, (t1, t)) -> (BothMeetType pa t1, t))) rexpr

data PatternResult ts f =
    forall a. MkPatternResult (UUPosShimWit ts a)
                              (AbstractResult ts (f a))

joinPatternResult ::
       forall ts. UnifyTypeSystem ts
    => PatternResult ts Maybe
    -> PatternResult ts Identity
    -> TSOuter ts (PatternResult ts Identity)
joinPatternResult (MkPatternResult wa expra) (MkPatternResult wb exprb) = do
    wab <- unifyUUPosShimWit @ts wa wb
    let
        pickpat (Just a) _ = Identity $ join1 a -- pattern matched
        pickpat Nothing (Identity b) = Identity $ join2 b -- pattern didn't match, try the rest of the patterns
    exprab <- joinAbstractResult pickpat expra exprb
    return $ MkPatternResult wab exprab

joinPatternResults ::
       forall ts. AbstractTypeSystem ts
    => [PatternResult ts Maybe]
    -> TSOuter ts (PatternResult ts Identity)
joinPatternResults [] = do
    MkNewVar tt _ <- renameNewFreeVar @ts
    MkNewVar _ ta <- renameNewFreeVar @ts
    return $
        MkPatternResult (uuLiftPosShimWit ta) $
        MkAbstractResult (uuLiftNegShimWit tt) $ pure $ \_ -> error "missing case"
joinPatternResults (p:pp) = do
    c <- joinPatternResults pp
    joinPatternResult p c

patternAbstractSealedExpression ::
       forall ts. AbstractTypeSystem ts
    => TSSealedPattern ts
    -> TSSealedExpression ts
    -> TSOuter ts (PatternResult ts Maybe)
patternAbstractSealedExpression (MkSealedPattern vwt pat) (MkSealedExpression twt expr) =
    patternAbstractUnifyExpression @ts pat expr $ \uconv uexpr' ->
        return $
        MkPatternResult (mapPosShimWit (applf cid uconv) $ uuLiftPosShimWit twt) $
        (fmap $ fmap $ \(pa, ()) -> BothMeetType id pa) $ MkAbstractResult (uuLiftNegShimWit vwt) uexpr'

type FunctionWitness vw tw = forall a b. vw a -> tw b -> tw (a -> b)

type UnifierFunctionPosWitness ts = FunctionWitness (TSNegShimWit ts) (TSPosShimWit ts)

type UnifierFunctionNegWitness ts = FunctionWitness (TSPosShimWit ts) (TSNegShimWit ts)

abstractSealedExpression ::
       forall ts. AbstractTypeSystem ts
    => UnifierFunctionPosWitness ts
    -> TSName ts
    -> TSSealedExpression ts
    -> TSInner ts (TSSealedExpression ts)
abstractSealedExpression absw name sexpr =
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        MkSealedExpression twt expr <- rename @ts FreeName sexpr
        MkAbstractResult uvwt expr' <- abstractNamedExpression @ts name expr
        unifierSolve @ts $ do
            vwt <- uuGetNegShimWit uvwt
            pure $ MkSealedExpression (absw vwt twt) expr'

applySealedExpression ::
       forall ts. AbstractTypeSystem ts
    => UnifierFunctionNegWitness ts
    -> TSSealedExpression ts
    -> TSSealedExpression ts
    -> TSInner ts (TSSealedExpression ts)
applySealedExpression appw sexprf sexpra =
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        MkSealedExpression tf exprf <- rename @ts FreeName sexprf
        MkSealedExpression ta expra <- rename @ts FreeName sexpra
        MkNewVar vx tx <- renameNewFreeVar @ts
        let vax = appw ta vx
        uconv <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit tf) (uuLiftNegShimWit vax)
        unifierSolve @ts $ do
            conv <- uuGetShim uconv
            pure $
                shimExtractFunction conv $ \fconv tconv ->
                    MkSealedExpression (mapPosShimWit tconv tx) $ shimToFunction fconv <$> exprf <*> expra

-- | not recursive
letSealedExpression ::
       forall ts. AbstractTypeSystem ts
    => TSName ts
    -> TSSealedExpression ts
    -> TSSealedExpression ts
    -> TSInner ts (TSSealedExpression ts)
letSealedExpression name sexpre sexprb =
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        MkSealedExpression te expre <- rename @ts FreeName sexpre
        MkSealedExpression tb exprb <- rename @ts FreeName sexprb
        MkAbstractResult uvt exprf <- abstractNamedExpression @ts name exprb
        uconv <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit te) uvt
        unifierSolve @ts $ do
            conv <- uuGetShim uconv
            pure $ MkSealedExpression tb $ (\f -> f . shimToFunction conv) <$> exprf <*> expre

bothSealedPattern ::
       forall ts. AbstractTypeSystem ts
    => TSSealedPattern ts
    -> TSSealedPattern ts
    -> TSInner ts (TSSealedPattern ts)
bothSealedPattern spat1 spat2 =
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        MkSealedPattern tw1 pat1 <- rename @ts FreeName spat1
        MkSealedPattern tw2 pat2 <- rename @ts FreeName spat2
        utwr <- unifyUUNegShimWit @ts (uuLiftNegShimWit tw1) (uuLiftNegShimWit tw2)
        unifierSolve @ts $ do
            twr <- uuGetNegShimWit utwr
            pure $
                MkSealedPattern twr $
                proc ab -> do
                    pat1 -< meet1 ab
                    pat2 -< meet2 ab

caseSealedExpression ::
       forall ts. AbstractTypeSystem ts
    => TSSealedExpression ts
    -> [(TSSealedPattern ts, TSSealedExpression ts)]
    -> TSInner ts (TSSealedExpression ts)
caseSealedExpression sbexpr rawcases =
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        patrs <-
            for rawcases $ \(rawpat, rawexpr) -> do
                pat <- rename @ts FreeName rawpat
                expr <- rename @ts FreeName rawexpr
                patternAbstractSealedExpression @ts pat expr
        MkPatternResult urtwt (MkAbstractResult rvwt rexpr) <- joinPatternResults patrs
        MkSealedExpression btwt bexpr <- rename @ts FreeName sbexpr
        uconv <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit btwt) rvwt
        unifierSolve @ts $ do
            rtwt <- uuGetPosShimWit urtwt
            conv <- uuGetShim uconv
            pure $ MkSealedExpression rtwt $ (\t1a t -> runIdentity $ t1a $ shimToFunction conv t) <$> rexpr <*> bexpr

caseAbstractSealedExpression ::
       forall ts. AbstractTypeSystem ts
    => UnifierFunctionPosWitness ts
    -> [(TSSealedPattern ts, TSSealedExpression ts)]
    -> TSInner ts (TSSealedExpression ts)
caseAbstractSealedExpression absw rawcases =
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        patrs <-
            for rawcases $ \(rawpat, rawexpr) -> do
                pat <- rename @ts FreeName rawpat
                expr <- rename @ts FreeName rawexpr
                patternAbstractSealedExpression @ts pat expr
        MkPatternResult urtwt (MkAbstractResult urvwt rexpr) <- joinPatternResults patrs
        unifierSolve @ts $ do
            rtwt <- uuGetPosShimWit urtwt
            rvwt <- uuGetNegShimWit urvwt
            pure $ MkSealedExpression (absw rvwt rtwt) $ fmap (\t1a t -> runIdentity $ t1a t) rexpr

applyPatternConstructor ::
       forall ts. (AbstractTypeSystem ts, MonadThrow ExpressionError (TSInner ts))
    => TSPatternConstructor ts
    -> TSSealedPattern ts
    -> TSInner ts (TSPatternConstructor ts)
applyPatternConstructor patcon patarg =
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        MkPatternConstructor pct pclt pcpat <- rename @ts FreeName patcon
        case pclt of
            NilListType -> lift $ throw PatternTooManyConsArgsError
            ConsListType pca pcla -> do
                MkSealedPattern ta pata <- rename @ts FreeName patarg
                uconv <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit pca) (uuLiftNegShimWit ta)
                unifierSolve @ts $ do
                    conv <- uuGetShim uconv
                    pure $
                        MkPatternConstructor pct pcla $
                        proc t -> do
                            (a, l) <- pcpat -< t
                            pata -< shimToFunction conv a
                            returnA -< l
