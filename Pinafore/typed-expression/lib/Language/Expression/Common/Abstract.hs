{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Abstract
    ( AbstractTypeSystem(..)
    , AbstractResult(..)
    , abstractNamedExpression
    , FunctionWitness
    , UnifierFunctionPosWitness
    , UnifierFunctionNegWitness
    , abstractSealedExpression
    , applySealedExpression
    , letSealedExpression
    , bothSealedPattern
    , caseSealedExpression
    , caseAbstractSealedExpression
    , applyPatternConstructor
    ) where

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
      , MonadThrow ExpressionError (TSInner ts)
      , Ord (TSVarID ts)
      , TSOuter ts ~ RenamerT ts (TSInner ts)
      ) => AbstractTypeSystem ts where
    type TSInner ts :: Type -> Type

abstractNamedExpressionUnifier ::
       forall ts t a r. UnifyTypeSystem ts
    => TSVarID ts
    -> TSNegShimWit ts t
    -> TSOpenExpression ts a
    -> (forall tu. UUNegShimWit ts (MeetType t tu) -> TSOpenExpression ts (tu -> a) -> TSOuter ts r)
    -> TSOuter ts r
abstractNamedExpressionUnifier _name vwt (ClosedExpression a) cont =
    cont (uuLiftNegShimWit @ts $ mapPolarShimWit iPolarL1 vwt) $ pure $ \_ -> a
abstractNamedExpressionUnifier name vwt (OpenExpression (MkNameWitness name' vwt') expr) cont
    | name == name' =
        abstractNamedExpressionUnifier @ts name vwt expr $ \vwt1 expr' -> do
            vwtt <- unifyUUNegShimWit @ts vwt1 (uuLiftNegShimWit @ts vwt')
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
    => TSVarID ts
    -> TSOpenExpression ts a
    -> TSOuter ts (AbstractResult ts a)
abstractNamedExpression name expr = do
    MkNewVar vwt0 _ <- renameNewFreeVar @ts
    abstractNamedExpressionUnifier @ts name vwt0 expr $ \vwt expr' ->
        return $ MkAbstractResult (mapPolarShimWit polar2 vwt) expr'

patternAbstractUnifyExpression ::
       forall ts q a t r. AbstractTypeSystem ts
    => TSOpenPattern ts q t
    -> TSOpenExpression ts a
    -> (forall pa. UUShim ts pa a -> TSOpenExpression ts (q -> Maybe (pa, t)) -> TSOuter ts r)
    -> TSOuter ts r
patternAbstractUnifyExpression (ClosedPattern qmt) expr cont = cont id $ fmap (\a q -> fmap (\t -> (a, t)) $ qmt q) expr
patternAbstractUnifyExpression (OpenPattern (MkNameWitness name vwt) pat) expr cont = do
    MkAbstractResult absvwt absexpr <- abstractNamedExpression @ts name expr
    uabsconv <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit @ts vwt) absvwt
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
        MkPatternResult (uuLiftPosShimWit @ts ta) $
        MkAbstractResult (uuLiftNegShimWit @ts tt) $ pure $ \_ -> error "missing case"
joinPatternResults (p:pp) = do
    c <- joinPatternResults pp
    joinPatternResult p c

patternAbstractSealedExpression ::
       forall ts. AbstractTypeSystem ts
    => TSSealedExpressionPattern ts
    -> TSSealedExpression ts
    -> TSOuter ts (PatternResult ts Maybe)
patternAbstractSealedExpression (MkSealedPattern vwt pat) (MkSealedExpression twt expr) =
    patternAbstractUnifyExpression @ts pat expr $ \uconv uexpr' ->
        return $
        MkPatternResult (mapPosShimWit (applf id uconv) $ uuLiftPosShimWit @ts twt) $
        (fmap $ fmap $ \(pa, ()) -> BothMeetType id pa) $ MkAbstractResult (uuLiftNegExpressionShimWit @ts vwt) uexpr'

type FunctionWitness vw tw = forall a b. vw a -> tw b -> tw (a -> b)

type UnifierFunctionPosWitness ts = FunctionWitness (TSNegShimWit ts) (TSPosShimWit ts)

type UnifierFunctionNegWitness ts = FunctionWitness (TSPosShimWit ts) (TSNegShimWit ts)

abstractSealedExpression ::
       forall ts. AbstractTypeSystem ts
    => UnifierFunctionPosWitness ts
    -> TSVarID ts
    -> TSSealedExpression ts
    -> TSInner ts (TSSealedExpression ts)
abstractSealedExpression absw name sexpr =
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        MkSealedExpression twt expr <- rename @ts FreeName sexpr
        MkAbstractResult (MkShimWit vwt (MkPolarMap uconv)) expr' <- abstractNamedExpression @ts name expr
        (convexpr, subs) <- solveUUShim @ts uconv
        unifierSubstituteAndSimplify @ts subs $
            MkSealedExpression (absw (mkShimWit vwt) twt) $ liftA2 (\f conv -> f . shimToFunction conv) expr' convexpr

applySealedExpression ::
       forall ts. (AllConstraint Show (TSPosWitness ts), AllConstraint Show (TSNegWitness ts), AbstractTypeSystem ts)
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
        uconv <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit @ts tf) (uuLiftNegShimWit @ts vax)
        (convexpr, subs) <- solveUUShim @ts $ uconv
        unifierSubstituteAndSimplify @ts subs $
            case convexpr of
                ClosedExpression conv ->
                    shimExtractFunction conv $ \fconv tconv ->
                        MkSealedExpression (mapPosShimWit tconv tx) $ shimToFunction fconv <$> exprf <*> expra
                _ -> MkSealedExpression tx $ shimToFunction <$> convexpr <*> exprf <*> expra

-- | not recursive
letSealedExpression ::
       forall ts. AbstractTypeSystem ts
    => TSVarID ts
    -> TSSealedExpression ts
    -> TSSealedExpression ts
    -> TSInner ts (TSSealedExpression ts)
letSealedExpression name sexpre sexprb =
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        MkSealedExpression te expre <- rename @ts FreeName sexpre
        MkSealedExpression tb exprb <- rename @ts FreeName sexprb
        MkAbstractResult uvt exprf <- abstractNamedExpression @ts name exprb
        uconv <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit @ts te) uvt
        (convexpr, subs) <- solveUUShim @ts uconv
        unifierSubstituteAndSimplify @ts subs $
            MkSealedExpression tb $ (\f conv e -> f $ shimToFunction conv e) <$> exprf <*> convexpr <*> expre

bothSealedPattern ::
       forall ts. AbstractTypeSystem ts
    => TSSealedExpressionPattern ts
    -> TSSealedExpressionPattern ts
    -> TSInner ts (TSSealedExpressionPattern ts)
bothSealedPattern spat1 spat2 =
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        MkSealedPattern tw1 pat1 <- rename @ts FreeName spat1
        MkSealedPattern tw2 pat2 <- rename @ts FreeName spat2
        MkShimWit tr (MkPolarMap uconv) <-
            unifyUUNegShimWit @ts (uuLiftNegExpressionShimWit @ts tw1) (uuLiftNegExpressionShimWit @ts tw2)
        (convexpr, subs) <- solveUUShim @ts uconv
        unifierSubstituteAndSimplify @ts subs $
            MkSealedPattern (MkExpressionWitness (mkShimWit tr) convexpr) $
            proc (MkMeetType (t, shim)) -> do
                let ab = shimToFunction shim t
                pat1 -< meet1 ab
                pat2 -< meet2 ab

caseSealedExpression ::
       forall ts. AbstractTypeSystem ts
    => TSSealedExpression ts
    -> [(TSSealedExpressionPattern ts, TSSealedExpression ts)]
    -> TSInner ts (TSSealedExpression ts)
caseSealedExpression sbexpr rawcases =
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        patrs <-
            for rawcases $ \(rawpat, rawexpr) -> do
                pat <- rename @ts FreeName rawpat
                expr <- rename @ts FreeName rawexpr
                patternAbstractSealedExpression @ts pat expr
        MkPatternResult (MkShimWit rtt (MkPolarMap tuconv)) (MkAbstractResult rvwt rexpr) <- joinPatternResults patrs
        MkSealedExpression btwt bexpr <- rename @ts FreeName sbexpr
        uconv <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit @ts btwt) rvwt
        (convexpr, subs) <- solveUnifierExpression @ts $ liftA2 (,) (uuGetShim @ts uconv) (uuGetShim @ts tuconv)
        unifierSubstituteAndSimplify @ts subs $
            MkSealedExpression (mkShimWit rtt) $
            (\(conv, tconv) tia t -> shimToFunction tconv $ runIdentity $ tia $ shimToFunction conv t) <$> convexpr <*>
            rexpr <*>
            bexpr

caseAbstractSealedExpression ::
       forall ts. AbstractTypeSystem ts
    => UnifierFunctionPosWitness ts
    -> [(TSSealedExpressionPattern ts, TSSealedExpression ts)]
    -> TSInner ts (TSSealedExpression ts)
caseAbstractSealedExpression absw rawcases =
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        patrs <-
            for rawcases $ \(rawpat, rawexpr) -> do
                pat <- rename @ts FreeName rawpat
                expr <- rename @ts FreeName rawexpr
                patternAbstractSealedExpression @ts pat expr
        MkPatternResult (MkShimWit rtt (MkPolarMap tuconv)) (MkAbstractResult (MkShimWit rvt (MkPolarMap vuconv)) rexpr) <-
            joinPatternResults patrs
        (convexpr, subs) <- solveUnifierExpression @ts $ liftA2 (,) (uuGetShim @ts tuconv) (uuGetShim @ts vuconv)
        unifierSubstituteAndSimplify @ts subs $
            MkSealedExpression (absw (mkShimWit rvt) (mkShimWit rtt)) $
            (\(tconv, vconv) tia t -> shimToFunction tconv $ runIdentity $ tia $ shimToFunction vconv t) <$> convexpr <*>
            rexpr

applyPatternConstructor ::
       forall ts. AbstractTypeSystem ts
    => TSExpressionPatternConstructor ts
    -> TSSealedExpressionPattern ts
    -> TSInner ts (TSExpressionPatternConstructor ts)
applyPatternConstructor patcon patarg =
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        MkPatternConstructor (MkExpressionWitness pcw pcconvexpr) pclt pcpat <- rename @ts FreeName patcon
        case pclt of
            NilListType -> lift $ throw PatternTooManyConsArgsError
            ConsListType pca pcla -> do
                MkSealedPattern (MkExpressionWitness ta tconvexpr) pata <- rename @ts FreeName patarg
                uconv <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit @ts pca) (uuLiftNegShimWit @ts ta)
                (convexpr, subs) <- solveUUShim @ts uconv
                unifierSubstituteAndSimplify @ts subs $
                    MkPatternConstructor (MkExpressionWitness pcw $ (,,) <$> pcconvexpr <*> tconvexpr <*> convexpr) pcla $
                    proc (MkMeetType (t, (r, r1, conv))) -> do
                        (a, l) <- pcpat -< MkMeetType (t, r)
                        pata -< MkMeetType (shimToFunction conv a, r1)
                        returnA -< l
