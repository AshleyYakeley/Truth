{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Abstract
    ( AbstractTypeSystem(..)
    , TSOpenPattern
    , TSMatch
    , TSSealedExpressionPattern
    , TSExpressionPatternConstructor
    , unifierSubstituteSimplifyFinalRename
    , unifierSolve
    , abstractExpression
    , FunctionWitness
    , UnifierFunctionPosWitness
    , UnifierFunctionNegWitness
    , abstractSealedExpression
    , applySealedExpression
    , letSealedExpression
    , bothSealedPattern
    , applyPatternConstructor
    , tsPartialExpressionSumList
    , tsLambdaPatternMatch
    , tsExpressionPatternMatch
    , tsMatchGate
    , tsMatchBindings
    ) where

import Data.Shim
import Language.Expression.Common.Error
import Language.Expression.Common.Expression
import Language.Expression.Common.Named
import Language.Expression.Common.Partial
import Language.Expression.Common.Pattern
import Language.Expression.Common.Rename
import Language.Expression.Common.Sealed
import Language.Expression.Common.Simplifier
import Language.Expression.Common.SolverExpression
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
    bottomShimWit :: Some (TSPosShimWit ts)

type TSOpenPattern :: Type -> Type -> Type -> Type
type TSOpenPattern ts = NamedPattern (TSVarID ts) (TSPosShimWit ts)

type TSMatch ts = Match (TSVarID ts) (TSPosShimWit ts) (TSNegShimWit ts)

type TSSealedExpressionPattern ts = SealedExpressionPattern (TSVarID ts) (TSPosShimWit ts) (TSNegShimWit ts)

type TSExpressionPatternConstructor ts = ExpressionPatternConstructor (TSVarID ts) (TSPosShimWit ts) (TSNegShimWit ts)

unifierSubstituteSimplifyFinalRename ::
       forall ts a. (AbstractTypeSystem ts, TSMappable ts a)
    => UnifierSubstitutions ts
    -> EndoM (TSOuter ts) a
unifierSubstituteSimplifyFinalRename subs = mconcat [unifierSubstitute @ts subs, simplify @ts, finalRename @ts]

unifierSolve ::
       forall ts a b. (AbstractTypeSystem ts, TSMappable ts b)
    => UnifierExpression ts a
    -> (TSOpenExpression ts a -> TSOuter ts b)
    -> TSOuter ts b
unifierSolve uexpr mab = do
    (expr, subs) <- solveUnifierExpression @ts uexpr
    b <- mab expr
    unEndoM (unifierSubstituteSimplifyFinalRename @ts subs) b

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
    forall t. MkAbstractResult (TSNegWitness ts t)
                               (UnifierExpression ts (t -> a))

abstractResult ::
       forall ts a. AbstractTypeSystem ts
    => TSVarID ts
    -> TSOpenExpression ts a
    -> TSOuter ts (AbstractResult ts a)
abstractResult name expr = do
    MkNewVar vwt0 _ <- renameNewFreeVar @ts
    abstractNamedExpressionUnifier @ts name vwt0 expr $ \(MkShimWit vwt (MkPolarMap (MkComposeShim uconv))) expr' ->
        return $
        MkAbstractResult vwt $
        liftA2 (\tb stt a -> tb $ meet2 $ shimToFunction stt a) (solverExpressionLiftValue expr') uconv

abstractExpression ::
       forall ts a b. AbstractTypeSystem ts
    => TSVarID ts
    -> TSPosShimWit ts a
    -> TSOpenExpression ts b
    -> TSOuter ts (UnifierExpression ts (a -> b))
abstractExpression name twt expr = do
    MkAbstractResult vwt uexpr <- abstractResult @ts name expr
    uabsconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts twt) (mkShimWit vwt)
    return $ liftA2 (\tb sat -> tb . shimToFunction sat) uexpr uabsconv

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
    runRenamer @ts [] [] $
    withTransConstraintTM @Monad $ do
        MkSealedExpression twt expr <- renameSimple @ts sexpr
        MkAbstractResult vwt uexpr' <- abstractResult @ts name expr
        unifierSolve @ts uexpr' $ \rexpr -> return $ MkSealedExpression (absw (mkShimWit vwt) twt) rexpr

applySealedExpression ::
       forall ts. (AllConstraint Show (TSPosWitness ts), AllConstraint Show (TSNegWitness ts), AbstractTypeSystem ts)
    => UnifierFunctionNegWitness ts
    -> TSSealedExpression ts
    -> TSSealedExpression ts
    -> TSInner ts (TSSealedExpression ts)
applySealedExpression appw sexprf sexpra =
    runRenamer @ts [] [] $
    withTransConstraintTM @Monad $ do
        MkSealedExpression tf exprf <- renameSimple @ts sexprf
        MkSealedExpression ta expra <- renameSimple @ts sexpra
        MkNewVar vx tx <- renameNewFreeVar @ts
        let vax = appw ta vx
        uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts tf) (uuLiftNegShimWit @ts vax)
        unifierSolve @ts uconv $ \convexpr ->
            return $
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
    runRenamer @ts [] [] $
    withTransConstraintTM @Monad $ do
        MkSealedExpression te expre <- renameSimple @ts sexpre
        MkSealedExpression tb exprb <- renameSimple @ts sexprb
        MkSolverExpression uconv exprf <- abstractExpression @ts name te exprb
        unifierSolve @ts (solverExpressionLiftType uconv) $ \convexpr ->
            return $ MkSealedExpression tb $ exprf <*> convexpr <*> expre

bothSealedPattern ::
       forall ts. AbstractTypeSystem ts
    => TSSealedExpressionPattern ts
    -> TSSealedExpressionPattern ts
    -> TSInner ts (TSSealedExpressionPattern ts)
bothSealedPattern spat1 spat2 =
    runRenamer @ts [] [] $
    withTransConstraintTM @Monad $ do
        MkSealedPattern tw1 pat1 <- renameSimple @ts spat1
        MkSealedPattern tw2 pat2 <- renameSimple @ts spat2
        MkShimWit tr (MkPolarMap uconv) <-
            unifyUUNegShimWit @ts (uuLiftNegExpressionShimWit @ts tw1) (uuLiftNegExpressionShimWit @ts tw2)
        unifierSolve @ts (uuGetShim @ts uconv) $ \convexpr ->
            return $
            MkSealedPattern (MkExpressionWitness (mkShimWit tr) convexpr) $
            proc (MkMeetType (t, shim)) -> do
                let ab = shimToFunction shim t
                pat1 -< meet1 ab
                pat2 -< meet2 ab

applyPatternConstructor ::
       forall ts. AbstractTypeSystem ts
    => TSExpressionPatternConstructor ts
    -> TSSealedExpressionPattern ts
    -> TSInner ts (TSExpressionPatternConstructor ts)
applyPatternConstructor patcon patarg =
    runRenamer @ts [] [] $
    withTransConstraintTM @Monad $ do
        MkPatternConstructor (MkExpressionWitness pcw pcconvexpr) pclt pcpat <- renameSimple @ts patcon
        case pclt of
            NilListType -> lift $ throw PatternTooManyConsArgsError
            ConsListType pca pcla -> do
                MkSealedPattern (MkExpressionWitness ta tconvexpr) pata <- renameSimple @ts patarg
                uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts pca) (uuLiftNegShimWit @ts ta)
                unifierSolve @ts uconv $ \convexpr ->
                    return $
                    MkPatternConstructor (MkExpressionWitness pcw $ (,,) <$> pcconvexpr <*> tconvexpr <*> convexpr) pcla $
                    proc (MkMeetType (t, (r, r1, conv))) -> do
                        (a, l) <- pcpat -< MkMeetType (t, r)
                        pata -< MkMeetType (shimToFunction conv a, r1)
                        returnA -< l

partialExpressionZero ::
       forall ts. AbstractTypeSystem ts
    => TSSealedPartialExpression ts
partialExpressionZero =
    case bottomShimWit @ts of
        MkSome twt -> neverSealedPartialExpression twt

partialExpressionSum ::
       forall ts. AbstractTypeSystem ts
    => TSSealedPartialExpression ts
    -> TSSealedPartialExpression ts
    -> TSOuter ts (TSSealedPartialExpression ts)
partialExpressionSum (MkSealedExpression (MkPartialWit purity1 etype1) expr1) (MkSealedExpression (MkPartialWit purity2 etype2) expr2) =
    purityTypeSum purity1 purity2 $ \purity12 pconv -> do
        MkShimWit etype12 (MkPolarMap (MkComposeShim uconvexpr)) <-
            unifyUUPosShimWit @ts (uuLiftPosShimWit @ts etype1) (uuLiftPosShimWit @ts etype2)
        unifierSolve @ts uconvexpr $ \convexpr -> let
            sumExpr jshim f1 f2 =
                purityIs @Functor purity1 $
                purityIs @Functor purity2 $ let
                    jf = shimToFunction jshim
                    j1 = fmap (jf . LeftJoinType) f1
                    j2 = fmap (jf . RightJoinType) f2
                    in pconv j1 j2
            in return $
               MkSealedExpression (MkPartialWit purity12 $ mkShimWit etype12) $ sumExpr <$> convexpr <*> expr1 <*> expr2

partialExpressionSumList ::
       forall ts. AbstractTypeSystem ts
    => [TSSealedPartialExpression ts]
    -> TSOuter ts (TSSealedPartialExpression ts)
partialExpressionSumList [] = return $ partialExpressionZero @ts
partialExpressionSumList [e] = return e
partialExpressionSumList (e1:ee) = do
    e2 <- partialExpressionSumList @ts ee
    partialExpressionSum @ts e1 e2

tsPartialExpressionSumList ::
       forall ts. AbstractTypeSystem ts
    => [TSSealedPartialExpression ts]
    -> TSInner ts (TSSealedPartialExpression ts)
tsPartialExpressionSumList [] = return $ partialExpressionZero @ts
tsPartialExpressionSumList [e] = return e
tsPartialExpressionSumList rawee =
    runRenamer @ts [] [] $ do
        ee <- for rawee $ renameSimple @ts
        partialExpressionSumList @ts ee

tsLambdaPatternMatch :: forall ts. TSVarID ts -> TSSealedExpressionPattern ts -> TSMatch ts
tsLambdaPatternMatch varid (MkSealedPattern (MkExpressionWitness ptw pexpr) opat) =
    MkSealedPattern (OpenExpression (MkNameWitness varid ptw) $ fmap (\r t -> BothMeetType t r) pexpr) opat

tsExpressionPatternMatch ::
       forall ts. AbstractTypeSystem ts
    => TSSealedExpression ts
    -> TSSealedExpressionPattern ts
    -> TSInner ts (TSMatch ts)
tsExpressionPatternMatch rawexpr rawpat =
    runRenamer @ts [] [] $ do
        MkSealedExpression etw expr <- renameSimple @ts rawexpr
        MkSealedPattern (MkExpressionWitness ptw pexpr) opat <- renameSimple @ts rawpat
        uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts etw) (uuLiftNegShimWit @ts ptw)
        unifierSolve @ts uconv $ \convexpr ->
            return $
            MkSealedPattern ((\conv r t -> BothMeetType (shimToFunction conv t) r) <$> convexpr <*> pexpr <*> expr) opat

tsMatchGate ::
       forall ts. AbstractTypeSystem ts
    => TSMatch ts
    -> TSSealedPartialExpression ts
    -> TSInner ts (TSSealedPartialExpression ts)
tsMatchGate rawmatch rawexpr =
    runRenamer @ts [] [] $ do
        MkSealedPattern pexpr (MkPattern _ (MkPurityFunction ppurity (Kleisli pf))) <- renameSimple @ts rawmatch
        MkSealedExpression (MkPartialWit epurity etype) expr <- renameSimple @ts rawexpr
        return $
            purityTypeProduct ppurity epurity $ \purity pconv econv ->
                purityIs @Monad purity $
                MkSealedExpression (MkPartialWit purity etype) $ liftA2 (\t ft -> (pconv $ pf t) >> econv ft) pexpr expr

tsMatchBindings :: forall ts. TSMatch ts -> [(TSVarID ts, TSSealedExpression ts)]
tsMatchBindings (MkSealedPattern pexpr (MkPattern ww (MkPurityFunction purity (Kleisli pf)))) =
    purityIs @Functor purity $ let
        mkBinding ::
               SomeFor ((->) _) (NameWitness (TSVarID ts) (TSPosShimWit ts)) -> (TSVarID ts, TSSealedExpression ts)
        mkBinding (MkSomeFor (MkNameWitness wvar wtype) f) =
            ( wvar
            , partialToSealedExpression $
              MkSealedExpression (MkPartialWit purity wtype) $ fmap (fmap (\(tt, ()) -> f tt) . pf) pexpr)
        in fmap mkBinding ww
