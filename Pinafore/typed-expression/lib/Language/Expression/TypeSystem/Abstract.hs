{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.TypeSystem.Abstract
    ( AbstractTypeSystem (..)
    , TSOpenPattern
    , TSMatch
    , TSSealedPattern
    , TSPatternConstructor
    , simplifyFinalRename
    , unifierSubstituteSimplifyFinalRename
    , unifierSolve
    , abstractOpenExpression
    , FunctionWitness
    , UnifierFunctionPosWitness
    , UnifierFunctionNegWitness
    , substituteSealedExpression
    , abstractSealedExpression
    , polyAbstractSealedFExpression
    , applySealedExpression
    , letSealedExpression
    , bothSealedPattern
    , applyPatternConstructor
    , tsPartialExpressionSumList
    , tsLambdaPatternMatch
    , tsExpressionPatternMatch
    , tsMatchGate
    , tsMatchBindings
    )
where

import Data.Shim
import Shapes

import Language.Expression.Common
import Language.Expression.TypeSystem.Rename
import Language.Expression.TypeSystem.Simplify
import Language.Expression.TypeSystem.SolverExpression
import Language.Expression.TypeSystem.TypeSystem
import Language.Expression.TypeSystem.Unify

class
    ( RenameTypeSystem ts
    , UnifyTypeSystem ts
    , SimplifyTypeSystem ts
    , Monad (TSInner ts)
    , MonadThrow PatternError (TSInner ts)
    , Ord (TSVarID ts)
    , TSOuter ts ~ RenamerT ts (TSInner ts)
    , RecoverShim (TSShim ts)
    ) =>
    AbstractTypeSystem ts
    where
    type TSInner ts :: Type -> Type
    bottomShimWit :: Some (TSPosShimWit ts)
    cleanOpenExpression :: forall a. TSOpenExpression ts a -> TSOuter ts (TSOpenExpression ts a)
    cleanOpenExpression = return

cleanSealedExpression ::
    forall ts.
    AbstractTypeSystem ts =>
    TSSealedExpression ts ->
    TSOuter ts (TSSealedExpression ts)
cleanSealedExpression (MkSealedExpression t oexpr) = do
    oexpr' <- cleanOpenExpression @ts oexpr
    return $ MkSealedExpression t oexpr'

type TSOpenPattern :: Type -> Type -> Type -> Type
type TSOpenPattern ts = NamedFuncPattern (TSVarID ts) (TSPosShimWit ts) (TSNegShimWit ts)

type TSMatch ts = TSOpenPattern ts () ()

type TSSealedPattern ts = NamedSealedPattern (TSVarID ts) (TSPosShimWit ts) (TSNegShimWit ts) ()

type TSPatternConstructor ts = NamedPatternConstructor (TSVarID ts) (TSPosShimWit ts) (TSNegShimWit ts)

finalRenameINTERNAL :: Bool
finalRenameINTERNAL = True

simplifyFinalRename ::
    forall ts a.
    (AbstractTypeSystem ts, TSMappable ts a) =>
    EndoM (TSOuter ts) a
simplifyFinalRename = simplify @ts <> mif finalRenameINTERNAL (finalRenameMappable @ts)

unifierSubstituteSimplifyFinalRename ::
    forall ts a.
    (AbstractTypeSystem ts, TSMappable ts a) =>
    UnifierSubstitutions ts ->
    EndoM (TSOuter ts) a
unifierSubstituteSimplifyFinalRename subs = unifierSubstitute @ts subs <> simplifyFinalRename @ts

unifierSolve ::
    forall ts a b.
    (AbstractTypeSystem ts, TSMappable ts b) =>
    UnifierExpression ts a ->
    (TSOpenExpression ts a -> TSOuter ts b) ->
    TSOuter ts b
unifierSolve (MkSolverExpression ut eta) mab = do
    (texpr, subs) <- solveUnifier @ts ut
    b <- mab $ eta <*> texpr
    unEndoM (unifierSubstituteSimplifyFinalRename @ts subs) b

abstractNamedExpressionUnifier ::
    forall ts t a r.
    UnifyTypeSystem ts =>
    TSVarID ts ->
    TSNegShimWit ts t ->
    TSOpenExpression ts a ->
    (forall tu. UUNegShimWit ts (MeetType t tu) -> TSOpenExpression ts (tu -> a) -> TSOuter ts r) ->
    TSOuter ts r
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

data AbstractResult ts a
    = forall t. MkAbstractResult
        (TSNegWitness ts t)
        (UnifierExpression ts (t -> a))

abstractResult ::
    forall ts a.
    AbstractTypeSystem ts =>
    TSVarID ts ->
    TSOpenExpression ts a ->
    TSOuter ts (AbstractResult ts a)
abstractResult name expr = do
    MkNewVar vwt0 _ <- renameNewFreeVar @ts
    abstractNamedExpressionUnifier @ts name vwt0 expr $ \(MkShimWit vwt (MkPolarShim (MkComposeShim uconv))) expr' ->
        return
            $ MkAbstractResult vwt
            $ liftA2 (\tb stt a -> tb $ meet2 $ shimToFunction stt a) (solverExpressionLiftValue expr') uconv

abstractOpenExpression ::
    forall ts a b.
    AbstractTypeSystem ts =>
    TSVarID ts ->
    TSPosShimWit ts a ->
    TSOpenExpression ts b ->
    TSOuter ts (UnifierExpression ts (a -> b))
abstractOpenExpression name twt expr = do
    MkAbstractResult vwt uexpr <- abstractResult @ts name expr
    uabsconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts twt) (mkShimWit vwt)
    return $ liftA2 (\tb sat -> tb . shimToFunction sat) uexpr uabsconv

polyAbstractExpression ::
    forall ts a b.
    AbstractTypeSystem ts =>
    TSVarID ts ->
    TSPosShimWit ts a ->
    TSOpenExpression ts b ->
    TSOuter ts (UnifierExpression ts (a -> b))
polyAbstractExpression name twt =
    \case
        ClosedExpression b -> return $ pure $ \_ -> b
        OpenExpression (MkNameWitness name' vwt) expr
            | name == name' -> do
                twt' <- renameMappableSimple @ts twt
                uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts twt') (uuLiftNegShimWit @ts vwt)
                uexpr <- polyAbstractExpression @ts name twt expr
                return $ liftA2 (\conv atb a -> atb a $ shimToFunction conv a) uconv uexpr
        OpenExpression nw expr -> do
            uexpr <- polyAbstractExpression @ts name twt expr
            return $ liftA2 (\t atb a -> atb a t) (solverExpressionLiftValue $ varExpression nw) uexpr

polyAbstractSealedFExpression ::
    forall ts p q.
    AbstractTypeSystem ts =>
    TSVarID ts ->
    TSPosShimWit ts p ->
    TSSealedFExpression ts ((->) q) ->
    TSInner ts (TSSealedFExpression ts ((->) (p, q)))
polyAbstractSealedFExpression name nwt fexpr = do
    runRenamer @ts [] []
        $ withTransConstraintTM @Monad
        $ do
            MkSealedFExpression twt expr <- renameMappableSimple @ts fexpr
            uexpr <- polyAbstractExpression @ts name nwt expr
            unifierSolve @ts uexpr $ \expr' -> return $ MkSealedFExpression twt $ fmap (\pqt (p, q) -> pqt p q) expr'

type FunctionWitness vw tw = forall a b. vw a -> tw b -> tw (a -> b)

type UnifierFunctionPosWitness ts = FunctionWitness (TSNegShimWit ts) (TSPosShimWit ts)

type UnifierFunctionNegWitness ts = FunctionWitness (TSPosShimWit ts) (TSNegShimWit ts)

uSubstitute ::
    forall ts t.
    AbstractTypeSystem ts =>
    (TSVarID ts -> Maybe (TSSealedExpression ts)) ->
    TSOpenExpression ts t ->
    TSOuter ts (UnifierExpression ts t)
uSubstitute subst =
    runExpressionM $ \w@(MkNameWitness var vwt) ->
        case subst var of
            Nothing -> return $ solverExpressionLiftValue $ varExpression w
            Just expr -> do
                MkSealedExpression twt oexpr <- renameMappableSimple @ts expr
                uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts twt) (uuLiftNegShimWit @ts vwt)
                return $ liftA2 shimToFunction uconv (solverExpressionLiftValue oexpr)

substituteSealedExpression ::
    forall ts.
    AbstractTypeSystem ts =>
    (TSVarID ts -> Maybe (TSSealedExpression ts)) ->
    TSSealedExpression ts ->
    TSInner ts (TSSealedExpression ts)
substituteSealedExpression subst expr =
    runRenamer @ts [] []
        $ withTransConstraintTM @Monad
        $ do
            MkSealedExpression twt oexpr <- renameMappableSimple @ts expr
            uexpr <- uSubstitute @ts subst oexpr
            unifierSolve @ts uexpr $ \oexpr' -> return $ MkSealedExpression twt oexpr'

abstractSealedExpression ::
    forall ts.
    AbstractTypeSystem ts =>
    UnifierFunctionPosWitness ts ->
    TSVarID ts ->
    TSSealedExpression ts ->
    TSInner ts (TSSealedExpression ts)
abstractSealedExpression absw name sexpr =
    runRenamer @ts [] []
        $ withTransConstraintTM @Monad
        $ do
            MkSealedExpression twt expr <- renameMappableSimple @ts sexpr
            MkAbstractResult vwt uexpr' <- abstractResult @ts name expr
            unifierSolve @ts uexpr' $ \rexpr -> return $ MkSealedExpression (absw (mkShimWit vwt) twt) rexpr

cleanExprINTERNAL :: Bool
cleanExprINTERNAL = True

applySealedExpression ::
    forall ts.
    AbstractTypeSystem ts =>
    UnifierFunctionNegWitness ts ->
    TSSealedExpression ts ->
    TSSealedExpression ts ->
    TSInner ts (TSSealedExpression ts)
applySealedExpression appw sexprf sexpra =
    runRenamer @ts [] []
        $ withTransConstraintTM @Monad
        $ do
            MkSealedExpression tf exprf <- renameMappableSimple @ts sexprf
            MkSealedExpression ta expra <- renameMappableSimple @ts sexpra
            MkNewVar vx tx <- renameNewFreeVar @ts
            let vax = appw ta vx
            uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts tf) (uuLiftNegShimWit @ts vax)
            unifierSolve @ts uconv $ \convexpr -> let
                rexpr =
                    case convexpr of
                        ClosedExpression conv ->
                            shimExtractFunction conv $ \fconv tconv ->
                                MkSealedExpression (mapPosShimWit tconv tx) $ shimToFunction fconv <$> exprf <*> expra
                        _ -> MkSealedExpression tx $ shimToFunction <$> convexpr <*> exprf <*> expra
                in if cleanExprINTERNAL
                    then cleanSealedExpression @ts rexpr
                    else return rexpr

-- | not recursive
letSealedExpression ::
    forall ts.
    AbstractTypeSystem ts =>
    TSVarID ts ->
    TSSealedExpression ts ->
    TSSealedExpression ts ->
    TSInner ts (TSSealedExpression ts)
letSealedExpression name sexpre sexprb =
    runRenamer @ts [] []
        $ withTransConstraintTM @Monad
        $ do
            MkSealedExpression te expre <- renameMappableSimple @ts sexpre
            MkSealedExpression tb exprb <- renameMappableSimple @ts sexprb
            MkSolverExpression uconv exprf <- abstractOpenExpression @ts name te exprb
            unifierSolve @ts (solverExpressionLiftType uconv) $ \convexpr ->
                return $ MkSealedExpression tb $ exprf <*> convexpr <*> expre

bothSealedPattern ::
    forall ts.
    AbstractTypeSystem ts =>
    TSSealedPattern ts ->
    TSSealedPattern ts ->
    TSInner ts (TSSealedPattern ts)
bothSealedPattern spat1 spat2 =
    runRenamer @ts [] []
        $ withTransConstraintTM @Monad
        $ do
            MkSealedPattern tw1 pat1 <- renameMappableSimple @ts spat1
            MkSealedPattern tw2 pat2 <- renameMappableSimple @ts spat2
            MkShimWit tr (MkPolarShim uconv) <- unifyUUNegShimWit @ts (uuLiftNegShimWit @ts tw1) (uuLiftNegShimWit @ts tw2)
            unifierSolve @ts (uuGetShim @ts uconv) $ \convexpr ->
                return
                    $ MkSealedPattern (mkShimWit tr)
                    $ proc t -> do
                        ab <- pureFuncPattern (fmap shimToFunction convexpr) -< t
                        pat1 -< meet1 ab
                        pat2 -< meet2 ab

applyPatternConstructor ::
    forall ts.
    AbstractTypeSystem ts =>
    TSPatternConstructor ts ->
    TSSealedPattern ts ->
    TSInner ts (TSPatternConstructor ts)
applyPatternConstructor patcon patarg =
    runRenamer @ts [] []
        $ withTransConstraintTM @Monad
        $ do
            MkPatternConstructor pclt (MkSealedPattern pcw pcpat) <- renameMappableSimple @ts patcon
            case pclt of
                NilListType -> lift $ throw PatternTooManyConsArgsError
                ConsListType pca pcla -> do
                    MkSealedPattern ta pata <- renameMappableSimple @ts patarg
                    uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts pca) (uuLiftNegShimWit @ts ta)
                    unifierSolve @ts uconv $ \convexpr ->
                        return
                            $ MkPatternConstructor pcla
                            $ MkSealedPattern pcw
                            $ proc t -> do
                                (a, l) <- pcpat -< t
                                t1 <- pureFuncPattern $ fmap shimToFunction convexpr -< a
                                pata -< t1
                                returnA -< l

partialExpressionZero ::
    forall ts.
    AbstractTypeSystem ts =>
    TSSealedPartialExpression ts
partialExpressionZero =
    case bottomShimWit @ts of
        MkSome twt -> neverSealedPartialExpression twt

partialExpressionSum ::
    forall ts.
    AbstractTypeSystem ts =>
    TSSealedPartialExpression ts ->
    TSSealedPartialExpression ts ->
    TSOuter ts (TSSealedPartialExpression ts)
partialExpressionSum (MkSealedExpression (MkPartialWit purity1 etype1) expr1) (MkSealedExpression (MkPartialWit purity2 etype2) expr2) =
    purityTypeSum purity1 purity2 $ \purity12 pconv -> do
        MkShimWit etype12 (MkPolarShim (MkComposeShim uconvexpr)) <-
            unifyUUPosShimWit @ts (uuLiftPosShimWit @ts etype1) (uuLiftPosShimWit @ts etype2)
        unifierSolve @ts uconvexpr $ \convexpr -> let
            sumExpr jshim f1 f2 =
                purityIs @Functor purity1
                    $ purityIs @Functor purity2
                    $ let
                        jf = shimToFunction jshim
                        j1 = fmap (jf . LeftJoinType) f1
                        j2 = fmap (jf . RightJoinType) f2
                        in pconv j1 j2
            in return
                $ MkSealedExpression (MkPartialWit purity12 $ mkShimWit etype12)
                $ sumExpr
                <$> convexpr
                <*> expr1
                <*> expr2

partialExpressionSumList ::
    forall ts.
    AbstractTypeSystem ts =>
    [TSSealedPartialExpression ts] ->
    TSOuter ts (TSSealedPartialExpression ts)
partialExpressionSumList [] = return $ partialExpressionZero @ts
partialExpressionSumList [e] = return e
partialExpressionSumList (e1 : ee) = do
    e2 <- partialExpressionSumList @ts ee
    partialExpressionSum @ts e1 e2

tsPartialExpressionSumList ::
    forall ts.
    AbstractTypeSystem ts =>
    [TSSealedPartialExpression ts] ->
    TSInner ts (TSSealedPartialExpression ts)
tsPartialExpressionSumList [] = return $ partialExpressionZero @ts
tsPartialExpressionSumList [e] = return e
tsPartialExpressionSumList rawee =
    runRenamer @ts [] [] $ do
        ee <- for rawee $ renameMappableSimple @ts
        partialExpressionSumList @ts ee

tsLambdaPatternMatch :: forall ts. TSVarID ts -> TSSealedPattern ts -> TSMatch ts
tsLambdaPatternMatch varid (MkSealedPattern ptype pat) =
    applyFuncPattern pat $ varExpression $ MkNameWitness varid ptype

tsExpressionPatternMatch ::
    forall ts.
    AbstractTypeSystem ts =>
    TSSealedExpression ts ->
    TSSealedPattern ts ->
    TSInner ts (TSMatch ts)
tsExpressionPatternMatch rawexpr rawpat =
    runRenamer @ts [] [] $ do
        MkSealedExpression etw expr <- renameMappableSimple @ts rawexpr
        MkSealedPattern ptype opat <- renameMappableSimple @ts rawpat
        uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts etw) (uuLiftNegShimWit @ts ptype)
        unifierSolve @ts uconv $ \convexpr -> return $ applyFuncPattern opat $ liftA2 shimToFunction convexpr expr

tsMatchGate ::
    forall ts.
    AbstractTypeSystem ts =>
    TSMatch ts ->
    TSSealedPartialExpression ts ->
    TSInner ts (TSSealedPartialExpression ts)
tsMatchGate rawmatch rawexpr =
    runRenamer @ts [] [] $ do
        MkPattern _ (MkPurityFunction ppurity pfexpr) <- renameMappableSimple @ts rawmatch
        MkSealedExpression (MkPartialWit epurity etype) expr <- renameMappableSimple @ts rawexpr
        return
            $ purityTypeProduct ppurity epurity
            $ \purity pconv econv ->
                purityIs @Monad purity
                    $ MkSealedExpression (MkPartialWit purity etype)
                    $ liftA2 (\pf et -> (pconv $ pf ()) >> econv et) pfexpr expr

tsMatchBindings :: forall ts. String -> TSMatch ts -> [(TSVarID ts, TSSealedExpression ts)]
tsMatchBindings err (MkPattern ww (MkPurityFunction purity pfexpr)) =
    purityIs @Functor purity $ let
        mkBinding ::
            SomeFor ((->) _) (NameWitness (TSVarID ts) (TSPosShimWit ts)) -> (TSVarID ts, TSSealedExpression ts)
        mkBinding (MkSomeFor (MkNameWitness wvar wtype) f) =
            ( wvar
            , partialToSealedExpression err
                $ MkSealedExpression (MkPartialWit purity wtype)
                $ fmap (\pf -> fmap (\(tt, ()) -> f tt) $ pf ()) pfexpr
            )
        in fmap mkBinding ww
