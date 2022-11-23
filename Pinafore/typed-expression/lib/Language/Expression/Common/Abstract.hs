{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Abstract
    ( AbstractTypeSystem(..)
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
    , caseSealedExpression
    , caseAbstractSealedExpression
    , applyPatternConstructor
    , multiCaseAbstractSealedExpression
    , MatchResult(..)
    , tsMatchSealedPattern
    ) where

import Data.Shim
import Language.Expression.Common.Error
import Language.Expression.Common.Expression
import Language.Expression.Common.Named
import Language.Expression.Common.Pattern
import Language.Expression.Common.Rename
import Language.Expression.Common.Sealed
import Language.Expression.Common.Simplifier
import Language.Expression.Common.SolverExpression
import Language.Expression.Common.TypeSystem
import Language.Expression.Common.Unifier
import Language.Expression.Common.UnifierPurityFunction
import Language.Expression.Common.WitnessMappable
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

unifierSubstituteSimplifyFinalRename ::
       forall ts a. (AbstractTypeSystem ts, TSMappable ts a)
    => UnifierSubstitutions ts
    -> a
    -> TSOuter ts a
unifierSubstituteSimplifyFinalRename subs a = do
    a' <- unifierSubstitute @ts subs a
    a'' <- simplify @ts a'
    -- finalRenamer only needed to "clean up" type variable names
    finalRenamer @ts $ rename @ts FreeName a''

unifierSolve ::
       forall ts a b. (AbstractTypeSystem ts, TSMappable ts b)
    => UnifierExpression ts a
    -> (TSOpenExpression ts a -> TSOuter ts b)
    -> TSOuter ts b
unifierSolve uexpr mab = do
    (expr, subs) <- solveUnifierExpression @ts uexpr
    b <- mab expr
    unifierSubstituteSimplifyFinalRename @ts subs b

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

abstractUnifierExpression ::
       forall ts t a. AbstractTypeSystem ts
    => NameWitness (TSVarID ts) (TSPosShimWit ts) t
    -> UnifierExpression ts a
    -> TSOuter ts (UnifierExpression ts (t -> a))
abstractUnifierExpression (MkNameWitness name twt) (MkSolverExpression upf expr) = do
    MkSolverExpression upf' expr' <- abstractExpression @ts name twt expr
    return $ MkSolverExpression (liftA2 (,) upf upf') $ fmap (\ff (t1, t2) t -> ff t2 t t1) expr'

data MatchResult (ts :: Type) =
    forall f t. MkMatchResult (PurityType Maybe f)
                              (TSOpenExpression ts (f t))
                              [(TSVarID ts, TSPosShimWit ts t)]

instance (poswit ~ TSPosShimWit ts, negwit ~ TSNegShimWit ts) => WitnessMappable poswit negwit (MatchResult ts) where
    mapWitnessesM mapPos mapNeg (MkMatchResult purity expr ww) =
        MkMatchResult purity <$> mapWitnessesM mapPos mapNeg expr <*>
        (for ww $ \(v, w) -> fmap (\w' -> (v, w')) $ mapWitnessesM mapPos mapNeg w)

tsMatchSealedPattern ::
       forall ts. AbstractTypeSystem ts
    => TSSealedExpressionPattern ts
    -> TSSealedExpression ts
    -> TSInner ts (MatchResult ts)
tsMatchSealedPattern rawpat rawexpr = do
    runRenamer @ts [] [] $ do
        MkSealedPattern (MkExpressionWitness pw convexpr) pat <- rename @ts FreeName rawpat
        MkSealedExpression tw expr <- rename @ts FreeName rawexpr
        uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts tw) (uuLiftNegShimWit @ts pw)
        unifierSolve
            @ts
            ((\shim r t -> BothMeetType (shimToFunction shim t) r) <$> uconv <*> solverExpressionLiftValue convexpr <*>
             solverExpressionLiftValue expr) $ \expr' ->
            tsMatchPattern @ts pat expr' $ \purity rexpr ww ->
                return $ purityIs @Functor purity $ MkMatchResult purity (fmap (fmap fst) rexpr) ww

patternAbstractUnifyExpression ::
       forall ts ap bp bf. AbstractTypeSystem ts
    => TSOpenPattern ts ap bp
    -> TSOpenExpression ts bf
    -> TSOuter ts (UnifierPurityFunction ts ap (bp, bf))
patternAbstractUnifyExpression (MkPattern ww (MkPurityFunction purity (qmt :: _ (t, _)))) expr =
    purityIs @Functor purity $ do
        let
            uexpr0 :: UnifierExpression ts (t -> bf)
            uexpr0 = solverExpressionLiftValue $ fmap (\bf _ -> bf) expr
        uexprf <-
            foldM
                (\uexpr (MkSomeFor w f) -> fmap (fmap $ \atb t -> atb (f t) t) $ abstractUnifierExpression @ts w uexpr)
                uexpr0
                ww
        return $ MkUnifierPurityFunction purity $ fmap (\tb -> fmap (\(t, bp) -> (bp, tb t)) qmt) uexprf

patternAbstractUnifyUPF ::
       forall ts ap bp af bf. AbstractTypeSystem ts
    => TSOpenPattern ts ap bp
    -> UnifierPurityFunction ts af bf
    -> TSOuter ts (UnifierPurityFunction ts (ap, af) (bp, bf))
patternAbstractUnifyUPF pat (MkUnifierPurityFunction purity1 (MkSolverExpression uua exp)) = do
    MkUnifierPurityFunction purity2 uexpr <- patternAbstractUnifyExpression @ts pat exp
    return $
        matchPurityType purity1 purity2 $ \purity12 c1 c2 ->
            purityIs @Monad purity12 $
            MkUnifierPurityFunction purity12 $
            (\t apf ->
                 Kleisli $ \(ap, af) -> do
                     (bp, bf) <- c2 $ runKleisli apf ap
                     bf' <- c1 $ runKleisli (bf t) af
                     return (bp, bf')) <$>
            solverExpressionLiftType uua <*>
            uexpr

data PatternResult ts =
    forall t a. MkPatternResult (UUPosShimWit ts a)
                                (UUNegShimWit ts t)
                                (UnifierPurityFunction ts t a)

joinPatternResult ::
       forall ts. UnifyTypeSystem ts
    => PatternResult ts
    -> PatternResult ts
    -> TSOuter ts (PatternResult ts)
joinPatternResult (MkPatternResult pa na expra) (MkPatternResult pb nb exprb) = do
    pab <- unifyUUPosShimWit @ts pa pb
    nab <- unifyUUNegShimWit @ts na nb
    return $ MkPatternResult pab nab $ (arr join1 . expra . arr meet1) <|> (arr join2 . exprb . arr meet2)

joinPatternResults ::
       forall ts. AbstractTypeSystem ts
    => [PatternResult ts]
    -> TSOuter ts (PatternResult ts)
joinPatternResults [] = do
    MkNewVar tt _ <- renameNewFreeVar @ts
    MkNewVar _ ta <- renameNewFreeVar @ts
    return $ MkPatternResult (uuLiftPosShimWit @ts ta) (uuLiftNegShimWit @ts tt) empty
joinPatternResults (p:pp) = do
    c <- joinPatternResults pp
    joinPatternResult p c

patternAbstractSealedExpression ::
       forall ts. AbstractTypeSystem ts
    => TSSealedExpressionPattern ts
    -> TSSealedExpression ts
    -> TSOuter ts (PatternResult ts)
patternAbstractSealedExpression (MkSealedPattern vwt pat) (MkSealedExpression twt expr) = do
    upfexpr <- patternAbstractUnifyExpression @ts pat expr
    return $ MkPatternResult (uuLiftPosShimWit @ts twt) (uuLiftNegExpressionShimWit @ts vwt) $ fmap snd upfexpr

type MultiPatternResult :: Type -> PeanoNat -> Type
data MultiPatternResult ts n =
    forall (tt :: [Type]) a. (ListLength tt ~ n) =>
                                 MkMultiPatternResult (UUPosShimWit ts a)
                                                      (ListType (UUNegShimWit ts) tt)
                                                      (UnifierPurityFunction ts (ListProduct tt) a)

type ListShim :: ShimKind Type -> [Type] -> [Type] -> Type
data ListShim shim aa bb where
    NilListShim :: ListShim shim '[] '[]
    ConsListShim :: shim a b -> ListShim shim aa bb -> ListShim shim (a ': aa) (b ': bb)

listShimApply ::
       forall shim aa bb. (forall a b. shim a b -> a -> b) -> ListShim shim aa bb -> ListProduct aa -> ListProduct bb
listShimApply _ NilListShim () = ()
listShimApply f (ConsListShim shim shims) (a, aa) = (f shim a, listShimApply f shims aa)

sequenceListShim :: Applicative f => ListShim (ComposeShim f shim) aa bb -> f (ListShim shim aa bb)
sequenceListShim NilListShim = pure NilListShim
sequenceListShim (ConsListShim (MkComposeShim fshim) shims) = ConsListShim <$> fshim <*> sequenceListShim shims

negativeToListShim ::
       ListType (ShimWit (PolarMap shim 'Negative) wit) tt
    -> (forall aa. ListType wit aa -> ListShim shim aa tt -> r)
    -> r
negativeToListShim NilListType call = call NilListType NilListShim
negativeToListShim (ConsListType (MkShimWit w (MkPolarMap conv)) ww) call =
    negativeToListShim ww $ \tt ss -> call (ConsListType w tt) (ConsListShim conv ss)

multiJoin ::
       forall ts (r :: Type) tta ttb. (UnifyTypeSystem ts, ListLength tta ~ ListLength ttb)
    => ListType (UUNegShimWit ts) tta
    -> ListType (UUNegShimWit ts) ttb
    -> (forall ttab.
            (ListLength tta ~ ListLength ttab) =>
                    ListType (UUNegShimWit ts) ttab -> (ListProduct ttab -> ListProduct tta) -> (ListProduct ttab -> ListProduct ttb) -> TSOuter ts r)
    -> TSOuter ts r
multiJoin NilListType NilListType call = call NilListType id id
multiJoin (ConsListType a aa) (ConsListType b bb) call = do
    ab <- unifyUUNegShimWit @ts a b
    multiJoin @ts aa bb $ \aabb aconv bconv ->
        call (ConsListType ab aabb) (\(x, xx) -> (meet1 x, aconv xx)) (\(x, xx) -> (meet2 x, bconv xx))

joinMultiPatternResult ::
       forall ts n. UnifyTypeSystem ts
    => MultiPatternResult ts n
    -> MultiPatternResult ts n
    -> TSOuter ts (MultiPatternResult ts n)
joinMultiPatternResult (MkMultiPatternResult pa na expra) (MkMultiPatternResult pb nb exprb) = do
    pab <- unifyUUPosShimWit @ts pa pb
    multiJoin @ts na nb $ \nab aconv bconv ->
        return $ MkMultiPatternResult pab nab $ (arr join1 . expra . arr aconv) <|> (arr join2 . exprb . arr bconv)

multiNew ::
       forall ts n r. AbstractTypeSystem ts
    => PeanoNatType n
    -> (forall tt. (ListLength tt ~ n) => ListType (UUNegShimWit ts) tt -> TSOuter ts r)
    -> TSOuter ts r
multiNew ZeroType call = call NilListType
multiNew (SuccType n) call = do
    MkNewVar tt _ <- renameNewFreeVar @ts
    multiNew @ts n $ \lt -> call $ ConsListType (uuLiftNegShimWit @ts tt) lt

joinMultiPatternResults ::
       forall ts n. AbstractTypeSystem ts
    => PeanoNatType n
    -> [MultiPatternResult ts n]
    -> TSOuter ts (MultiPatternResult ts n)
joinMultiPatternResults nn [] = do
    MkNewVar _ ta <- renameNewFreeVar @ts
    multiNew @ts nn $ \lnwit -> return $ MkMultiPatternResult (uuLiftPosShimWit @ts ta) lnwit empty
joinMultiPatternResults nn (p:pp) = do
    c <- joinMultiPatternResults nn pp
    joinMultiPatternResult p c

multiPatternAbstractSealedExpression ::
       forall ts n. AbstractTypeSystem ts
    => FixedList n (TSSealedExpressionPattern ts)
    -> TSSealedExpression ts
    -> TSOuter ts (MultiPatternResult ts n)
multiPatternAbstractSealedExpression NilFixedList (MkSealedExpression t expr) =
    return $ MkMultiPatternResult (uuLiftPosShimWit @ts t) NilListType $ openUnifierPurityFunction expr
multiPatternAbstractSealedExpression (ConsFixedList (MkSealedPattern vwt pat) pats) expr = do
    MkMultiPatternResult twt vww texpr <- multiPatternAbstractSealedExpression @ts pats expr
    upf <- patternAbstractUnifyUPF @ts pat texpr
    return $ MkMultiPatternResult twt (ConsListType (uuLiftNegExpressionShimWit @ts vwt) vww) $ fmap snd upf

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
        MkSealedExpression twt expr <- rename @ts FreeName sexpr
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
        MkSealedExpression tf exprf <- rename @ts FreeName sexprf
        MkSealedExpression ta expra <- rename @ts FreeName sexpra
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
        MkSealedExpression te expre <- rename @ts FreeName sexpre
        MkSealedExpression tb exprb <- rename @ts FreeName sexprb
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
        MkSealedPattern tw1 pat1 <- rename @ts FreeName spat1
        MkSealedPattern tw2 pat2 <- rename @ts FreeName spat2
        MkShimWit tr (MkPolarMap uconv) <-
            unifyUUNegShimWit @ts (uuLiftNegExpressionShimWit @ts tw1) (uuLiftNegExpressionShimWit @ts tw2)
        unifierSolve @ts (uuGetShim @ts uconv) $ \convexpr ->
            return $
            MkSealedPattern (MkExpressionWitness (mkShimWit tr) convexpr) $
            proc (MkMeetType (t, shim)) -> do
                let ab = shimToFunction shim t
                pat1 -< meet1 ab
                pat2 -< meet2 ab

casesPatternResult ::
       forall ts. AbstractTypeSystem ts
    => [(TSSealedExpressionPattern ts, TSSealedExpression ts)]
    -> TSOuter ts (PatternResult ts)
casesPatternResult rawcases = do
    patrs <-
        for rawcases $ \(rawpat, rawexpr) -> do
            pat <- rename @ts FreeName rawpat
            expr <- rename @ts FreeName rawexpr
            patternAbstractSealedExpression @ts pat expr
    joinPatternResults patrs

caseSealedExpression ::
       forall ts. AbstractTypeSystem ts
    => TSSealedExpression ts
    -> [(TSSealedExpressionPattern ts, TSSealedExpression ts)]
    -> TSInner ts (TSSealedExpression ts)
caseSealedExpression sbexpr rawcases =
    runRenamer @ts [] [] $
    withTransConstraintTM @Monad $ do
        MkPatternResult (MkShimWit rtt (MkPolarMap tuconv)) rvwt upfexpr <- casesPatternResult @ts rawcases
        MkSealedExpression btwt bexpr <- rename @ts FreeName sbexpr
        uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts btwt) rvwt
        unifierSolve
            @ts
            ((\conv tconv ta -> shimToFunction tconv . ta . shimToFunction conv) <$> uconv <*> uuGetShim @ts tuconv <*>
             runUnifierPurityFunction upfexpr) $ \convexpr ->
            return $ MkSealedExpression (mkShimWit rtt) $ convexpr <*> bexpr

caseAbstractSealedExpression ::
       forall ts. AbstractTypeSystem ts
    => UnifierFunctionPosWitness ts
    -> [(TSSealedExpressionPattern ts, TSSealedExpression ts)]
    -> TSInner ts (TSSealedExpression ts)
caseAbstractSealedExpression absw rawcases =
    runRenamer @ts [] [] $
    withTransConstraintTM @Monad $ do
        MkPatternResult (MkShimWit rtt (MkPolarMap tuconv)) (MkShimWit rvt (MkPolarMap vuconv)) upfexpr <-
            casesPatternResult @ts rawcases
        unifierSolve
            @ts
            ((\tconv vconv ta -> shimToFunction tconv . ta . shimToFunction vconv) <$> uuGetShim @ts tuconv <*>
             uuGetShim @ts vuconv <*>
             runUnifierPurityFunction upfexpr) $ \convexpr ->
            return $ MkSealedExpression (absw (mkShimWit rvt) (mkShimWit rtt)) convexpr

type FunctionOnList :: [Type] -> Type -> Type
type family FunctionOnList tt b where
    FunctionOnList '[] b = b
    FunctionOnList (a ': aa) b = a -> FunctionOnList aa b

productToFunctionOnList :: ListShim shim aa bb -> (ListProduct aa -> b) -> FunctionOnList aa b
productToFunctionOnList NilListShim f = f ()
productToFunctionOnList (ConsListShim _ ls) f = \a -> productToFunctionOnList ls $ \l -> f (a, l)

listAbstract ::
       forall ts aa b. UnifyTypeSystem ts
    => UnifierFunctionPosWitness ts
    -> ListType (TSNegWitness ts) aa
    -> TSPosShimWit ts b
    -> TSPosShimWit ts (FunctionOnList aa b)
listAbstract _ NilListType b = b
listAbstract absw (ConsListType a aa) b = absw (mkShimWit a) $ listAbstract @ts absw aa b

multiCaseAbstractSealedExpression ::
       forall ts n. AbstractTypeSystem ts
    => UnifierFunctionPosWitness ts
    -> PeanoNatType n
    -> [(FixedList n (TSSealedExpressionPattern ts), TSSealedExpression ts)]
    -> TSInner ts (TSSealedExpression ts)
multiCaseAbstractSealedExpression absw nn rawcases =
    runRenamer @ts [] [] $
    withTransConstraintTM @Monad $ do
        patrs <-
            for rawcases $ \(rawpatlist, rawexpr) -> do
                patlist <- for rawpatlist $ rename @ts FreeName
                expr <- rename @ts FreeName rawexpr
                multiPatternAbstractSealedExpression @ts patlist expr
        MkMultiPatternResult (MkShimWit rtt (MkPolarMap tuconv)) nl upfexpr <- joinMultiPatternResults nn patrs
        negativeToListShim nl $ \rvts vuconvs ->
            unifierSolve
                @ts
                ((\tconv vconv ta ->
                      productToFunctionOnList vconv $ shimToFunction tconv . ta . listShimApply shimToFunction vconv) <$>
                 uuGetShim @ts tuconv <*>
                 sequenceListShim vuconvs <*>
                 runUnifierPurityFunction upfexpr) $ \convexpr ->
                return $ MkSealedExpression (listAbstract @ts absw rvts (mkShimWit rtt)) $ convexpr

applyPatternConstructor ::
       forall ts. AbstractTypeSystem ts
    => TSExpressionPatternConstructor ts
    -> TSSealedExpressionPattern ts
    -> TSInner ts (TSExpressionPatternConstructor ts)
applyPatternConstructor patcon patarg =
    runRenamer @ts [] [] $
    withTransConstraintTM @Monad $ do
        MkPatternConstructor (MkExpressionWitness pcw pcconvexpr) pclt pcpat <- rename @ts FreeName patcon
        case pclt of
            NilListType -> lift $ throw PatternTooManyConsArgsError
            ConsListType pca pcla -> do
                MkSealedPattern (MkExpressionWitness ta tconvexpr) pata <- rename @ts FreeName patarg
                uconv <- unifyPosNegShimWit @ts (uuLiftPosShimWit @ts pca) (uuLiftNegShimWit @ts ta)
                unifierSolve @ts uconv $ \convexpr ->
                    return $
                    MkPatternConstructor (MkExpressionWitness pcw $ (,,) <$> pcconvexpr <*> tconvexpr <*> convexpr) pcla $
                    proc (MkMeetType (t, (r, r1, conv))) -> do
                        (a, l) <- pcpat -< MkMeetType (t, r)
                        pata -< MkMeetType (shimToFunction conv a, r1)
                        returnA -< l
