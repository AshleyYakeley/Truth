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
    , multiCaseAbstractSealedExpression
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
    -> (forall pa. UUShim ts pa a -> TSOpenExpression ts (PurityFunction Maybe q (pa, t)) -> TSOuter ts r)
    -> TSOuter ts r
patternAbstractUnifyExpression (ClosedPattern qmt) expr cont = cont id $ fmap (\a -> fmap (\t -> (a, t)) qmt) expr
patternAbstractUnifyExpression (OpenPattern (MkNameWitness name vwt) pat) expr cont = do
    MkAbstractResult absvwt absexpr <- abstractNamedExpression @ts name expr
    uabsconv <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit @ts vwt) absvwt
    patternAbstractUnifyExpression @ts pat absexpr $ \uuconv rexpr ->
        cont (applf uuconv uabsconv) $ fmap (fmap $ \(pa, (t1, t)) -> (BothMeetType pa t1, t)) rexpr

data PurityType (f :: Type -> Type) where
    PureType :: PurityType Identity
    ImpureType :: PurityType Maybe

purityIsMonad :: PurityType f -> (MonadInner f => r) -> r
purityIsMonad PureType cr = cr
purityIsMonad ImpureType cr = cr

matchPurityType ::
       PurityType fa
    -> PurityType fb
    -> (forall fab. MonadInner fab => PurityType fab -> (fa --> fab) -> (fb --> fab) -> r)
    -> r
matchPurityType PureType PureType call = call PureType id id
matchPurityType PureType ImpureType call = call ImpureType (Just . runIdentity) id
matchPurityType ImpureType PureType call = call ImpureType id (Just . runIdentity)
matchPurityType ImpureType ImpureType call = call ImpureType id id

data OpenPurityExpression ts a b =
    forall f. MkOpenPurityExpression (PurityType f)
                                     (TSOpenExpression ts (a -> f b))

instance Functor (OpenPurityExpression ts a) where
    fmap ab (MkOpenPurityExpression purity expr) =
        MkOpenPurityExpression purity $ purityIsMonad purity $ fmap (fmap $ fmap ab) expr

patternAbstractUnifyPFExpression ::
       forall ts ap bp bf r. AbstractTypeSystem ts
    => TSOpenPattern ts ap bp
    -> TSOpenExpression ts bf
    -> (forall bf'. UUShim ts bf' bf -> OpenPurityExpression ts ap (bp, bf') -> TSOuter ts r)
    -> TSOuter ts r
patternAbstractUnifyPFExpression (ClosedPattern (PureFunction qt)) expr cont =
    cont id $ MkOpenPurityExpression PureType $ fmap (\a q -> pure (qt q, a)) expr
patternAbstractUnifyPFExpression (ClosedPattern (ImpureFunction qmt)) expr cont =
    cont id $ MkOpenPurityExpression ImpureType $ fmap (\a q -> fmap (\t -> (t, a)) $ qmt q) expr
patternAbstractUnifyPFExpression (OpenPattern (MkNameWitness name vwt) pat) expr cont = do
    MkAbstractResult absvwt absexpr <- abstractNamedExpression @ts name expr
    uabsconv <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit @ts vwt) absvwt
    patternAbstractUnifyPFExpression @ts pat absexpr $ \uuconv rexpr ->
        cont (applf uuconv uabsconv) $ fmap (\((t, bp), bf) -> (bp, BothMeetType bf t)) rexpr

data UnifierPurityFunction ts a b =
    forall f. MkUnifierPurityFunction (PurityType f)
                                      (UnifierExpression ts (a -> f b))

instance UnifyTypeSystem ts => Functor (UnifierPurityFunction ts a) where
    fmap ab (MkUnifierPurityFunction purity expr) =
        MkUnifierPurityFunction purity $ purityIsMonad purity $ fmap (fmap $ fmap ab) expr

instance UnifyTypeSystem ts => Applicative (UnifierPurityFunction ts a) where
    pure b = arr $ \_ -> b
    liftA2 f (MkUnifierPurityFunction purityA exprA) (MkUnifierPurityFunction purityB exprB) =
        matchPurityType purityA purityB $ \purityAB cA cB ->
            MkUnifierPurityFunction purityAB $ liftA2 (liftA2 $ \fa fb -> liftA2 f (cA fa) (cB fb)) exprA exprB

mii :: Maybe a -> Identity a -> Identity a
mii (Just a) _ = Identity a
mii Nothing ia = ia

instance UnifyTypeSystem ts => Alternative (UnifierPurityFunction ts a) where
    empty = MkUnifierPurityFunction ImpureType $ pure $ pure empty
    MkUnifierPurityFunction PureType expr <|> _ = MkUnifierPurityFunction PureType expr
    MkUnifierPurityFunction ImpureType expr1 <|> MkUnifierPurityFunction PureType expr2 =
        MkUnifierPurityFunction PureType $ liftA2 (liftA2 mii) expr1 expr2
    MkUnifierPurityFunction ImpureType expr1 <|> MkUnifierPurityFunction ImpureType expr2 =
        MkUnifierPurityFunction ImpureType $ liftA2 (liftA2 (<|>)) expr1 expr2

instance UnifyTypeSystem ts => Category (UnifierPurityFunction ts) where
    id = MkUnifierPurityFunction PureType $ pure Identity
    MkUnifierPurityFunction purityBC exprBC . MkUnifierPurityFunction purityAB exprAB =
        matchPurityType purityBC purityAB $ \purityAC c1 c2 ->
            MkUnifierPurityFunction purityAC $ liftA2 (\bfc afb a -> c2 (afb a) >>= \b -> c1 $ bfc b) exprBC exprAB

instance UnifyTypeSystem ts => Arrow (UnifierPurityFunction ts) where
    arr f = MkUnifierPurityFunction PureType $ pure $ \a -> Identity $ f a
    first (MkUnifierPurityFunction purity expr) =
        MkUnifierPurityFunction purity $ purityIsMonad purity $ fmap (\bfc (b, d) -> fmap (\c -> (c, d)) $ bfc b) expr

openUnifierPurityFunction :: UnifyTypeSystem ts => TSOpenExpression ts a -> UnifierPurityFunction ts () a
openUnifierPurityFunction expr =
    MkUnifierPurityFunction PureType $ solverExpressionLiftValue $ fmap (\a () -> Identity a) expr

patternAbstractUnifyThingExpression ::
       forall ts ap bp bf. AbstractTypeSystem ts
    => TSOpenPattern ts ap bp
    -> UnifierExpression ts bf
    -> TSOuter ts (UnifierPurityFunction ts ap (bp, bf))
patternAbstractUnifyThingExpression pat (MkSolverExpression uua exp) =
    patternAbstractUnifyPFExpression @ts pat exp $ \(MkComposeShim conv) (MkOpenPurityExpression purity texpr) ->
        return $
        MkUnifierPurityFunction purity $
        purityIsMonad purity $
        (\sptbf t apf ap -> fmap (second $ \pa -> shimToFunction sptbf pa t) $ apf ap) <$> conv <*>
        solverExpressionLiftType uua <*>
        solverExpressionLiftValue texpr

patternAbstractUnifyThing2Expression ::
       forall ts ap bp af bf. AbstractTypeSystem ts
    => TSOpenPattern ts ap bp
    -> UnifierPurityFunction ts af bf
    -> TSOuter ts (UnifierPurityFunction ts (ap, af) (bp, bf))
patternAbstractUnifyThing2Expression pat (MkUnifierPurityFunction purity1 uexpr1) = do
    MkUnifierPurityFunction purity2 uexpr2 <- patternAbstractUnifyThingExpression @ts pat uexpr1
    return $
        matchPurityType purity1 purity2 $ \purity12 c1 c2 ->
            MkUnifierPurityFunction purity12 $
            fmap
                (\af2z (ap, af) -> do
                     (bp, afbf) <- c2 $ af2z ap
                     bf <- c1 $ afbf af
                     return (bp, bf))
                uexpr2

data PatternResult ts =
    forall t a. MkPatternResult (UUPosShimWit ts a)
                                (UUNegShimWit ts t)
                                (TSOpenExpression ts (PurityFunction Maybe t a))

runPurity :: PurityType f -> f a -> a
runPurity PureType (Identity a) = a
runPurity ImpureType (Just a) = a
runPurity ImpureType Nothing = error "missing case"

runPurityFunction :: PurityFunction Maybe a b -> a -> b
runPurityFunction (PureFunction ab) a = ab a
runPurityFunction (ImpureFunction amb) a =
    case amb a of
        Just b -> b
        Nothing -> error "missing case"

joinPurityFunctions ::
       PurityFunction Maybe a1 b1
    -> PurityFunction Maybe a2 b2
    -> PurityFunction Maybe (MeetType a1 a2) (JoinType b1 b2)
joinPurityFunctions pf1 pf2 = (arr join1 . pf1 . arr meet1) <|> (arr join2 . pf2 . arr meet2)

joinPatternResult ::
       forall ts. UnifyTypeSystem ts
    => PatternResult ts
    -> PatternResult ts
    -> TSOuter ts (PatternResult ts)
joinPatternResult (MkPatternResult pa na expra) (MkPatternResult pb nb exprb) = do
    pab <- unifyUUPosShimWit @ts pa pb
    nab <- unifyUUNegShimWit @ts na nb
    return $ MkPatternResult pab nab $ joinPurityFunctions <$> expra <*> exprb

joinPatternResults ::
       forall ts. AbstractTypeSystem ts
    => [PatternResult ts]
    -> TSOuter ts (PatternResult ts)
joinPatternResults [] = do
    MkNewVar tt _ <- renameNewFreeVar @ts
    MkNewVar _ ta <- renameNewFreeVar @ts
    return $ MkPatternResult (uuLiftPosShimWit @ts ta) (uuLiftNegShimWit @ts tt) $ pure empty
joinPatternResults (p:pp) = do
    c <- joinPatternResults pp
    joinPatternResult p c

patternAbstractSealedExpression ::
       forall ts. AbstractTypeSystem ts
    => TSSealedExpressionPattern ts
    -> TSSealedExpression ts
    -> TSOuter ts (PatternResult ts)
patternAbstractSealedExpression (MkSealedPattern vwt pat) (MkSealedExpression twt expr) =
    patternAbstractUnifyExpression @ts pat expr $ \uconv uexpr' ->
        return $
        MkPatternResult (mapPosShimWit (applf id uconv) $ uuLiftPosShimWit @ts twt) (uuLiftNegExpressionShimWit @ts vwt) $
        (fmap $ fmap $ \(pa, ()) -> BothMeetType id pa) uexpr'

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
    upf <- patternAbstractUnifyThing2Expression @ts pat texpr
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
        MkPatternResult (MkShimWit rtt (MkPolarMap tuconv)) rvwt rexpr <- joinPatternResults patrs
        MkSealedExpression btwt bexpr <- rename @ts FreeName sbexpr
        uconv <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit @ts btwt) rvwt
        (convexpr, subs) <- solveUnifierExpression @ts $ liftA2 (,) (uuGetShim @ts uconv) (uuGetShim @ts tuconv)
        unifierSubstituteAndSimplify @ts subs $
            MkSealedExpression (mkShimWit rtt) $
            (\(conv, tconv) fta t -> shimToFunction tconv $ runPurityFunction fta $ shimToFunction conv t) <$> convexpr <*>
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
        MkPatternResult (MkShimWit rtt (MkPolarMap tuconv)) (MkShimWit rvt (MkPolarMap vuconv)) rexpr <-
            joinPatternResults patrs
        (convexpr, subs) <- solveUnifierExpression @ts $ liftA2 (,) (uuGetShim @ts tuconv) (uuGetShim @ts vuconv)
        unifierSubstituteAndSimplify @ts subs $
            MkSealedExpression (absw (mkShimWit rvt) (mkShimWit rtt)) $
            (\(tconv, vconv) fta t -> shimToFunction tconv $ runPurityFunction fta $ shimToFunction vconv t) <$>
            convexpr <*>
            rexpr

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
    runRenamer @ts $
    withTransConstraintTM @Monad $ do
        patrs <-
            for rawcases $ \(rawpatlist, rawexpr) -> do
                patlist <- for rawpatlist $ rename @ts FreeName
                expr <- rename @ts FreeName rawexpr
                multiPatternAbstractSealedExpression @ts patlist expr
        MkMultiPatternResult (MkShimWit rtt (MkPolarMap tuconv)) nl (MkUnifierPurityFunction purity urexpr) <-
            joinMultiPatternResults nn patrs
        negativeToListShim nl $ \rvts vuconvs -> do
            (convexpr, subs) <-
                solveUnifierExpression @ts $ (,,) <$> uuGetShim @ts tuconv <*> sequenceListShim vuconvs <*> urexpr
            unifierSubstituteAndSimplify @ts subs $
                MkSealedExpression (listAbstract @ts absw rvts (mkShimWit rtt)) $
                (\(tconv, vconv, fta) ->
                     productToFunctionOnList vconv $
                     shimToFunction tconv . runPurity purity . fta . listShimApply shimToFunction vconv) <$>
                convexpr

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
