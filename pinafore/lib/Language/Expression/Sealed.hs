{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Sealed where

import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Renamer
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
       forall unifier renamer.
       ( Renamer renamer
       , Unifier unifier
       , RenamerNegWitness renamer ~ UnifierNegWitness unifier
       , RenamerPosWitness renamer ~ UnifierPosWitness unifier
       )
    => renamer (Abstracter unifier)
abstractNamedExpression =
    renameNewVar $ \vwt0 _ ->
        return $
        MkAbstracter $ \name expr ->
            abstractNamedExpressionUnifier @unifier name vwt0 expr $ \vwt uconv expr' ->
                return $ MkAbstractResult vwt $ MkUnifyExpression uconv $ fmap (\tua t'ttu -> tua . snd . t'ttu) expr'

data SealedExpression name vw tw =
    forall t. MkSealedExpression (tw t)
                                 (NamedExpression name vw t)

renameSealedExpression ::
       Renamer rn
    => SealedExpression name (RenamerNegWitness rn) (RenamerPosWitness rn)
    -> rn (SealedExpression name (RenamerNegWitness rn) (RenamerPosWitness rn))
renameSealedExpression (MkSealedExpression twt expr) =
    namespace $ do
        expr' <- renameExpression expr
        renamePosWitness twt $ \twt' bij -> return $ MkSealedExpression twt' $ fmap (biForwards bij) expr'

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
    runRenamer @renamer $ do
        MkSealedExpression twt expr <- renameSealedExpression sexpr
        MkAbstracter abstract <- abstractNamedExpression @unifier
        return $ do
            MkAbstractResult vwt (unifierExpression -> uexpr') <- abstract name expr
            solveUnifier @unifier twt uexpr' $ \twt' tconv expr' ->
                absw vwt twt' $ \twf abconv -> return $ MkSealedExpression twf $ fmap (abconv . fmap tconv) expr'

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
    runRenamer @renamer $ do
        MkSealedExpression tf exprf <- renameSealedExpression sexprf
        MkSealedExpression ta expra <- renameSealedExpression sexpra
        renameNewVar $ \vx tx ->
            return $
            appw ta vx $ \vax convf -> do
                uconv <- unifyPosNegWitnesses tf vax
                solveUnifier @unifier tx uconv $ \tx' tconv convu ->
                    return $ MkSealedExpression tx' $ (\t t1 -> tconv $ convf (convu t) t1) <$> exprf <*> expra

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
    runRenamer @renamer $ do
        MkSealedExpression te expre <- renameSealedExpression sexpre
        MkSealedExpression tb exprb <- renameSealedExpression sexprb
        MkAbstracter abstract <- abstractNamedExpression @unifier
        return $ do
            MkAbstractResult vt (unifierExpression -> uexprf) <- abstract name exprb
            uconvet <- unifyPosNegWitnesses te vt
            solveUnifier @unifier tb ((\exprf convet -> fmap (\tt2 -> tt2 . convet) exprf) <$> uexprf <*> uconvet) $ \tb' tconv exprf' ->
                return $ MkSealedExpression tb' $ (fmap tconv) <$> exprf' <*> expre

constSealedExpression :: tw t -> t -> SealedExpression name vw tw
constSealedExpression twt t = MkSealedExpression twt $ pure t

evalSealedExpression :: (MonadFail m, Show name) => SealedExpression name vw tw -> m (Any tw)
evalSealedExpression (MkSealedExpression twa expr) = do
    a <- evalExpression expr
    return $ MkAny twa a

varSealedExpression :: name -> vw t -> tw t -> SealedExpression name vw tw
varSealedExpression n vwt twt = MkSealedExpression twt $ varNamedExpression n vwt

type instance Element (SealedExpression name vw ((:~:) val)) = val

instance MonoFunctor (SealedExpression name vw ((:~:) val)) where
    omap ab (MkSealedExpression Refl expr) = MkSealedExpression Refl $ fmap ab expr

instance MonoPointed (SealedExpression name vw ((:~:) val)) where
    opoint = constSealedExpression Refl

instance MonoApplicative (SealedExpression name vw ((:~:) val)) where
    oliftA2 appf (MkSealedExpression Refl vexpr) (MkSealedExpression Refl bexpr) =
        MkSealedExpression Refl $ appf <$> vexpr <*> bexpr
    osequenceA conv exprs =
        MkSealedExpression Refl $ fmap conv $ sequenceA $ fmap (\(MkSealedExpression Refl expr) -> expr) exprs
