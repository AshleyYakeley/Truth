module Language.Expression.Expression where

import Language.Expression.Name
import Shapes

liftCombinator ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k) (t :: k). CategoryCombinator cat
    => cat a b
    -> cat (CatFunction cat t a) (CatFunction cat t b)
-- \km -> S (K km)
liftCombinator km = let
    apc :: forall (a' :: k) (b' :: k). CatObject cat (CatFunction cat a' b') -> CatObject cat a' -> CatObject cat b'
    apc = applyCombinator @k @cat
    s :: forall (a' :: k) (b' :: k) (c' :: k).
         CatObject cat (CatFunction cat (CatFunction cat a' (CatFunction cat b' c')) (CatFunction cat (CatFunction cat a' b') (CatFunction cat a' c')))
    s = combinatorAp @k @cat
    k :: forall (a' :: k) (b' :: k). CatObject cat (CatFunction cat a' (CatFunction cat b' a'))
    k = combinatorConst @k @cat
    in fromCombinator $ apc s $ apc k $ toCombinator km

coliftCombinator ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k) (t :: k). CategoryCombinator cat
    => cat a b
    -> cat (CatFunction cat b t) (CatFunction cat a t)
-- \km -> (C B) km
coliftCombinator km = let
    apc :: forall (a' :: k) (b' :: k). CatObject cat (CatFunction cat a' b') -> CatObject cat a' -> CatObject cat b'
    apc = applyCombinator @k @cat
    b :: forall (a' :: k) (b' :: k) (c' :: k).
         CatObject cat (CatFunction cat (CatFunction cat b' c') (CatFunction cat (CatFunction cat a' b') (CatFunction cat a' c')))
    b = combinatorCompose @k @cat
    c :: forall (a' :: k) (b' :: k) (c' :: k).
         CatObject cat (CatFunction cat (CatFunction cat a' (CatFunction cat b' c')) (CatFunction cat b' (CatFunction cat a' c')))
    c = combinatorSwapArgs @k @cat
    in fromCombinator $ apc (apc c b) $ toCombinator @k @cat km

kswapargs ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k) (c :: k). CategoryCombinator cat
    => cat (CatFunction cat a (CatFunction cat b c)) (CatFunction cat b (CatFunction cat a c))
kswapargs = fromCombinator $ combinatorSwapArgs @k @cat

unifyCombinators ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k) (ab :: k) (r :: k). CategoryCombinator cat
    => CatObject cat (CatFunction cat ab a)
    -> CatObject cat (CatFunction cat ab b)
    -> CatObject cat (CatFunction cat (CatFunction cat b (CatFunction cat a r)) (CatFunction cat ab r))
-- :: (ab -> a) -> (ab -> b) -> (b -> a -> r) -> ab -> r
-- \aba abb -> \bar ab -> bar (abb ab) (aba ab)
-- \aba abb -> C (B S (C B abb)) aba
unifyCombinators aba abb = let
    apc :: forall (a' :: k) (b' :: k). CatObject cat (CatFunction cat a' b') -> CatObject cat a' -> CatObject cat b'
    apc = applyCombinator @k @cat
    s :: forall (a' :: k) (b' :: k) (c' :: k).
         CatObject cat (CatFunction cat (CatFunction cat a' (CatFunction cat b' c')) (CatFunction cat (CatFunction cat a' b') (CatFunction cat a' c')))
    s = combinatorAp @k @cat
    b :: forall (a' :: k) (b' :: k) (c' :: k).
         CatObject cat (CatFunction cat (CatFunction cat b' c') (CatFunction cat (CatFunction cat a' b') (CatFunction cat a' c')))
    b = combinatorCompose @k @cat
    c :: forall (a' :: k) (b' :: k) (c' :: k).
         CatObject cat (CatFunction cat (CatFunction cat a' (CatFunction cat b' c')) (CatFunction cat b' (CatFunction cat a' c')))
    c = combinatorSwapArgs @k @cat
    in apc (apc c (apc (apc b s) $ apc (apc c b) abb)) aba

data Expression (varwit :: k -> Type) (f :: k -> Type) (a :: k)
    = ClosedExpression (f a)
    | forall (t :: k). OpenExpression Name
                                      (varwit t)
                                      (Expression varwit f (KindFunction k t a))

instance (FunctionKind k, KindFunctor f) => KindFunctor (Expression (varwit :: k -> Type) f) where
    kfmap ab (ClosedExpression fa) = ClosedExpression $ kfmap ab fa
    kfmap ab (OpenExpression name tp expr) = OpenExpression name tp $ kfmap (liftCombinator ab) expr

ffmapexpr ::
       forall k varwit f a b. KindApplicative f
    => f (KindFunction k a b)
    -> Expression varwit f a
    -> Expression varwit f b
ffmapexpr fab (ClosedExpression fa) = ClosedExpression $ kap fab fa
ffmapexpr fab (OpenExpression name tp expr) = let
    apc :: forall (a' :: k) (b' :: k). KindObject k (KindFunction k a' b') -> KindObject k a' -> KindObject k b'
    apc = applyCombinator @k @(KindMorphism k)
    s :: forall (a' :: k) (b' :: k) (c' :: k).
         KindObject k (KindFunction k (KindFunction k a' (KindFunction k b' c')) (KindFunction k (KindFunction k a' b') (KindFunction k a' c')))
    s = combinatorAp @k @(KindMorphism k)
    k :: forall (a' :: k) (b' :: k). KindObject k (KindFunction k a' (KindFunction k b' a'))
    k = combinatorConst @k @(KindMorphism k)
    b :: forall (a' :: k) (b' :: k) (c' :: k).
         KindObject k (KindFunction k (KindFunction k b' c') (KindFunction k (KindFunction k a' b') (KindFunction k a' c')))
    b = combinatorCompose @k @(KindMorphism k)
    in OpenExpression name tp $ ffmapexpr (kfmap (fromCombinator $ apc (apc b s) k) fab) expr

instance KindApplicative f => KindApplicative (Expression (varwit :: k -> Type) f) where
    kpure a = ClosedExpression $ kpure a
    kap (ClosedExpression fab) expr = ffmapexpr fab expr
    kap (OpenExpression name tp exprab) expr = OpenExpression name tp $ kap (kfmap kswapargs exprab) expr

exprFreeVariables :: Expression varwit f a -> [Name]
exprFreeVariables (ClosedExpression _) = []
exprFreeVariables (OpenExpression name _ expr) = name : (exprFreeVariables expr)

class UnifyWitness (varwit :: k -> Type) where
    freeWitness :: forall r. (forall t. varwit t -> r) -> r
    unifyWitnesses ::
           forall a b r.
           varwit a
        -> varwit b
        -> (forall ab. varwit ab -> KindMorphism k ab a -> KindMorphism k ab b -> Result Text r)
        -> Result Text r

exprAbstract ::
       forall (k :: Type) (varwit :: k -> Type) (f :: k -> Type) (a :: k) (r :: Type).
       (FunctionKind k, KindFunctor f, UnifyWitness varwit, TestEquality varwit)
    => Name
    -> Expression varwit f a
    -> (forall t. varwit t -> Expression varwit f (KindFunction k t a) -> Result Text r)
    -> Result Text r
exprAbstract _name (ClosedExpression fa) cont =
    freeWitness $ \vwf -> cont vwf $ ClosedExpression $ kfmap morphismConst fa
exprAbstract name (OpenExpression name' vw1 expr) cont
    | name == name' =
        exprAbstract name expr $ \vw2 absexpr ->
            unifyWitnesses vw1 vw2 $ \vw12 conva convb ->
                cont vw12 $
                kfmap
                    (fromCombinator @k @(KindMorphism k) $
                     unifyCombinators
                         @k
                         @(KindMorphism k)
                         (toCombinator @k @(KindMorphism k) conva)
                         (toCombinator @k @(KindMorphism k) convb))
                    absexpr -- (\vva v -> vva v v)
exprAbstract name (OpenExpression name' vw1 expr) cont =
    exprAbstract name expr $ \vw2 absexpr ->
        cont vw2 $ OpenExpression name' vw1 $ kfmap kswapargs absexpr -- (\vva v1 v2 -> vva v2 v1)

exprEval ::
       forall (k :: Type) (a :: k) (varwit :: k -> Type) (f :: k -> Type) (m :: Type -> Type).
       (MonadFail m, AllWitnessConstraint Show varwit)
    => Expression varwit f a
    -> m (f a)
exprEval (ClosedExpression fa) = return fa
exprEval expr@(OpenExpression _ _ _) = fail $ "undefined: " ++ (intercalate ", " $ fmap show $ exprFreeVariables expr)

exprVar ::
       forall (r :: Type) (k :: Type) (varwit :: k -> Type) (f :: k -> Type). (KindApplicative f, UnifyWitness varwit)
    => Name
    -> (forall (t :: k). Expression varwit f t -> r)
    -> r
exprVar name cont =
    freeWitness $ \vw -> cont $ OpenExpression name vw $ ClosedExpression $ kpure $ combinatorId @k @(KindMorphism k)

exprMapSymbol ::
       (FunctionKind k, KindFunctor f)
    => (forall (r :: Type) (t1 :: k). varwit1 t1 -> (forall (t2 :: k). varwit2 t2 -> KindMorphism k t2 t1 -> r) -> r)
    -> Expression varwit1 f a
    -> Expression varwit2 f a
exprMapSymbol _f (ClosedExpression a) = ClosedExpression a
exprMapSymbol f (OpenExpression name tp expr) =
    f tp $ \tp' km -> OpenExpression name tp' $ kfmap (coliftCombinator km) $ exprMapSymbol f expr
