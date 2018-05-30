module Control.Category.Combinator where

import Control.Category.Cartesian
import Shapes.Import

class Category cat => CategoryObject (cat :: k -> k -> *) where
    type CatObject cat :: k -> Type
    catApply :: forall (a :: k) (b :: k). cat a b -> CatObject cat a -> CatObject cat b
    catConst :: forall (a :: k) (b :: k). CatObject cat b -> cat a b

instance CategoryObject (->) where
    type CatObject (->) = Identity
    catApply ab = fmap ab
    catConst (Identity b) _ = b

class (CartesianClosedCategory cat, CategoryObject cat) => CategoryCombinator (cat :: k -> k -> *) where
    fromCombinator :: CatObject cat (CatFunction cat a b) -> cat a b
    toCombinator :: cat a b -> CatObject cat (CatFunction cat a b)

instance CategoryCombinator (->) where
    fromCombinator = runIdentity
    toCombinator = Identity

applyCombinator ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k). CategoryCombinator cat
    => CatObject cat (CatFunction cat a b)
    -> CatObject cat a
    -> CatObject cat b
applyCombinator f = catApply $ fromCombinator @_ @cat f

-- | "I" combinator
-- I :: a -> a
-- I = \x -> x = S K K
combinatorId ::
       forall k (cat :: k -> k -> Type) (a :: k). CategoryCombinator cat
    => CatObject cat (CatFunction cat a a)
combinatorId = toCombinator @k @cat id

-- | "K" combinator
-- K :: a -> b -> a
-- K = \x y -> x
combinatorConst ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k). CategoryCombinator cat
    => CatObject cat (CatFunction cat a (CatFunction cat b a))
combinatorConst = toCombinator @k @cat $ catCurry $ fst' @k @cat @a @b

-- | "S" combinator
-- S :: (a -> b -> c) -> (a -> b) -> a -> c
-- S = \x y z -> x z (y z)
combinatorAp ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k) (c :: k). CategoryCombinator cat
    => CatObject cat (CatFunction cat (CatFunction cat a (CatFunction cat b c)) (CatFunction cat (CatFunction cat a b) (CatFunction cat a c)))
combinatorAp =
    toCombinator @k @cat $
    catCurry $
    catCurry $ let
        x = fst' . fst'
        y = snd' . fst'
        z = snd'
        in catApplyFunction (catApplyFunction @k @cat x z) (catApplyFunction @k @cat y z)

-- | "B" combinator
-- B :: (b -> c) -> (a -> b) -> a -> c
-- B = \x y z -> x (y z) = S (K S) K
combinatorCompose ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k) (c :: k). CategoryCombinator cat
    => CatObject cat (CatFunction cat (CatFunction cat b c) (CatFunction cat (CatFunction cat a b) (CatFunction cat a c)))
combinatorCompose = let
    apc :: forall (a' :: k) (b' :: k). CatObject cat (CatFunction cat a' b') -> CatObject cat a' -> CatObject cat b'
    apc = applyCombinator @k @cat
    s :: forall (a' :: k) (b' :: k) (c' :: k).
         CatObject cat (CatFunction cat (CatFunction cat a' (CatFunction cat b' c')) (CatFunction cat (CatFunction cat a' b') (CatFunction cat a' c')))
    s = combinatorAp @k @cat
    k :: forall (a' :: k) (b' :: k). CatObject cat (CatFunction cat a' (CatFunction cat b' a'))
    k = combinatorConst @k @cat
    in apc (apc s (apc k s)) k

-- | "C" combinator
-- C :: (a -> b -> c) -> b -> a -> c
-- C = \x y z -> x z y = S (B B S) (K K)
combinatorSwapArgs ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k) (c :: k). CategoryCombinator cat
    => CatObject cat (CatFunction cat (CatFunction cat a (CatFunction cat b c)) (CatFunction cat b (CatFunction cat a c)))
combinatorSwapArgs = let
    apc :: forall (a' :: k) (b' :: k). CatObject cat (CatFunction cat a' b') -> CatObject cat a' -> CatObject cat b'
    apc = applyCombinator @k @cat
    s :: forall (a' :: k) (b' :: k) (c' :: k).
         CatObject cat (CatFunction cat (CatFunction cat a' (CatFunction cat b' c')) (CatFunction cat (CatFunction cat a' b') (CatFunction cat a' c')))
    s = combinatorAp @k @cat
    k :: forall (a' :: k) (b' :: k). CatObject cat (CatFunction cat a' (CatFunction cat b' a'))
    k = combinatorConst @k @cat
    b :: forall (a' :: k) (b' :: k) (c' :: k).
         CatObject cat (CatFunction cat (CatFunction cat b' c') (CatFunction cat (CatFunction cat a' b') (CatFunction cat a' c')))
    b = combinatorCompose @k @cat
    in apc (apc s $ apc (apc b b) s) $ apc k k
