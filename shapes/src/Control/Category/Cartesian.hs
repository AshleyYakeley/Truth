module Control.Category.Cartesian where

import Shapes.Import

class Category cat => TerminalCategory (cat :: k -> k -> *) where
    type TerminalObject cat :: k
    terminal :: cat a (TerminalObject cat)

instance TerminalCategory (->) where
    type TerminalObject (->) = ()
    terminal _ = ()

instance TerminalCategory (:-) where
    type TerminalObject (:-) = ()
    terminal = Sub Dict

class Category cat => CategoryProduct (cat :: k -> k -> *) where
    type CatProduct cat :: k -> k -> k
    fst' :: cat (CatProduct cat a b) a
    snd' :: cat (CatProduct cat a b) b
    prod' :: cat a b -> cat a c -> cat a (CatProduct cat b c)

instance CategoryProduct (->) where
    type CatProduct (->) = (,)
    fst' = fst
    snd' = snd
    prod' ab ac a = (ab a, ac a)

class (TerminalCategory cat, CategoryProduct cat) => CartesianClosedCategory (cat :: k -> k -> *) where
    type CatFunction cat :: k -> k -> k
    catApplyPair :: cat (CatProduct cat (CatFunction cat a b) a) b
    catCurry :: cat (CatProduct cat a b) c -> cat a (CatFunction cat b c)
    catUncurry :: cat a (CatFunction cat b c) -> cat (CatProduct cat a b) c

catApplyFunction ::
       forall k (cat :: k -> k -> *) (t :: k) (a :: k) (b :: k). CartesianClosedCategory cat
    => cat t (CatFunction cat a b)
    -> cat t a
    -> cat t b
catApplyFunction f a = catApplyPair . prod' f a

instance CartesianClosedCategory (->) where
    type CatFunction (->) = (->)
    catApplyPair (ab, a) = ab a
    catCurry abc a b = abc (a, b)
    catUncurry abc (a, b) = abc a b

morphismConst :: CartesianClosedCategory cat => cat a (CatFunction cat b a)
morphismConst = catCurry fst'

catToUnitFunc :: CartesianClosedCategory cat => cat t (CatFunction cat (TerminalObject cat) t)
catToUnitFunc = morphismConst

catFromUnitFunc :: CartesianClosedCategory cat => cat (CatFunction cat (TerminalObject cat) t) t
catFromUnitFunc = catApplyFunction id terminal

catToProductFunc ::
       CartesianClosedCategory cat
    => cat (CatFunction cat a (CatFunction cat b c)) (CatFunction cat (CatProduct cat a b) c)
catToProductFunc = catCurry $ catApplyFunction (catApplyFunction fst' (fst' . snd')) (snd' . snd')

catFromProductFunc ::
       CartesianClosedCategory cat
    => cat (CatFunction cat (CatProduct cat a b) c) (CatFunction cat a (CatFunction cat b c))
catFromProductFunc = catCurry $ catCurry $ catApplyFunction (fst' . fst') $ prod' (snd' . fst') snd'
