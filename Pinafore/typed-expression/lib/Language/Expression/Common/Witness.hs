module Language.Expression.Common.Witness where

import Shapes

data ListCat (cat :: k -> k -> Type) (a :: [k]) (b :: [k]) where
    NilListCat :: ListCat cat '[] '[]
    ConsListCat :: cat a b -> ListCat cat aa bb -> ListCat cat (a ': aa) (b ': bb)

listCatProduct :: ListCat (->) aa bb -> ListProduct aa -> ListProduct bb
listCatProduct NilListCat _ = ()
listCatProduct (ConsListCat m mm) ~(a, aa) = (m a, listCatProduct mm aa)

newtype WitnessConvert (cat :: k -> k -> Type) (w1 :: k -> Type) (w2 :: k -> Type) = MkWitnessConvert
    { unWitnessConvert :: forall t1 r. w1 t1 -> (forall t2. w2 t2 -> cat t1 t2 -> r) -> r
    }

instance Category cat => Category (WitnessConvert cat) where
    id = MkWitnessConvert $ \wt cont -> cont wt id
    (MkWitnessConvert m2) . (MkWitnessConvert m1) =
        MkWitnessConvert $ \wt0 cont -> m1 wt0 $ \wt1 c1 -> m2 wt1 $ \wt2 c2 -> cont wt2 $ c2 . c1

hoistWitnessConvert :: (forall a b. cat1 a b -> cat2 a b) -> WitnessConvert cat1 w1 w2 -> WitnessConvert cat2 w1 w2
hoistWitnessConvert mm (MkWitnessConvert f) = MkWitnessConvert $ \p call -> f p $ \q m -> call q $ mm m

listWitnessConvert ::
       forall k cat (w1 :: k -> Type) (w2 :: k -> Type).
       WitnessConvert cat w1 w2
    -> WitnessConvert (ListCat cat) (ListType w1) (ListType w2)
listWitnessConvert (MkWitnessConvert f) =
    MkWitnessConvert $ let
        go :: forall t1 r. ListType w1 t1 -> (forall t2. ListType w2 t2 -> ListCat cat t1 t2 -> r) -> r
        go NilListType call = call NilListType NilListCat
        go (ConsListType a aa) call = f a $ \b m -> go aa $ \bb mm -> call (ConsListType b bb) (ConsListCat m mm)
        in go

type WitnessSubstitution k (a :: k -> Type) (b :: k -> Type) = WitnessConvert KindBijection a b
