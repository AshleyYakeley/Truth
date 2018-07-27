module Language.Expression.Witness where

import Shapes

newtype WitnessMap (cat :: k -> k -> Type) (w1 :: k -> Type) (w2 :: k -> Type) =
    MkWitnessMap (forall t1 r. w1 t1 -> (forall t2. w2 t2 -> cat t1 t2 -> r) -> r)

instance Category cat => Category (WitnessMap cat) where
    id = MkWitnessMap $ \wt cont -> cont wt id
    (MkWitnessMap m2) . (MkWitnessMap m1) =
        MkWitnessMap $ \wt0 cont -> m1 wt0 $ \wt1 c1 -> m2 wt1 $ \wt2 c2 -> cont wt2 $ c2 . c1

type WitnessSubstitution k (a :: k -> Type) (b :: k -> Type) = WitnessMap (KindMorphism k Bijection) a b
