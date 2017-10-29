module Data.KindCategory where
{
    import Shapes.Import;


    class Category (KindMorphism k) => KindCategory k where
    {
        type KindMorphism k :: k -> k -> *;
    };

    instance KindCategory Type where
    {
        type KindMorphism Type = (->);
    };

    instance KindCategory Constraint where
    {
        type KindMorphism Constraint = (:-);
    };

    newtype NestedMorphism (a :: p -> q) (b :: p -> q) = MkNestedMorphism
        (forall (r :: p). KindMorphism q (a r) (b r));

    instance Category (KindMorphism q) => Category (NestedMorphism :: (p -> q) -> (p -> q) -> *) where
    {
        id = MkNestedMorphism id;
        (MkNestedMorphism bc) . (MkNestedMorphism ab) = MkNestedMorphism (bc . ab);
    };

    instance KindCategory kq => KindCategory (kp -> kq) where
    {
        type KindMorphism (kp -> kq) = (NestedMorphism :: (kp -> kq) -> (kp -> kq) -> *);
    };

    class KindCategory k => KindProductCategory k where
    {
        type KindMorphismProduct k (a :: k) (b :: k) :: k;
        kfst :: KindMorphism k (KindMorphismProduct k a b) a;
        ksnd :: KindMorphism k (KindMorphismProduct k a b) b;
    };

    instance KindProductCategory Type where
    {
        type KindMorphismProduct Type a b = (a,b);
        kfst = fst;
        ksnd = snd;
    };

    instance KindProductCategory Constraint where
    {
        type KindMorphismProduct Constraint a b = (a,b);
        kfst = Sub Dict;
        ksnd = Sub Dict;
    };
}
