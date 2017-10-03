module Data.KindCategory where
{
    import Shapes.Import;
    import Data.Category;


    data ConstraintWitness :: Constraint -> * where
    {
        MkConstraintWitness :: forall (c :: Constraint). c => ConstraintWitness c;
    };

    class Category (KindMorphism k) => KindCategory k where
    {
        type KindMorphism k :: k -> k -> *;
    };

    instance KindCategory Type where
    {
        type KindMorphism Type = (->);
    };

    newtype ConstraintMorphism (a :: Constraint) (b :: Constraint) = MkConstraintMorphism (a => ConstraintWitness b);

    instance Category ConstraintMorphism where
    {
        id = MkConstraintMorphism MkConstraintWitness;
        (MkConstraintMorphism bc) . (MkConstraintMorphism ab) = MkConstraintMorphism (case ab of
        {
            MkConstraintWitness -> case bc of
            {
                MkConstraintWitness -> MkConstraintWitness;
            };
        });
    };

    instance TerminalCategory ConstraintMorphism where
    {
        type Terminal ConstraintMorphism = (() :: Constraint);
        terminal = MkConstraintMorphism MkConstraintWitness;
    };

    instance KindCategory Constraint where
    {
        type KindMorphism Constraint = ConstraintMorphism;
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
        kfst = MkConstraintMorphism MkConstraintWitness;
        ksnd = MkConstraintMorphism MkConstraintWitness;
    };
}
