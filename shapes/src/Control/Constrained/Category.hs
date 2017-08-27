module Control.Constrained.Category where
{
    import Shapes.Import;

    class ConstrainedCategory (cat :: k -> k -> *) where
    {
        type CategoryConstraint cat (t :: k) :: Constraint;
        cid :: CategoryConstraint cat t => cat t t;
        (<.>) :: (CategoryConstraint cat a,CategoryConstraint cat b,CategoryConstraint cat c) => cat b c -> cat a b -> cat a c;
    };

    class ConstrainedCategory cat => ConstrainedCatFunctor (cat :: k -> k -> *) (f :: k -> k) where
    {
        ccfmap :: forall (a :: k) (b :: k). cat a b -> cat (f a) (f b);
    };
}
