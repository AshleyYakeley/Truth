module Control.Constrained.Category where
{
    import Shapes.Import;


    class ConstrainedCategory (cat :: k -> k -> *) where
    {
        type CategoryConstraint cat (t :: k) :: Constraint;
        cid :: CategoryConstraint cat t => cat t t;
        (<.>) :: (CategoryConstraint cat a,CategoryConstraint cat b,CategoryConstraint cat c) =>
            cat b c -> cat a b -> cat a c;
    };

    class ConstrainedCategory cat => ConstrainedCatFunctor (cat :: k -> k -> *) (f :: k -> k) where
    {
        ccfmap :: forall (a :: k) (b :: k). cat a b -> cat (f a) (f b);
    };

    class (ConstrainedCategory cat,CategoryConstraint cat (CInitialObject cat)) => ConstrainedInitialCategory (cat :: k -> k -> *) where
    {
        type CInitialObject cat :: k;
        cinitial :: CategoryConstraint cat t => cat (CInitialObject cat) t;
    };

    class (ConstrainedCategory cat,CategoryConstraint cat (CTerminalObject cat)) => ConstrainedTerminalCategory (cat :: k -> k -> *) where
    {
        type CTerminalObject cat :: k;
        cterminal :: CategoryConstraint cat t => cat t (CTerminalObject cat);
    };

    type ConstrainedZeroCategory cat = (ConstrainedInitialCategory cat,ConstrainedTerminalCategory cat,CInitialObject cat ~ CTerminalObject cat);

    class (ConstrainedCategory cat) => ConstrainedProductCategory (cat :: k -> k -> *) where
    {
        type CProduct cat (a :: k) (b :: k) :: k;
        cfst :: cat (CProduct cat a b) a;
        csnd :: cat (CProduct cat a b) b;
    };

    class (ConstrainedProductCategory cat,ConstrainedTerminalCategory cat) => ConstrainedFiniteCompleteCategory (cat :: k -> k -> *) where
    {
        cpullback :: (CategoryConstraint cat a,CategoryConstraint cat b,CategoryConstraint cat x) =>
            (cat a x,cat b x) -> (cat (CProduct cat a b) a,cat (CProduct cat a b) b);
    };
}
