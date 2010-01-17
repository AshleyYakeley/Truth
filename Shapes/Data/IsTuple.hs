module Data.IsTuple where
{
    import Data.Witness;

    class (Is (ListType Type) (ListTuple a)) => IsTuple a where
    {
        type ListTuple a;
        fromListTuple :: ListTuple a -> a;
        toListTuple :: a -> ListTuple a;
    };

    instance IsTuple (a,b) where
    {
        type ListTuple (a,b) = (a,(b,()));
        fromListTuple (a,(b,())) = (a,b);
        toListTuple (a,b) = (a,(b,()));
    };

    instance IsTuple (a,b,c) where
    {
        type ListTuple (a,b,c) = (a,(b,(c,())));
        fromListTuple (a,(b,(c,()))) = (a,b,c);
        toListTuple (a,b,c) = (a,(b,(c,())));
    };
}
