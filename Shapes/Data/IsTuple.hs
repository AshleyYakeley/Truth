module Data.IsTuple where
{
    import Data.Bijection;
    import Data.Witness;

    class (Is (ListType Type) (AggregateTuple a)) => Aggregate a where
    {
        type AggregateTuple a;
        fromAggregateTuple :: AggregateTuple a -> a;
        toAggregateTuple :: a -> AggregateTuple a;
    };

    instance Aggregate (a,b) where
    {
        type AggregateTuple (a,b) = (a,(b,()));
        fromAggregateTuple (a,(b,())) = (a,b);
        toAggregateTuple (a,b) = (a,(b,()));
    };

    instance Aggregate (a,b,c) where
    {
        type AggregateTuple (a,b,c) = (a,(b,(c,())));
        fromAggregateTuple (a,(b,(c,()))) = (a,b,c);
        toAggregateTuple (a,b,c) = (a,(b,(c,())));
    };

    aggregateTupleBijection :: (Aggregate t) => Bijection t (AggregateTuple t);
    aggregateTupleBijection = MkBijection toAggregateTuple fromAggregateTuple;
}
