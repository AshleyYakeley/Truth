module Data.Reity.HasInfo where
{
    import GHC.Types;
    import Data.Type.Heterogeneous;
    import Data.OpenWitness;
    import Data.Reity.Info;


    class HasInfo a where
    {
        info :: Info a;
    };

    isInfo :: forall a b. HasInfo a => Info b -> Maybe (HetEq a b);
    isInfo = testHetEquality info;

    mkSimpleInfo :: forall (k :: *) (t :: k). HasInfo k => IOWitness t -> [TypeKnowledge] -> Info t;
    mkSimpleInfo wit facts = MkInfo info $ SimpleWit wit $ mconcat facts;
}
