module Data.Reity.HasInfo where
{
    import GHC.Types;
    import Data.Type.Heterogeneous;
    import Language.Haskell.TH hiding (Info);
    import Data.OpenWitness;
    import Data.Reity.Info;


    class HasInfo a where
    {
        info :: Info a;
    };

    isInfo :: forall a b. HasInfo a => Info b -> Maybe (HetEq a b);
    isInfo = testHetEquality info;

    mkSimpleInfo :: forall (k :: *) (t :: k). HasInfo k => (IOWitness t,String) -> [TypeKnowledge] -> Info t;
    mkSimpleInfo (wit,name) facts = MkInfo info $ SimpleWit wit name $ mconcat facts;

    ionamedwitness :: TypeQ -> Q Exp;
    ionamedwitness qtp = do
    {
        tp <- qtp;
        let
        {
            tpname = case tp of
            {
                ConT n -> nameBase n;
                VarT n -> nameBase n;
                PromotedT n -> nameBase n;
                _ -> show tp;
            };
        };
        [|($(iowitness (return tp)),tpname)|]
    };
}
