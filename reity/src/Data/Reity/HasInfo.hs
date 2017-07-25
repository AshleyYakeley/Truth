module Data.Reity.HasInfo where
{
    import GHC.Types;
    import Data.Type.Heterogeneous;
    import Language.Haskell.TH hiding (Info);
    import Data.Knowledge;
    import Data.Reity.KnowM;
    import Data.Reity.Info;


    isInfo :: forall a b. HasInfo a => Info b -> Maybe (HetEq a b);
    isInfo = testHetEquality info;

    -- mkSimpleInfo :: forall (k :: *) (t :: k). HasInfo k => (IOWitness t,String) -> [TypeKnowledge] -> Info t;
    -- mkSimpleInfo (wit,name) facts = MkInfo info $ SimpleWit wit name $ mconcat facts;

    generateTypeName :: TypeQ -> Q Exp;
    generateTypeName qtp = do
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
        [|tpname|];
    };

    askInfo :: forall (k :: *) (a :: k). TypeKnowledge -> Info a -> KnowM (TypeFact a);
    askInfo k i = kmContext (show i) $ ask k i;
}
