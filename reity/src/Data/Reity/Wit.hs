module Data.Reity.Wit where
{
    import Data.Kind;
    import Data.Type.Equality;
    import Data.Type.Heterogeneous;
    import Language.Haskell.TH;
    import Data.OpenWitness;


    data Wit (t :: k) where
    {
        SimpleWit :: forall (k :: *) (t :: k). IOWitness t -> Wit t;
        ConsWit :: forall (ka :: *) (kb :: *) (f :: ka -> kb) (a :: ka). Wit f -> Wit a -> Wit (f a);
    };

    instance TestHetEquality Wit where
    {
        testHetEquality (SimpleWit iowa) (SimpleWit iowb) = testHetEquality iowa iowb;
        testHetEquality (ConsWit ica iaa) (ConsWit icb iab) = do
        {
            ReflH <- testHetEquality ica icb;
            ReflH <- testHetEquality iaa iab;
            return ReflH;
        };
        testHetEquality _ _ = Nothing;
    };

    instance TestEquality Wit where
    {
        testEquality wa wb = do
        {
            ReflH <- testHetEquality wa wb;
            return Refl;
        }
    };

    generateWitness :: TypeQ -> Q Exp;
    generateWitness qtp = [e|SimpleWit $(iowitness qtp)|];
}
