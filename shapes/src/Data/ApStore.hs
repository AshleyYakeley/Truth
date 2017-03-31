module Data.ApStore where
{
    import Control.Applicative.Free;
    import Data.Witness;


    type ApStore w = WitnessFDict w (FreeApplicative w);

    findInApStore :: (TestEquality w) => ApStore w -> w a -> Maybe a;
    findInApStore store wa = do
    {
        fa <- witnessFDictLookup wa store;
        findInApStoreApplicative store fa;
    };

    findInApStoreApplicative :: (TestEquality w) => ApStore w -> FreeApplicative w a -> Maybe a;
    findInApStoreApplicative _store (FreePure r) = return r;
    findInApStoreApplicative store (FreeApply wt ftr) = do
    {
        t <- findInApStore store wt;
        tr <- findInApStoreApplicative store ftr;
        return (tr t);
    };
}
