module Data.WitnessStore where
{
    import Prelude hiding (null);
    import Data.Kind;
    import Data.Type.Equality;
    import Data.Foldable;
    import Data.Witness;
    import Data.OpenWitness;
    import Data.Store;


    data WitnessKey (w :: k -> *) (a :: k) = MkWitnessKey Key (w a);

    instance TestEquality w => TestEquality (WitnessKey w) where
    {
        testEquality (MkWitnessKey k1 w1) (MkWitnessKey k2 w2) | k1 == k2 = do
        {
            Refl <- testEquality w1 w2;
            return Refl;
        };
        testEquality _ _ = Nothing;
    };

    newtype WitnessStore (w :: k -> *) (f :: k -> *) = MkWitnessStore (Store (AnyF w f));

    emptyWitnessStore :: WitnessStore w f;
    emptyWitnessStore = MkWitnessStore emptyStore;

    isEmptyWitnessStore :: WitnessStore w f -> Bool;
    isEmptyWitnessStore (MkWitnessStore store) = isEmptyStore store;

    addWitnessStore :: w a -> f a -> WitnessStore w f -> (WitnessKey w a,WitnessStore w f);
    addWitnessStore wa fa (MkWitnessStore oldstore) = let
    {
        (key,newstore) = addStore (MkAnyF wa fa) oldstore;
    }
    in (MkWitnessKey key wa,MkWitnessStore newstore);

    deleteWitnessStore :: WitnessKey w a -> WitnessStore w f -> WitnessStore w f;
    deleteWitnessStore (MkWitnessKey key _) (MkWitnessStore store) = MkWitnessStore $ deleteStore key store;

    allWitnessStore :: WitnessStore w f -> [AnyF w f];
    allWitnessStore (MkWitnessStore store) = toList store;

    lookupWitnessStore :: TestEquality w => WitnessKey w a -> WitnessStore w f -> Maybe (f a);
    lookupWitnessStore (MkWitnessKey key iowK) (MkWitnessStore store) = do
    {
        MkAnyF iowF fa <- lookupStore key store;
        Refl <- testEquality iowK iowF;
        return fa;
    };

    traverseWitnessStore :: Applicative m => (forall a. WitnessKey w a -> f1 a -> m (f2 a)) -> WitnessStore w f1 -> m (WitnessStore w f2);
    traverseWitnessStore ff (MkWitnessStore store) = let
    {
        kamb key (MkAnyF wa fa) = MkAnyF wa <$> ff (MkWitnessKey key wa) fa;
    } in MkWitnessStore <$> traverseStore kamb store;

    type IOWitnessStore = WitnessStore IOWitness;
    type IOWitnessKey = WitnessKey IOWitness;

    addIOWitnessStore :: f a -> IOWitnessStore f -> IO (IOWitnessKey a,IOWitnessStore f);
    addIOWitnessStore fa store = do
    {
        iow <- newIOWitness;
        return $ addWitnessStore iow fa store;
    };
}
