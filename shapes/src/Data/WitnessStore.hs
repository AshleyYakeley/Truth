module Data.WitnessStore where
{
    import Prelude hiding (null);
    import Data.Kind;
    import Data.Tuple;
    import Data.Type.Equality;
    import Data.Foldable;
    import Data.Functor.Identity;
    import Control.Monad.Trans.State;
    import Control.Monad.Trans.Writer;
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

    replaceWitnessStore :: TestEquality w => WitnessKey w a -> (f a -> f a) -> WitnessStore w f -> WitnessStore w f;
    replaceWitnessStore key mp oldstore = runIdentity $ traverseWitnessStore (\k fa -> return $ case testEquality k key of
    {
        Just Refl -> mp fa;
        Nothing -> fa;
    }) oldstore;

    type IOWitnessStore = WitnessStore IOWitness;
    type IOWitnessKey = WitnessKey IOWitness;

    addIOWitnessStore :: f a -> IOWitnessStore f -> IO (IOWitnessKey a,IOWitnessStore f);
    addIOWitnessStore fa store = do
    {
        iow <- newIOWitness;
        return $ addWitnessStore iow fa store;
    };

    deleteWitnessStoreStateT :: Applicative m => WitnessKey w a -> StateT (WitnessStore w f) m ();
    deleteWitnessStoreStateT key = StateT $ \oldstore -> pure ((),deleteWitnessStore key oldstore);

    traverseWitnessStoreStateT :: (Monoid r,Applicative m) => (forall a. WitnessKey w a -> StateT (f a) m r) -> StateT (WitnessStore w f) m r;
    traverseWitnessStoreStateT ff = StateT $ \oldstore -> fmap swap $ runWriterT $ traverseWitnessStore (\key fa -> WriterT $ fmap swap $ runStateT (ff key) fa) oldstore;

    replaceWitnessStoreStateT :: (TestEquality w,Monoid r,Monad m) => WitnessKey w a -> StateT (f a) m r -> StateT (WitnessStore w f) m r;
    replaceWitnessStoreStateT key call = traverseWitnessStoreStateT $ \k -> case testEquality k key of
    {
        Just Refl -> call;
        Nothing -> pure mempty;
    };

    addIOWitnessStoreStateT :: f a -> StateT (IOWitnessStore f) IO (IOWitnessKey a);
    addIOWitnessStoreStateT fa = StateT $ addIOWitnessStore fa;
}
