module Data.Lens where

import Data.CatFunctor
import Data.Codec
import Data.Injection
import Data.Isomorphism
import Shapes.Import

data Lens' m a b = MkLens
    { lensGet :: a -> b
    , lensPutback :: b -> a -> m a
    }

type Lens = Lens' Maybe

type PureLens = Lens' Identity

instance Invariant (Lens' m a) where
    invmap pq qp (MkLens ap pama) = MkLens (pq . ap) (pama . qp)

instance Monad m => Productable (Lens' m a) where
    rUnit = MkLens (\_ -> ()) (\() -> pure)
    MkLens ap pama <***> MkLens aq qama =
        MkLens (\a -> (ap a, aq a)) $ \(p, q) a -> do
            a' <- pama p a
            qama q a'

lensNone :: Lens' m Void b
lensNone = MkLens never $ \_ -> never

lensSum :: Functor m => Lens' m p b -> Lens' m q b -> Lens' m (Either p q) b
lensSum (MkLens pb bpmp) (MkLens qb bqmq) =
    MkLens (either pb qb) $ \b pq ->
        case pq of
            Left p -> fmap Left $ bpmp b p
            Right q -> fmap Right $ bqmq b q

lensModify :: Lens' m a b -> (b -> b) -> a -> m a
lensModify lens bb a = lensPutback lens (bb (lensGet lens a)) a

pureLensModify :: PureLens a b -> (b -> b) -> a -> a
pureLensModify lens bb a = runIdentity $ lensModify lens bb a

lensMap :: MonadInner m => Lens' m a b -> (b -> b) -> (a -> a)
lensMap lens bb a =
    case mToMaybe (lensModify lens bb a) of
        Just a' -> a'
        _ -> a

lensAllowed :: MonadInner m => Lens' m a b -> b -> a -> Bool
lensAllowed lens b a = isJust $ mToMaybe $ lensPutback lens b a

lensToPure :: MonadInner m => Lens' m a b -> PureLens a b
lensToPure (MkLens g pb) = MkLens g $ \b olda -> Identity $ fromMaybe olda $ mToMaybe $ pb b olda

instance IsBiMap Lens' where
    mapBiMapM ff lens =
        MkLens
            { lensGet = lensGet lens
            , lensPutback =
                \b -> do
                    ma <- lensPutback lens b
                    return (ff ma)
            }

instance Monad m => Category (Lens' m) where
    id = MkLens{lensGet = id, lensPutback = \b _ -> pure b}
    bc . ab =
        MkLens
            { lensGet = (lensGet bc) . (lensGet ab)
            , lensPutback =
                \c a -> do
                    b <- lensPutback bc c (lensGet ab a)
                    lensPutback ab b a
            }

instance (Traversable f, Applicative f, Applicative m) => CatFunctor (Lens' m) (Lens' m) f where
    cfmap lens =
        MkLens{lensGet = fmap (lensGet lens), lensPutback = \fb fa -> sequenceA (liftA2 (lensPutback lens) fb fa)}

fstLens :: PureLens (a, b) a
fstLens = let
    lensGet = fst
    lensPutback a (_, b) = Identity (a, b)
    in MkLens{..}

sndLens :: PureLens (a, b) b
sndLens = let
    lensGet = snd
    lensPutback b (a, _) = Identity (a, b)
    in MkLens{..}

pickLens :: Eq p => p -> PureLens (p -> a) a
pickLens p =
    MkLens
        { lensGet = \pa -> pa p
        , lensPutback =
            \a pa ->
                Identity
                    ( \p' ->
                        if p == p'
                            then a
                            else pa p'
                    )
        }

defaultLens :: a -> PureLens (Maybe a) a
defaultLens def = MkLens (fromMaybe def) $ \a _ -> Identity $ Just a

bijectionLens :: Bijection a b -> PureLens a b
bijectionLens (MkIsomorphism ab ba) = MkLens ab (\b _ -> return (ba b))

injectionLens :: Injection' m a b -> Lens' m a b
injectionLens lens = MkLens{lensGet = injForwards lens, lensPutback = \b -> pure (injBackwards lens b)}

hashMapLens :: Hashable key => key -> PureLens (HashMap key value) (Maybe value)
hashMapLens key = let
    lensGet = lookup key
    lensPutback Nothing hm = Identity $ deleteMap key hm
    lensPutback (Just value) hm = Identity $ insertMap key value hm
    in MkLens{..}

lensStateT :: Functor m => PureLens a b -> StateT b m r -> StateT a m r
lensStateT (MkLens g p) (StateT bmrb) =
    StateT $ \olda -> let
        mrb = bmrb $ g olda
        in fmap (\(r, b) -> (r, runIdentity $ p b olda)) mrb

lensToVL :: forall a b. PureLens a b -> forall f. Functor f => (b -> f b) -> (a -> f a)
lensToVL MkLens{..} bfb a = fmap (\b -> runIdentity $ lensPutback b a) $ bfb $ lensGet a

vlToLens ::
    forall m a b.
    Applicative m =>
    (forall f. Functor f => (b -> f b) -> (a -> f a)) ->
    Lens' m a b
vlToLens f = let
    lensGet a = getConst $ f Const a
    lensPutback b a = f (\_ -> pure b) a
    in MkLens{..}
