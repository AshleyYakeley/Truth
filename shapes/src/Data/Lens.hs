module Data.Lens where

import Data.CatFunctor
import Data.Codec
import Data.Injection
import Data.IsoVariant
import Data.Isomorphism
import Data.MonadOne
import Shapes.Import

data Lens' m a b = MkLens
    { lensGet :: a -> b
    , lensPutback :: b -> a -> m a
    }

instance IsoVariant (Lens' m a) where
    isoMap pq qp (MkLens ap pama) = MkLens (pq . ap) (pama . qp)

instance Monad m => Productish (Lens' m a) where
    pUnit = MkLens (\_ -> ()) (\() -> pure)
    MkLens ap pama <***> MkLens aq qama =
        MkLens (\a -> (ap a, aq a)) $ \(p, q) a -> do
            a' <- pama p a
            qama q a'

lensNone :: Lens' m None b
lensNone = MkLens never $ \_ -> never

lensSum :: Functor m => Lens' m p b -> Lens' m q b -> Lens' m (Either p q) b
lensSum (MkLens pb bpmp) (MkLens qb bqmq) =
    MkLens (either pb qb) $ \b pq ->
        case pq of
            Left p -> fmap Left $ bpmp b p
            Right q -> fmap Right $ bqmq b q

lensModify :: Lens' m a b -> (b -> b) -> a -> m a
lensModify lens bb a = lensPutback lens (bb (lensGet lens a)) a

lensMap :: (MonadOne m) => Lens' m a b -> (b -> b) -> (a -> a)
lensMap lens bb a =
    case getMaybeOne (lensModify lens bb a) of
        Just a' -> a'
        _ -> a

lensAllowed :: (MonadOne m) => Lens' m a b -> b -> a -> Bool
lensAllowed lens b a = isJust $ getMaybeOne $ lensPutback lens b a

type Lens = Lens' Maybe

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
    id = MkLens {lensGet = id, lensPutback = \b _ -> pure b}
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
        MkLens {lensGet = fmap (lensGet lens), lensPutback = \fb fa -> sequenceA (liftA2 (lensPutback lens) fb fa)}

fstLens :: Lens' Identity (a, b) a
fstLens = let
    lensGet = fst
    lensPutback a (_, b) = Identity (a, b)
    in MkLens {..}

sndLens :: Lens' Identity (a, b) b
sndLens = let
    lensGet = snd
    lensPutback b (a, _) = Identity (a, b)
    in MkLens {..}

pickLens :: (Eq p) => p -> Lens' Identity (p -> a) a
pickLens p =
    MkLens
        { lensGet = \pa -> pa p
        , lensPutback =
              \a pa ->
                  Identity
                      (\p' ->
                           if p == p'
                               then a
                               else pa p')
        }

defaultLens :: a -> Lens' Identity (Maybe a) a
defaultLens def = MkLens (fromMaybe def) $ \a _ -> Identity $ Just a

bijectionLens :: Bijection a b -> Lens' Identity a b
bijectionLens (MkIsomorphism ab ba) = MkLens ab (\b _ -> return (ba b))

injectionLens :: Injection' m a b -> Lens' m a b
injectionLens lens = MkLens {lensGet = injForwards lens, lensPutback = \b -> pure (injBackwards lens b)}

hashMapLens :: (Eq key, Hashable key) => key -> Lens' Identity (HashMap key value) (Maybe value)
hashMapLens key = let
    lensGet = lookup key
    lensPutback Nothing hm = Identity $ deleteMap key hm
    lensPutback (Just value) hm = Identity $ insertMap key value hm
    in MkLens {..}

lensStateT :: Functor m => Lens' Identity a b -> StateT b m r -> StateT a m r
lensStateT (MkLens g p) (StateT bmrb) =
    StateT $ \olda -> let
        mrb = bmrb $ g olda
        in fmap (\(r, b) -> (r, runIdentity $ p b olda)) mrb
