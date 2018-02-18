module Truth.Core.Edit.Unlift where

import Truth.Core.Import

data CloseUnlift f (a :: k) (b :: k) =
    forall t. MonadTransUnlift t =>
              MkCloseUnlift (Unlift t)
                            (f t a b)

class UnliftCategory (f :: ((* -> *) -> (* -> *)) -> k -> k -> *) where
    ucId :: forall a. f IdentityT a a
    ucCompose ::
           forall tab tbc a b c. (MonadTransUnlift tab, MonadTransUnlift tbc)
        => f tbc b c
        -> f tab a b
        -> f (ComposeT tbc tab) a c

instance UnliftCategory f => Category (CloseUnlift f) where
    id = MkCloseUnlift identityUnlift ucId
    (MkCloseUnlift unliftBC fBC) . (MkCloseUnlift unliftAB fAB) =
        MkCloseUnlift (composeUnlift unliftBC unliftAB) (ucCompose fBC fAB)

data PairUnlift f1 f2 (t :: (* -> *) -> (* -> *)) (a :: k) (b :: k) =
    MkPairUnlift (f1 t a b)
                 (f2 t a b)

class Unliftable (f :: ((* -> *) -> (* -> *)) -> k -> k -> *) where
    fmapUnliftable ::
           forall a b t1 t2.
           (forall m x. Monad m =>
                            t1 m x -> t2 m x)
        -> f t1 a b
        -> f t2 a b

joinUnlifts ::
       (Unliftable f1, Unliftable f2)
    => (forall t. MonadTransUnlift t =>
                      Unlift t -> f1 t a1 b1 -> f2 t a2 b2 -> r)
    -> CloseUnlift f1 a1 b1
    -> CloseUnlift f2 a2 b2
    -> r
joinUnlifts call (MkCloseUnlift unlift1 open1) (MkCloseUnlift unlift2 open2) =
    call (composeUnlift unlift1 unlift2) (fmapUnliftable lift1ComposeT open1) (fmapUnliftable lift2ComposeT' open2)

pairUnlift ::
       (Unliftable f1, Unliftable f2) => CloseUnlift f1 a b -> CloseUnlift f2 a b -> CloseUnlift (PairUnlift f1 f2) a b
pairUnlift = joinUnlifts $ \unlift f1' f2' -> MkCloseUnlift unlift $ MkPairUnlift f1' f2'
