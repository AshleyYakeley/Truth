module Truth.Core.Edit.Unlift where

import Truth.Core.Import

data CloseUnlift f (a :: k) (b :: k) =
    forall t. MonadTransUnlift t =>
                  MkCloseUnlift (Unlift t)
                                (f t a b)

class UnliftCategory (f :: ((Type -> Type) -> (Type -> Type)) -> k -> k -> Type) where
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

type TransLift t1 t2 = forall m (a :: Type). Monad m => t1 m a -> t2 m a

joinUnlifts ::
       (forall t1 t2.
            (MonadTransUnlift t1, MonadTransUnlift t2) => f1 t1 a1 b1 -> f2 t2 a2 b2 -> f3 (ComposeT t1 t2) a3 b3)
    -> CloseUnlift f1 a1 b1
    -> CloseUnlift f2 a2 b2
    -> CloseUnlift f3 a3 b3
joinUnlifts call (MkCloseUnlift unlift1 open1) (MkCloseUnlift unlift2 open2) =
    MkCloseUnlift (composeUnlift unlift1 unlift2) $ call open1 open2

data PairUnlift f1 f2 (t :: (Type -> Type) -> (Type -> Type)) (a :: k) (b :: k) =
    MkPairUnlift (f1 t a b)
                 (f2 t a b)

class Unliftable (f :: ((Type -> Type) -> (Type -> Type)) -> k -> k -> Type) where
    fmapUnliftable :: forall a b t1 t2. TransLift t1 t2 -> f t1 a b -> f t2 a b

joinUnliftables ::
       (Unliftable f1, Unliftable f2)
    => (forall t. MonadTransUnlift t => f1 t a1 b1 -> f2 t a2 b2 -> f3 t a3 b3)
    -> CloseUnlift f1 a1 b1
    -> CloseUnlift f2 a2 b2
    -> CloseUnlift f3 a3 b3
joinUnliftables call =
    joinUnlifts $ \open1 open2 -> call (fmapUnliftable lift1ComposeT open1) (fmapUnliftable lift2ComposeT' open2)

pairUnlift ::
       (Unliftable f1, Unliftable f2) => CloseUnlift f1 a b -> CloseUnlift f2 a b -> CloseUnlift (PairUnlift f1 f2) a b
pairUnlift = joinUnliftables MkPairUnlift
